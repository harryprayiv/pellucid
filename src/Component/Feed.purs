-- | A cursor-paginated timeline (home or public). Owns per-card UI state
-- | (revealed sensitive media, in-flight like/bookmark) keyed by status id.
module Component.Feed
  ( Kind(..)
  , component
  ) where

import Prelude

import App.Backend (AppError, Backend, Cursor, printAppError)
import App.Monad (Env)
import App.Route (Route(..), navigate)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (Page, followPage, homeTimeline, publicTimeline)
import Pixelfed.Entity (Status, StatusId, displayed, statusId)
import Pixelfed.StatusOps (Toggle)
import Pixelfed.StatusOps as Ops
import View.Common (cls, iconButton)
import View.StatusCard (CardAction(..), statusCard)

data Kind = HomeFeed | PublicFeed

derive instance Eq Kind

type State =
  { kind :: Kind
  , feed :: FeedState
  , now :: Maybe Instant
  , revealed :: Set StatusId
  , busy :: Set StatusId
  , notice :: Maybe String
  }

data FeedState
  = Loading
  | Failed AppError
  | Loaded { statuses :: Array Status, next :: Maybe Cursor, more :: MoreState }

data MoreState = Idle | LoadingMore | MoreFailed AppError | Exhausted

data Action
  = Initialize
  | Receive Kind
  | Refresh
  | LoadMore
  | Card StatusId CardAction
  | DismissNotice

component :: forall q o m. MonadAff m => MonadAsk Env m => H.Component q Kind o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

initialState :: Kind -> State
initialState kind =
  { kind, feed: Loading, now: Nothing, revealed: Set.empty, busy: Set.empty, notice: Nothing }

title :: Kind -> String
title = case _ of
  HomeFeed -> "Home"
  PublicFeed -> "Public"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "feed" ]
    [ HH.header [ cls "feed-header" ]
        [ HH.h1_ [ HH.text (title state.kind) ]
        , iconButton "ghost" "Refresh" "\x21BB" Refresh
        ]
    , case state.notice of
        Nothing -> HH.text ""
        Just text ->
          HH.div [ cls "notice" ]
            [ HH.span_ [ HH.text text ]
            , iconButton "ghost" "Dismiss" "\x2715" DismissNotice
            ]
    , case state.feed of
        Loading -> HH.p [ cls "status-line" ] [ HH.text "Loading\x2026" ]
        Failed err ->
          HH.div [ cls "error-box" ]
            [ HH.p_ [ HH.text (printAppError err) ]
            , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> Refresh ] [ HH.text "Try again" ]
            ]
        Loaded page ->
          HH.div_
            [ if Array.null page.statuses then HH.p [ cls "status-line" ] [ HH.text "Nothing here yet." ]
              else HH.div [ cls "cards" ] (map (card state) page.statuses)
            , footer page.next page.more
            ]
    ]

card :: forall m. State -> Status -> H.ComponentHTML Action () m
card state status =
  let
    id = statusId (displayed status)
  in
    statusCard
      { now: state.now
      , revealed: Set.member id state.revealed
      , busy: Set.member id state.busy
      , commentsOpenDetail: true
      }
      (Card id)
      status

footer :: forall m. Maybe Cursor -> MoreState -> H.ComponentHTML Action () m
footer next more = HH.div [ cls "feed-footer" ] case next, more of
  _, LoadingMore -> [ HH.p [ cls "status-line" ] [ HH.text "Loading more\x2026" ] ]
  _, MoreFailed err ->
    [ HH.p_ [ HH.text (printAppError err) ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadMore ] [ HH.text "Try again" ]
    ]
  Nothing, _ -> [ HH.p [ cls "status-line" ] [ HH.text "You're all caught up." ] ]
  _, Exhausted -> [ HH.p [ cls "status-line" ] [ HH.text "You're all caught up." ] ]
  Just _, Idle -> [ HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadMore ] [ HH.text "Load more" ] ]

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> load

  Receive kind -> do
    current <- H.gets _.kind
    when (kind /= current) do
      H.modify_ \s -> initialState kind # _ { now = s.now }
      load

  Refresh -> do
    H.modify_ _ { feed = Loading, notice = Nothing }
    load

  LoadMore -> do
    state <- H.get
    case state.feed of
      Loaded page@{ next: Just cursor, more: Idle } -> loadMore page cursor
      Loaded page@{ next: Just cursor, more: MoreFailed _ } -> loadMore page cursor
      _ -> pure unit

  DismissNotice -> H.modify_ _ { notice = Nothing }

  Card id action -> case action of
    Reveal -> H.modify_ \s -> s { revealed = Set.insert id s.revealed }
    OpenAccount accountId -> liftEffect (navigate (Profile accountId))
    OpenStatus sid -> liftEffect (navigate (StatusDetail sid))
    ToggleFavourite -> toggle id Ops.ToggleFavourite
    ToggleBookmark -> toggle id Ops.ToggleBookmark

load :: forall o m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () o m Unit
load = do
  backend <- asks _.backend
  kind <- H.gets _.kind
  stamp <- liftEffect now
  result <- liftAff (fetchFirst kind backend)
  H.modify_ \s -> s
    { now = Just stamp
    , feed = case result of
        Left err -> Failed err
        Right page -> Loaded { statuses: page.items, next: page.next, more: Idle }
    }

fetchFirst :: Kind -> Backend -> Aff (Either AppError (Page Status))
fetchFirst = case _ of
  HomeFeed -> homeTimeline
  PublicFeed -> publicTimeline

loadMore
  :: forall o m
   . MonadAff m
  => MonadAsk Env m
  => { statuses :: Array Status, next :: Maybe Cursor, more :: MoreState }
  -> Cursor
  -> H.HalogenM State Action () o m Unit
loadMore page cursor = do
  backend <- asks _.backend
  H.modify_ _ { feed = Loaded page { more = LoadingMore } }
  result <- liftAff (followPage backend cursor)
  stamp <- liftEffect now
  H.modify_ \s -> s
    { now = Just stamp
    , feed = case result of
        Left err -> Loaded page { more = MoreFailed err }
        Right next ->
          let
            known = Set.fromFoldable (map statusId page.statuses)
            fresh = Array.filter (\st -> not (Set.member (statusId st) known)) next.items
          in
            Loaded
              { statuses: page.statuses <> fresh
              , next: next.next
              , more: if Array.null fresh || next.next == Nothing then Exhausted else Idle
              }
    }

-- | Flips a like or bookmark on the displayed status with the given id,
-- | marking it busy meanwhile and merging the server's answer back.
toggle :: forall o m. MonadAff m => MonadAsk Env m => StatusId -> Toggle -> H.HalogenM State Action () o m Unit
toggle id which = do
  state <- H.get
  case state.feed of
    Loaded page | not (Set.member id state.busy) ->
      case Array.find (\st -> statusId (displayed st) == id) page.statuses of
        Nothing -> pure unit
        Just target -> do
          backend <- asks _.backend
          H.modify_ \s -> s { busy = Set.insert id s.busy }
          result <- liftAff (Ops.request which (displayed target) backend id)
          H.modify_ \s -> s
            { busy = Set.delete id s.busy
            , notice = case result of
                Left err -> Just (printAppError err)
                Right _ -> s.notice
            , feed = case result, s.feed of
                Right fromServer, Loaded p ->
                  Loaded p { statuses = Ops.replaceAll id (Ops.merge which (displayed target) fromServer) p.statuses }
                _, feed -> feed
            }
    _ -> pure unit