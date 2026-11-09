-- | One post with its comments and a reply box. Comments are the context
-- | endpoint's descendants, shown flat in the order the server gives them.
module Component.StatusDetail
  ( Input
  , component
  ) where

import Prelude

import App.Backend (AppError, printAppError)
import App.Monad (Env)
import App.Route (Route(..), goBack, navigate)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (deleteStatus, getContext, getStatus, postReply)
import Pixelfed.Entity (AccountId, Status(..), StatusId, captionOf, displayed, statusId)
import Pixelfed.StatusOps (Toggle)
import Pixelfed.StatusOps as Ops
import View.Common (agoLabel, avatar, cls, handleName, iconButton, screenHeader)
import View.StatusCard (CardAction(..), statusCard)
import Web.Event.Event (Event, preventDefault)

type Input = { id :: StatusId, me :: Maybe AccountId }

data Loadable a = Pending | Missing AppError | Ready a

type Reply = { text :: String, sending :: Boolean, error :: Maybe String }

type State =
  { input :: Input
  , main :: Loadable Status
  , comments :: Loadable (Array Status)
  , now :: Maybe Instant
  , revealed :: Set StatusId
  , busy :: Set StatusId
  , reply :: Reply
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | Reload
  | Back
  | Card StatusId CardAction
  | SetReply String
  | SubmitReply Event
  | DeleteComment StatusId
  | DismissNotice

component :: forall q o m. MonadAff m => MonadAsk Env m => H.Component q Input o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState input =
  { input
  , main: Pending
  , comments: Pending
  , now: Nothing
  , revealed: Set.empty
  , busy: Set.empty
  , reply: { text: "", sending: false, error: Nothing }
  , notice: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "detail" ]
    [ screenHeader Back "Post" [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , case state.notice of
        Nothing -> HH.text ""
        Just text ->
          HH.div [ cls "notice" ]
            [ HH.span_ [ HH.text text ], iconButton "ghost" "Dismiss" "\x2715" DismissNotice ]
    , case state.main of
        Pending -> HH.p [ cls "status-line" ] [ HH.text "Loading\x2026" ]
        Missing err ->
          HH.div [ cls "error-box" ]
            [ HH.p_ [ HH.text (printAppError err) ]
            , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> Reload ] [ HH.text "Try again" ]
            ]
        Ready status ->
          let
            id = statusId (displayed status)
            Status shown = displayed status
          in
            HH.div_
              [ statusCard
                  { now: state.now
                  , revealed: Set.member id state.revealed
                  , busy: Set.member id state.busy
                  , commentsOpenDetail: false
                  }
                  (Card id)
                  status
              , HH.section [ cls "comments" ]
                  [ HH.h2_ [ HH.text "Comments" ]
                  , case state.comments of
                      Pending -> HH.p [ cls "status-line" ] [ HH.text "Loading comments\x2026" ]
                      Missing err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
                      Ready [] -> HH.p [ cls "status-line" ] [ HH.text "No comments yet." ]
                      Ready comments -> HH.div [ cls "comment-list" ] (map (commentRow state) comments)
                  , if shown.commentsDisabled then HH.p [ cls "muted" ] [ HH.text "Comments are off for this post." ]
                    else replyForm state.reply
                  ]
              ]
    ]

commentRow :: forall m. State -> Status -> H.ComponentHTML Action () m
commentRow state comment =
  let
    Status c = comment
    mine = state.input.me == Just c.account.id
  in
    HH.article [ cls "comment" ]
      [ HH.button
          [ cls "avatar-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> Card c.id (OpenAccount c.account.id) ]
          [ avatar "avatar avatar-small" c.account.avatar ]
      , HH.div [ cls "comment-body" ]
          [ HH.div [ cls "comment-head" ]
              [ HH.strong_ [ HH.text (handleName c.account.displayName c.account.acct) ]
              , HH.span [ cls "card-time", HP.title c.createdAt ] [ HH.text (agoLabel state.now c.createdAt) ]
              ]
          , HH.p [ cls "comment-text" ] [ HH.text (captionOf c) ]
          , HH.div [ cls "comment-actions" ]
              [ HH.button
                  [ cls ("action action-small" <> if c.favourited then " action-active" else "")
                  , HP.type_ HP.ButtonButton
                  , HP.disabled (Set.member c.id state.busy)
                  , HP.title (if c.favourited then "Unlike" else "Like")
                  , HE.onClick \_ -> Card c.id ToggleFavourite
                  ]
                  [ HH.text (if c.favourited then "\x2665" else "\x2661")
                  , if c.favouritesCount > 0 then HH.span [ cls "count" ] [ HH.text (" " <> show c.favouritesCount) ]
                    else HH.text ""
                  ]
              , if mine then
                  HH.button
                    [ cls "action action-small"
                    , HP.type_ HP.ButtonButton
                    , HP.disabled (Set.member c.id state.busy)
                    , HP.title "Delete comment"
                    , HE.onClick \_ -> DeleteComment c.id
                    ]
                    [ HH.text "\x1F5D1" ]
                else HH.text ""
              ]
          ]
      ]

replyForm :: forall m. Reply -> H.ComponentHTML Action () m
replyForm reply =
  HH.form [ cls "reply-form", HE.onSubmit SubmitReply ]
    [ HH.textarea
        [ HP.value reply.text
        , HP.placeholder "Add a comment"
        , HP.rows 2
        , HP.disabled reply.sending
        , HE.onValueInput SetReply
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit, HP.disabled (reply.sending || trim reply.text == "") ]
        [ HH.text (if reply.sending then "Posting\x2026" else "Post") ]
    , case reply.error of
        Just err -> HH.p [ cls "error" ] [ HH.text err ]
        Nothing -> HH.text ""
    ]

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> load

  Receive input -> do
    current <- H.gets _.input
    when (input /= current) do
      H.put (initialState input)
      load

  Reload -> do
    H.modify_ _ { main = Pending, comments = Pending, notice = Nothing }
    load

  Back -> liftEffect goBack

  DismissNotice -> H.modify_ _ { notice = Nothing }

  Card id action -> case action of
    Reveal -> H.modify_ \s -> s { revealed = Set.insert id s.revealed }
    OpenAccount accountId -> liftEffect (navigate (Profile accountId))
    -- Already on the detail screen; the comments are below.
    OpenStatus _ -> pure unit
    ToggleFavourite -> toggle id Ops.ToggleFavourite
    ToggleBookmark -> toggle id Ops.ToggleBookmark

  SetReply text -> H.modify_ \s -> s { reply = s.reply { text = text } }

  SubmitReply event -> do
    liftEffect (preventDefault event)
    state <- H.get
    let text = trim state.reply.text
    case state.main of
      Ready main | text /= "" && not state.reply.sending -> do
        backend <- asks _.backend
        H.modify_ \s -> s { reply = s.reply { sending = true, error = Nothing } }
        result <- liftAff (postReply backend (statusId (displayed main)) text)
        H.modify_ \s -> case result of
          Left err -> s { reply = s.reply { sending = false, error = Just (printAppError err) } }
          Right comment -> s
            { reply = { text: "", sending: false, error: Nothing }
            , comments = case s.comments of
                Ready cs -> Ready (Array.snoc cs comment)
                other -> other
            , main = adjustReplies 1 s.main
            }
      _ -> pure unit

  DeleteComment id -> do
    state <- H.get
    when (not (Set.member id state.busy)) do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (deleteStatus backend id)
      H.modify_ \s -> case result of
        Left err -> s { busy = Set.delete id s.busy, notice = Just (printAppError err) }
        Right _ -> s
          { busy = Set.delete id s.busy
          , comments = case s.comments of
              Ready cs -> Ready (Array.filter (\c -> statusId c /= id) cs)
              other -> other
          , main = adjustReplies (-1) s.main
          }

adjustReplies :: Int -> Loadable Status -> Loadable Status
adjustReplies delta = case _ of
  Ready st -> Ready (bump st)
  other -> other
  where
  bump (Status top) = case top.reblog of
    Just (Status inner) -> Status top { reblog = Just (Status inner { replyCount = max 0 (inner.replyCount + delta) }) }
    Nothing -> Status top { replyCount = max 0 (top.replyCount + delta) }

load :: forall o m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () o m Unit
load = do
  backend <- asks _.backend
  id <- H.gets _.input.id
  stamp <- liftEffect now
  Tuple main context <- liftAff $ sequential $
    Tuple <$> parallel (getStatus backend id) <*> parallel (getContext backend id)
  H.modify_ \s -> s
    { now = Just stamp
    , main = case main of
        Left err -> Missing err
        Right st -> Ready st
    , comments = case context of
        Left err -> Missing err
        Right ctx -> Ready ctx.descendants
    }

-- | Flips a like or bookmark on the main post or on a comment.
toggle :: forall o m. MonadAff m => MonadAsk Env m => StatusId -> Toggle -> H.HalogenM State Action () o m Unit
toggle id which = do
  state <- H.get
  let
    target = case state.main of
      Ready st | statusId (displayed st) == id -> Just (displayed st)
      _ -> case state.comments of
        Ready cs -> Array.find (\c -> statusId c == id) cs
        _ -> Nothing
  case target of
    Just st | not (Set.member id state.busy) -> do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (Ops.request which st backend id)
      H.modify_ \s -> s
        { busy = Set.delete id s.busy
        , notice = case result of
            Left err -> Just (printAppError err)
            Right _ -> s.notice
        , main = case result, s.main of
            Right fromServer, Ready m -> Ready (Ops.replaceDisplayed id (Ops.merge which st fromServer) m)
            _, m -> m
        , comments = case result, s.comments of
            Right fromServer, Ready cs -> Ready (Ops.replaceAll id (Ops.merge which st fromServer) cs)
            _, cs -> cs
        }
    _ -> pure unit
