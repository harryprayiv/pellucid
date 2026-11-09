-- | The viewer's notifications, newest first, paginated by the Link header
-- | like a timeline. A row names the actor and what they did; tapping it
-- | opens the post concerned, or the actor's profile when there is none.
module Component.Notifications (component) where

import Prelude

import App.Backend (AppError, Cursor, printAppError)
import App.Monad (Env)
import App.Route (Route(..), navigate)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (take)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (followNotifications, notifications)
import Pixelfed.Entity (Notification, NotificationKind(..), Status(..), captionOf, displayed, firstMedia, statusId)
import View.Common (agoLabel, avatar, cls, handleName, iconButton, lazy)

type State =
  { list :: ListState
  , now :: Maybe Instant
  }

data ListState
  = Loading
  | Failed AppError
  | Loaded Loaded

type Loaded = { items :: Array Notification, next :: Maybe Cursor, more :: MoreState }

data MoreState = Idle | LoadingMore | MoreFailed AppError | Exhausted

data Action
  = Initialize
  | Refresh
  | LoadMore
  | Open Route

component :: forall q i o m. MonadAff m => MonadAsk Env m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { list: Loading, now: Nothing }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "notifications" ]
    [ HH.header [ cls "feed-header" ]
        [ HH.h1_ [ HH.text "Notifications" ]
        , iconButton "ghost" "Refresh" "\x21BB" Refresh
        ]
    , case state.list of
        Loading -> HH.p [ cls "status-line" ] [ HH.text "Loading\x2026" ]
        Failed err ->
          HH.div [ cls "error-box" ]
            [ HH.p_ [ HH.text (printAppError err) ]
            , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> Refresh ] [ HH.text "Try again" ]
            ]
        Loaded page ->
          HH.div_
            [ if Array.null page.items then HH.p [ cls "status-line" ] [ HH.text "Nothing yet." ]
              else HH.div [ cls "notif-list" ] (map (row state.now) page.items)
            , footer page.next page.more
            ]
    ]

row :: forall m. Maybe Instant -> Notification -> H.ComponentHTML Action () m
row stamp n =
  HH.article [ cls "notif" ]
    [ case n.account of
        Just actor ->
          HH.button
            [ cls "avatar-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> Open (Profile actor.id) ]
            [ avatar "avatar avatar-small" actor.avatar ]
        Nothing -> HH.span [ cls "avatar avatar-small avatar-blank" ] []
    , HH.button
        ( [ cls "notif-body", HP.type_ HP.ButtonButton ]
            <> case target n of
              Just route -> [ HE.onClick \_ -> Open route ]
              Nothing -> [ HP.disabled true ]
        )
        [ HH.div [ cls "notif-text" ]
            [ HH.p [ cls "notif-line" ]
                [ HH.strong_ [ HH.text (actorName n) ]
                , HH.text (" " <> phrase n.kind)
                ]
            , case preview n of
                "" -> HH.text ""
                text -> HH.p [ cls "notif-preview" ] [ HH.text text ]
            , HH.span [ cls "card-time", HP.title n.createdAt ] [ HH.text (agoLabel stamp n.createdAt) ]
            ]
        , thumbnail n
        ]
    ]

actorName :: Notification -> String
actorName n = case n.account of
  Just a -> handleName a.displayName a.acct
  Nothing -> "Someone"

-- | What the actor did, in the words pixelfed-rn uses for the same verbs.
phrase :: NotificationKind -> String
phrase = case _ of
  NFollow -> "followed you"
  NMention -> "mentioned you"
  NShare -> "shared your post"
  NFavourite -> "liked your post"
  NComment -> "commented on your post"
  NTagged -> "tagged you in a post"
  NModlog -> "left a moderation note"
  NDirect -> "sent you a message"
  NStoryReact -> "reacted to your story"
  NStoryComment -> "commented on your story"
  NOther verb -> verb

-- | Where a tap goes. The server's `status` is the liked or shared post for
-- | a like or a share, the comment itself for a comment and the mentioning
-- | post for a mention; a comment opens its parent so the comment is read
-- | in place. A follow, or anything without a post, opens the actor.
target :: Notification -> Maybe Route
target n = case n.kind, n.tagged, n.status of
  NTagged, Just t, _ -> Just (StatusDetail t.statusId)
  NComment, _, Just (Status c) -> Just (StatusDetail (fromMaybe c.id c.inReplyToId))
  _, _, Just st -> Just (StatusDetail (statusId (displayed st)))
  _, _, Nothing -> map (\a -> Profile a.id) n.account

-- | A line of the post concerned: a comment's or mention's own text, else
-- | the caption of the post that was liked or shared.
preview :: Notification -> String
preview n = case n.status of
  Just st ->
    let
      Status shown = displayed st
    in
      excerpt (captionOf shown)
  Nothing -> ""

excerpt :: String -> String
excerpt s = if String.length s > 90 then take 87 s <> "\x2026" else s

thumbnail :: forall m. Notification -> H.ComponentHTML Action () m
thumbnail n = case n.status of
  Just st ->
    let
      Status shown = displayed st
    in
      case firstMedia shown of
        Just m | not shown.sensitive ->
          HH.img
            [ cls "notif-thumb"
            , HP.src (fromMaybe (fromMaybe m.url m.optimizedUrl) m.previewUrl)
            , HP.alt ""
            , lazy
            ]
        Just _ -> HH.span [ cls "notif-thumb notif-thumb-blank" ] []
        Nothing -> HH.text ""
  Nothing -> HH.text ""

footer :: forall m. Maybe Cursor -> MoreState -> H.ComponentHTML Action () m
footer next more = HH.div [ cls "feed-footer" ] case next, more of
  _, LoadingMore -> [ HH.p [ cls "status-line" ] [ HH.text "Loading more\x2026" ] ]
  _, MoreFailed err ->
    [ HH.p_ [ HH.text (printAppError err) ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadMore ] [ HH.text "Try again" ]
    ]
  Nothing, _ -> [ HH.p [ cls "status-line" ] [ HH.text "That's everything." ] ]
  _, Exhausted -> [ HH.p [ cls "status-line" ] [ HH.text "That's everything." ] ]
  Just _, Idle -> [ HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadMore ] [ HH.text "Load more" ] ]

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> load

  Refresh -> do
    H.modify_ _ { list = Loading }
    load

  LoadMore -> do
    state <- H.get
    case state.list of
      Loaded page@{ next: Just cursor, more: Idle } -> loadMore page cursor
      Loaded page@{ next: Just cursor, more: MoreFailed _ } -> loadMore page cursor
      _ -> pure unit

  Open route -> liftEffect (navigate route)

load :: forall o m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () o m Unit
load = do
  backend <- asks _.backend
  stamp <- liftEffect now
  result <- liftAff (notifications backend)
  H.modify_ \s -> s
    { now = Just stamp
    , list = case result of
        Left err -> Failed err
        Right page -> Loaded { items: page.items, next: page.next, more: Idle }
    }

loadMore :: forall o m. MonadAff m => MonadAsk Env m => Loaded -> Cursor -> H.HalogenM State Action () o m Unit
loadMore page cursor = do
  backend <- asks _.backend
  H.modify_ _ { list = Loaded page { more = LoadingMore } }
  result <- liftAff (followNotifications backend cursor)
  stamp <- liftEffect now
  H.modify_ \s -> s
    { now = Just stamp
    , list = case result of
        Left err -> Loaded page { more = MoreFailed err }
        Right next ->
          let
            known = Set.fromFoldable (map _.id page.items)
            fresh = Array.filter (\n -> not (Set.member n.id known)) next.items
          in
            Loaded
              { items: page.items <> fresh
              , next: next.next
              , more: if Array.null fresh || next.next == Nothing then Exhausted else Idle
              }
    }
