-- | The viewer's notifications, newest first, paginated by the Link header
-- | like a timeline. A row names the actor and what they did; tapping it
-- | opens the post concerned, the conversation for a direct message, or the
-- | actor's profile when there is nothing else to open.
module Component.Notifications (component) where

import Prelude

import App.Monad (Env)
import App.Paged (Paged, Phase(..))
import App.Paged as Paged
import App.Route (Route(..), navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (take)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (notificationsPager, runPager)
import Pixelfed.Entity (Notification, NotificationKind(..), Status(..), captionOf, displayed, firstMedia, statusId)
import View.Common (agoLabel, avatar, cls, errorBox, handleName, iconButton, lazy, pagedFooter, statusLine)

type State =
  { list :: Paged Notification
  , now :: Maybe Instant
  }

data Action
  = Initialize
  | Refresh
  | LoadMore
  | Open Route

component :: forall i m. MonadAff m => MonadAsk Env m => H.Component Screen.Query i Screen.Output m
component = H.mkComponent
  { initialState: \_ -> { list: Paged.loading, now: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "notifications" ]
    [ HH.header [ cls "feed-header" ]
        [ HH.h1_ [ HH.text "Notifications" ]
        , iconButton "ghost" "Refresh" "\x21BB" Refresh
        ]
    , case state.list.phase of
        FirstLoading -> statusLine "Loading\x2026"
        FirstFailed err -> errorBox Refresh err
        _ ->
          HH.div_
            [ if Array.null state.list.items then statusLine "Nothing yet."
              else HH.div [ cls "notif-list" ] (map (row state.now) state.list.items)
            , pagedFooter LoadMore "That's everything." state.list
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
-- | in place. A direct message opens the conversation with its sender. A
-- | follow, or anything without a post, opens the actor.
target :: Notification -> Maybe Route
target n = case n.kind, n.tagged, n.status of
  NDirect, _, _ -> map (\a -> Thread a.id) n.account
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

handleQuery :: forall a m. MonadAff m => MonadAsk Env m => Screen.Query a -> H.HalogenM State Action () Screen.Output m (Maybe a)
handleQuery = case _ of
  Screen.NearBottom next -> do
    loadMore
    pure (Just next)
  Screen.StatusRemoved _ next -> pure (Just next)
  Screen.StatusUpdated _ next -> pure (Just next)
  Screen.AccountUpdated _ next -> pure (Just next)

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Refresh -> do
    H.modify_ _ { list = Paged.loading }
    load

  LoadMore -> loadMore

  Open route -> liftEffect (navigate route)

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  stamp <- liftEffect now
  result <- liftAff (runPager notificationsPager backend)
  H.modify_ _ { now = Just stamp, list = Paged.first result }

loadMore :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
loadMore = do
  state <- H.get
  case Paged.wantsMore state.list of
    Nothing -> pure unit
    Just pager -> do
      backend <- asks _.backend
      H.modify_ \s -> s { list = Paged.startMore s.list }
      result <- liftAff (runPager pager backend)
      stamp <- liftEffect now
      H.modify_ \s -> s { now = Just stamp, list = Paged.more _.id result s.list }
