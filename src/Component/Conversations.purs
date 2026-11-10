-- | The viewer's direct message conversations, most recent first. A row
-- | opens the thread with the other person. New conversations start from a
-- | profile's Message button; there is no recipient picker here.
module Component.Conversations (component) where

import Prelude

import App.Monad (Env)
import App.Paged (Paged, Phase(..))
import App.Paged as Paged
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..))
import Data.String (take)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (conversationsPager, runPager)
import Pixelfed.Entity (AccountId, Conversation)
import View.Common (agoLabel, avatar, cls, errorBox, handleName, iconButton, pagedFooter, screenHeader, statusLine)

type State =
  { list :: Paged Conversation
  , now :: Maybe Instant
  }

data Action
  = Initialize
  | Reload
  | LoadMore
  | Back
  | Open AccountId

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
  HH.section [ cls "conversations" ]
    [ screenHeader Back "Messages" [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , case state.list.phase of
        FirstLoading -> statusLine "Loading\x2026"
        FirstFailed err -> errorBox Reload err
        _ ->
          HH.div_
            [ if Array.null state.list.items then statusLine "No messages yet. Open someone's profile and press Message."
              else HH.div [ cls "notif-list" ] (Array.mapMaybe (row state.now) state.list.items)
            , pagedFooter LoadMore "" state.list
            ]
    ]

-- | A conversation is drawn by its first other participant. One with no
-- | readable participant has nobody to open and is left out.
row :: forall m. Maybe Instant -> Conversation -> Maybe (H.ComponentHTML Action () m)
row stamp conversation = Array.head conversation.accounts <#> \other ->
  HH.article [ cls "notif" ]
    [ avatar "avatar avatar-small" other.avatar
    , HH.button [ cls "notif-body", HP.type_ HP.ButtonButton, HE.onClick \_ -> Open other.id ]
        [ HH.div [ cls "notif-text" ]
            [ HH.p [ cls "notif-line" ]
                [ HH.strong_ [ HH.text (handleName other.displayName other.acct) ]
                , if conversation.unread then HH.span [ cls "badge" ] [ HH.text "new" ] else HH.text ""
                ]
            , HH.p [ cls "notif-preview" ] [ HH.text (excerpt conversation.lastText) ]
            , HH.span [ cls "card-time", HP.title conversation.lastAt ] [ HH.text (agoLabel stamp conversation.lastAt) ]
            ]
        ]
    ]

excerpt :: String -> String
excerpt s = if String.length s > 90 then take 87 s <> "\x2026" else s

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

  Reload -> do
    H.modify_ _ { list = Paged.loading }
    load

  LoadMore -> loadMore

  Back -> liftEffect goBack

  Open id -> liftEffect (navigate (Thread id))

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  stamp <- liftEffect now
  result <- liftAff (runPager conversationsPager backend)
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
      H.modify_ \s -> s { list = Paged.more _.id result s.list }
