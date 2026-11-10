-- | One direct message conversation. The server gives eight messages a
-- | request, newest first; they are drawn oldest first with the control for
-- | earlier ones at the top and the composer at the bottom. Only text is
-- | sent; photos and videos others sent are shown.
module Component.Thread
  ( Input
  , component
  ) where

import Prelude

import App.Backend (printAppError)
import App.Loadable (Loadable(..), fromEither)
import App.Monad (Env)
import App.Paged (Paged, Phase(..))
import App.Paged as Paged
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (deleteMessage, getAccount, runPager, sendMessage, threadPager)
import Pixelfed.Entity (Account, AccountId, Message, MessageId)
import View.Common (agoLabel, cls, errorBox, handleName, iconButton, lazy, noticeBar, screenHeader, statusLine)
import Web.Event.Event (Event, preventDefault)

type Input = { id :: AccountId }

-- | Pixelfed 0.12 validates a message as at most 500 characters; newer
-- | servers allow more. The lower limit works against both.
maxMessageLength :: Int
maxMessageLength = 500

type State =
  { input :: Input
  , other :: Loadable Account
  -- | Newest first, as the server sends them.
  , list :: Paged Message
  , now :: Maybe Instant
  , draft :: String
  , sending :: Boolean
  , sendError :: Maybe String
  , busy :: Set MessageId
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | Reload
  | Back
  | OpenProfile
  | LoadEarlier
  | SetDraft String
  | Submit Event
  | Delete MessageId
  | DismissNotice

component :: forall m. MonadAff m => MonadAsk Env m => H.Component Screen.Query Input Screen.Output m
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
  , other: Pending
  , list: Paged.loading
  , now: Nothing
  , draft: ""
  , sending: false
  , sendError: Nothing
  , busy: Set.empty
  , notice: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "thread" ]
    [ screenHeader Back title
        [ iconButton "ghost" "Profile" "\x1F464" OpenProfile
        , iconButton "ghost" "Reload" "\x21BB" Reload
        ]
    , noticeBar DismissNotice state.notice
    , case state.list.phase of
        FirstLoading -> statusLine "Loading\x2026"
        FirstFailed err -> errorBox Reload err
        _ ->
          HH.div_
            [ earlier state.list
            , if Array.null state.list.items then statusLine "No messages yet. Say hello."
              else HH.div [ cls "bubbles" ] (map (bubble state) (Array.reverse state.list.items))
            ]
    , composer state
    ]
  where
  title = case state.other of
    Ready account -> handleName account.displayName account.acct
    _ -> "Messages"

earlier :: forall m. Paged Message -> H.ComponentHTML Action () m
earlier list = HH.div [ cls "feed-footer" ] case list.phase of
  Idle _ -> [ HH.button [ cls "secondary", HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadEarlier ] [ HH.text "Earlier messages" ] ]
  LoadingMore _ -> [ statusLine "Loading\x2026" ]
  MoreFailed err _ ->
    [ HH.p_ [ HH.text (printAppError err) ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadEarlier ] [ HH.text "Try again" ]
    ]
  _ -> []

bubble :: forall m. State -> Message -> H.ComponentHTML Action () m
bubble state message =
  HH.div [ cls (if message.isAuthor then "bubble bubble-mine" else "bubble") ]
    [ body
    , HH.div [ cls "bubble-meta" ]
        [ HH.span [ cls "card-time", HP.title message.createdAt ] [ HH.text age ]
        , if message.isAuthor && message.seen then HH.span [ cls "card-time" ] [ HH.text "seen" ] else HH.text ""
        , if message.isAuthor then
            HH.button
              [ cls "link"
              , HP.type_ HP.ButtonButton
              , HP.disabled (Set.member message.id state.busy)
              , HE.onClick \_ -> Delete message.id
              ]
              [ HH.text "Delete" ]
          else HH.text ""
        ]
    ]
  where
  -- A computed age when the server sent a timestamp, its own label when
  -- it did not (0.12 omits `created_at` from the answer to a send).
  age = case agoLabel state.now message.createdAt of
    "" -> message.timeAgo
    label -> label

  body = case message.media of
    Just url | message.kind == "photo" ->
      HH.img [ cls "bubble-media", HP.src url, HP.alt "Photo", lazy ]
    Just url | message.kind == "video" ->
      HH.video [ cls "bubble-media", HP.src url, HP.controls true, HP.preload HP.PreloadMetadata, HP.attr (HH.AttrName "playsinline") "" ] []
    _ ->
      HH.p [ cls "bubble-text" ]
        [ HH.text (if trim message.text == "" then "(" <> message.kind <> ")" else message.text) ]

composer :: forall m. State -> H.ComponentHTML Action () m
composer state =
  HH.form [ cls "reply-form thread-composer", HE.onSubmit Submit ]
    [ HH.textarea
        [ HP.value state.draft
        , HP.placeholder "Message"
        , HP.rows 2
        , HP.attr (HH.AttrName "maxlength") (show maxMessageLength)
        , HP.disabled state.sending
        , HE.onValueInput SetDraft
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit, HP.disabled (state.sending || trim state.draft == "") ]
        [ HH.text (if state.sending then "Sending\x2026" else "Send") ]
    , case state.sendError of
        Just err -> HH.p [ cls "error" ] [ HH.text err ]
        Nothing -> HH.text ""
    ]

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Receive input -> do
    current <- H.gets _.input
    when (input /= current) do
      H.put (initialState input)
      load

  Reload -> do
    -- The draft survives a reload; everything fetched does not.
    H.modify_ \s -> (initialState s.input) { draft = s.draft }
    load

  Back -> liftEffect goBack

  OpenProfile -> do
    id <- H.gets _.input.id
    liftEffect (navigate (Profile id))

  DismissNotice -> H.modify_ _ { notice = Nothing }

  SetDraft text -> H.modify_ _ { draft = String.take maxMessageLength text, sendError = Nothing }

  LoadEarlier -> do
    state <- H.get
    case Paged.wantsMore state.list of
      Nothing -> pure unit
      Just pager -> do
        backend <- asks _.backend
        H.modify_ \s -> s { list = Paged.startMore s.list }
        result <- liftAff (runPager pager backend)
        H.modify_ \s -> if s.input /= state.input then s else s { list = Paged.more _.id result s.list }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    let text = trim state.draft
    when (text /= "" && not state.sending) do
      backend <- asks _.backend
      H.modify_ _ { sending = true, sendError = Nothing }
      result <- liftAff (sendMessage backend state.input.id text)
      stamp <- liftEffect now
      H.modify_ \s -> case result of
        Left err -> s { sending = false, sendError = Just (printAppError err) }
        Right message -> s
          { sending = false
          , draft = ""
          , now = Just stamp
          , list = Paged.modifyItems (Array.cons message <<< Array.filter (\m -> m.id /= message.id)) s.list
          }

  Delete id -> do
    busy <- H.gets _.busy
    when (not (Set.member id busy)) do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (deleteMessage backend id)
      H.modify_ \s -> case result of
        Left err -> s { busy = Set.delete id s.busy, notice = Just (printAppError err) }
        Right _ -> s
          { busy = Set.delete id s.busy
          , list = Paged.modifyItems (Array.filter (\m -> m.id /= id)) s.list
          }

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  input <- H.gets _.input
  stamp <- liftEffect now
  Tuple other chunk <- liftAff $ sequential $
    Tuple <$> parallel (getAccount backend input.id) <*> parallel (runPager (threadPager input.id) backend)
  H.modify_ \s ->
    if s.input /= input then s
    else s { now = Just stamp, other = fromEither other, list = Paged.first chunk }
