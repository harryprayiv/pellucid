-- | Editing the viewer's own post: caption, sensitivity and content warning.
-- | Attachments are neither added nor removed here, and every one the post
-- | has is sent back with the edit, because the server deletes any
-- | attachment an edit leaves out. The server allows ten edits a post.
module Component.EditPost
  ( Input
  , component
  ) where

import Prelude

import App.Backend (printAppError)
import App.Loadable (Loadable(..))
import App.Monad (Env)
import App.Route (goBack)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.String (trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (composeSettings, editStatus, getStatus)
import Pixelfed.Entity (AccountId, Status(..), StatusId, captionOf, displayed)
import View.Common (cls, errorBox, iconButton, screenHeader, statusLine)
import Web.Event.Event (Event, preventDefault)

type Input = { id :: StatusId, me :: Maybe AccountId }

-- | Pixelfed's stock caption limit, used when the instance's own cannot be read.
defaultMaxCaption :: Int
defaultMaxCaption = 500

-- | `spoiler_text` is validated as max:140.
maxSpoilerLength :: Int
maxSpoilerLength = 140

data Saving = Idle | Saving | SaveFailed String

type State =
  { input :: Input
  , status :: Loadable Status
  , maxCaption :: Int
  , caption :: String
  , sensitive :: Boolean
  , spoilerText :: String
  , saving :: Saving
  }

data Action
  = Initialize
  | Receive Input
  | Reload
  | Back
  | SetCaption String
  | SetSensitive Boolean
  | SetSpoiler String
  | Submit Event

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
  , status: Pending
  , maxCaption: defaultMaxCaption
  , caption: ""
  , sensitive: false
  , spoilerText: ""
  , saving: Idle
  }

-- | Only the author may edit, and a share has nothing of its own to edit.
editable :: State -> Status -> Boolean
editable state (Status s) = isNothing s.reblog && state.input.me == Just s.account.id

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "edit-post" ]
    [ screenHeader Back "Edit post" [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , case state.status of
        Pending -> statusLine "Loading\x2026"
        Missing err -> errorBox Reload err
        Ready status
          | editable state status -> form state
          | otherwise -> statusLine "Only the author can edit this post."
    ]

form :: forall m. State -> H.ComponentHTML Action () m
form state =
  HH.form [ cls "stack-form", HE.onSubmit Submit ]
    [ HH.label [ cls "field" ]
        [ HH.span [ cls "field-label" ]
            [ HH.text "Caption"
            , HH.span [ cls "counter-text" ]
                [ HH.text (show (String.length state.caption) <> "/" <> show state.maxCaption) ]
            ]
        , HH.textarea
            [ HP.value state.caption
            , HP.rows 5
            , HP.disabled busy
            , HE.onValueInput SetCaption
            ]
        ]
    , HH.label [ cls "check" ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox, HP.checked state.sensitive, HP.disabled busy, HE.onChecked SetSensitive ]
        , HH.span_ [ HH.text "Sensitive content" ]
        ]
    , if state.sensitive then
        HH.label [ cls "field" ]
          [ HH.span [ cls "field-label" ]
              [ HH.text "Content warning"
              , HH.span [ cls "counter-text" ]
                  [ HH.text (show (String.length state.spoilerText) <> "/" <> show maxSpoilerLength) ]
              ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.value state.spoilerText
              , HP.attr (HH.AttrName "maxlength") (show maxSpoilerLength)
              , HP.disabled busy
              , HE.onValueInput SetSpoiler
              ]
          ]
      else HH.text ""
    , HH.button [ HP.type_ HP.ButtonSubmit, HP.disabled (busy || tooLong) ]
        [ HH.text (if busy then "Saving\x2026" else "Save") ]
    , case state.saving of
        SaveFailed message -> HH.p [ cls "error" ] [ HH.text message ]
        _ -> HH.text ""
    ]
  where
  busy = case state.saving of
    Saving -> true
    _ -> false

  tooLong = String.length state.caption > state.maxCaption

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Receive input -> do
    current <- H.gets _.input
    when (input /= current) do
      H.put (initialState input)
      load

  Reload -> do
    H.modify_ \s -> initialState s.input
    load

  Back -> liftEffect goBack

  SetCaption text -> H.modify_ _ { caption = text, saving = Idle }

  SetSensitive flag -> H.modify_ _ { sensitive = flag, saving = Idle }

  SetSpoiler text -> H.modify_ _ { spoilerText = String.take maxSpoilerLength text, saving = Idle }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    case state.status, state.saving of
      Ready _, Saving -> pure unit
      Ready status@(Status s), _ | editable state status && String.length state.caption <= state.maxCaption -> do
        backend <- asks _.backend
        H.modify_ _ { saving = Saving }
        result <- liftAff $ editStatus backend s.id
          { caption: trim state.caption
          , sensitive: state.sensitive
          -- A warning belongs to a sensitive post only.
          , spoilerText: if state.sensitive then state.spoilerText else ""
          , mediaIds: map _.id s.mediaAttachments
          }
        case result of
          Left err -> H.modify_ _ { saving = SaveFailed (printAppError err) }
          Right updated -> do
            H.raise (Screen.StatusChanged (displayed updated))
            liftEffect goBack
      _, _ -> pure unit

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  input <- H.gets _.input
  Tuple status settings <- liftAff $ sequential $
    Tuple <$> parallel (getStatus backend input.id) <*> parallel (composeSettings backend)
  H.modify_ \s ->
    if s.input /= input then s
    else case status of
      Left err -> s { status = Missing err }
      Right st@(Status shown) -> s
        { status = Ready st
        , caption = captionOf shown
        , sensitive = shown.sensitive
        , spoilerText = shown.spoilerText
        , maxCaption = case settings of
            Right limits -> limits.maxCaptionLength
            Left _ -> defaultMaxCaption
        }
