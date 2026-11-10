-- | The viewer's own display name, bio, website and whether follows need
-- | approval. The limits are the server's validation rules for
-- | `update_credentials`; the fields stop at them so the 422 is never seen.
module Component.EditProfile (component) where

import Prelude

import App.Backend (printAppError)
import App.Loadable (Loadable(..))
import App.Monad (Env)
import App.Route (goBack)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (updateCredentials, verifyCredentials)
import Pixelfed.Entity (Account, ProfileEdit)
import Util.Text (stripTags)
import View.Common (cls, errorBox, iconButton, screenHeader, statusLine)
import Web.Event.Event (Event, preventDefault)

maxDisplayName :: Int
maxDisplayName = 30

maxNote :: Int
maxNote = 200

maxWebsite :: Int
maxWebsite = 120

data Saving = Idle | Saving | Saved | SaveFailed String

type State =
  { account :: Loadable Account
  , form :: ProfileEdit
  , saving :: Saving
  }

data Action
  = Initialize
  | Reload
  | Back
  | SetDisplayName String
  | SetNote String
  | SetWebsite String
  | SetLocked Boolean
  | Submit Event

component :: forall i m. MonadAff m => MonadAsk Env m => H.Component Screen.Query i Screen.Output m
component = H.mkComponent
  { initialState: \_ ->
      { account: Pending
      , form: { displayName: "", note: "", website: "", locked: false }
      , saving: Idle
      }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

formOf :: Account -> ProfileEdit
formOf account =
  { displayName: account.displayName
  , note: trim (fromMaybe (stripTags account.note) account.noteText)
  , website: fromMaybe "" account.website
  , locked: account.locked
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "edit-profile" ]
    [ screenHeader Back "Edit profile" [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , case state.account of
        Pending -> statusLine "Loading\x2026"
        Missing err -> errorBox Reload err
        Ready _ -> form state
    ]

form :: forall m. State -> H.ComponentHTML Action () m
form state =
  HH.form [ cls "stack-form", HE.onSubmit Submit ]
    [ field "Display name" (counter state.form.displayName maxDisplayName)
        ( HH.input
            [ HP.type_ HP.InputText
            , HP.value state.form.displayName
            , HP.attr (HH.AttrName "maxlength") (show maxDisplayName)
            , HP.disabled busy
            , HE.onValueInput SetDisplayName
            ]
        )
    , field "Bio" (counter state.form.note maxNote)
        ( HH.textarea
            [ HP.value state.form.note
            , HP.rows 4
            , HP.attr (HH.AttrName "maxlength") (show maxNote)
            , HP.disabled busy
            , HE.onValueInput SetNote
            ]
        )
    , field "Website" (counter state.form.website maxWebsite)
        ( HH.input
            [ HP.type_ HP.InputUrl
            , HP.value state.form.website
            , HP.placeholder "https://"
            , HP.attr (HH.AttrName "maxlength") (show maxWebsite)
            , HP.attr (HH.AttrName "autocapitalize") "none"
            , HP.disabled busy
            , HE.onValueInput SetWebsite
            ]
        )
    , HH.label [ cls "check" ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.form.locked
            , HP.disabled busy
            , HE.onChecked SetLocked
            ]
        , HH.span_ [ HH.text "Private account: new followers need my approval" ]
        ]
    , HH.button [ HP.type_ HP.ButtonSubmit, HP.disabled busy ]
        [ HH.text (if busy then "Saving\x2026" else "Save") ]
    , case state.saving of
        Saved -> HH.p [ cls "muted" ] [ HH.text "Saved." ]
        SaveFailed message -> HH.p [ cls "error" ] [ HH.text message ]
        _ -> HH.text ""
    ]
  where
  busy = case state.saving of
    Saving -> true
    _ -> false

  field label right control =
    HH.label [ cls "field" ]
      [ HH.span [ cls "field-label" ] [ HH.text label, HH.span [ cls "counter-text" ] [ HH.text right ] ]
      , control
      ]

  counter text limit = show (String.length text) <> "/" <> show limit

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Reload -> do
    H.modify_ _ { account = Pending, saving = Idle }
    load

  Back -> liftEffect goBack

  SetDisplayName text -> edit _ { displayName = String.take maxDisplayName text }
  SetNote text -> edit _ { note = String.take maxNote text }
  SetWebsite text -> edit _ { website = String.take maxWebsite text }
  SetLocked locked -> edit _ { locked = locked }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    case state.account, state.saving of
      Ready _, Saving -> pure unit
      Ready _, _ -> do
        backend <- asks _.backend
        H.modify_ _ { saving = Saving }
        result <- liftAff (updateCredentials backend state.form)
        case result of
          Left err -> H.modify_ _ { saving = SaveFailed (printAppError err) }
          Right account -> do
            -- The server's copy replaces the form: it trims and cleans.
            H.modify_ _ { account = Ready account, form = formOf account, saving = Saved }
            H.raise (Screen.AccountChanged account.id)
      _, _ -> pure unit
  where
  -- Any edit takes back the Saved or failed line.
  edit f = H.modify_ \s -> s { form = f s.form, saving = Idle }

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  result <- liftAff (verifyCredentials backend)
  H.modify_ \s -> case result of
    Left err -> s { account = Missing err }
    Right account -> s { account = Ready account, form = formOf account }
