-- | Instance picker plus the hand-off to the system browser. The actual OAuth
-- | exchange happens in Rust; this component only learns the outcome through
-- | the `login-failed` event (failure) or the root's `session-changed` (success).
module Component.Login (component) where

import Prelude

import App.Backend (AppError, LoginStarted, printAppError)
import App.Monad (Env)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.Event.Event (Event, preventDefault)

type State = { domain :: String, phase :: Phase }

data Phase
  = Entering
  | Working
  | Waiting LoginStarted
  | Failed AppError

data Action
  = Initialize
  | SetDomain String
  | Submit Event
  | LoginFailed AppError
  | Reset

component :: forall q i o m. MonadAff m => MonadAsk Env m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { domain: "", phase: Entering }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "login" ]
    [ HH.h1_ [ HH.text "pellucid-ps" ]
    , HH.p [ cls "muted" ] [ HH.text "Sign in to a Pellucid server." ]
    , case state.phase of
        Waiting started ->
          HH.div [ cls "login-waiting" ]
            [ HH.p_ [ HH.text "Finish signing in using the browser window that just opened." ]
            , HH.p [ cls "muted" ]
                [ HH.text "If nothing opened, copy this address into a browser:" ]
            , HH.code [ cls "url" ] [ HH.text started.authorizeUrl ]
            , HH.button [ cls "ghost", HP.type_ HP.ButtonButton, HE.onClick \_ -> Reset ] [ HH.text "Start over" ]
            ]
        _ ->
          HH.form [ cls "login-form", HE.onSubmit Submit ]
            [ HH.label [ HP.for "domain" ] [ HH.text "Server" ]
            , HH.input
                [ HP.id "domain"
                , HP.type_ HP.InputText
                , HP.placeholder "pellucid.social"
                , HP.value state.domain
                , HP.autocomplete HP.AutocompleteOff
                , HP.autofocus true
                , HP.disabled (isWorking state.phase)
                , HE.onValueInput SetDomain
                ]
            , HH.button
                [ HP.type_ HP.ButtonSubmit, HP.disabled (isWorking state.phase || trim state.domain == "") ]
                [ HH.text (if isWorking state.phase then "Contacting server\x2026" else "Sign in") ]
            , case state.phase of
                Failed err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
                _ -> HH.text ""
            ]
    ]

isWorking :: Phase -> Boolean
isWorking = case _ of
  Working -> true
  _ -> false

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    backend <- asks _.backend
    void $ H.subscribe $ HS.makeEmitter \push -> backend.onLoginFailed (push <<< LoginFailed)

  SetDomain domain -> H.modify_ _ { domain = domain }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    let domain = trim state.domain
    when (domain /= "" && not (isWorking state.phase)) do
      backend <- asks _.backend
      H.modify_ _ { phase = Working }
      result <- liftAff (backend.loginStart domain)
      H.modify_ _
        { phase = case result of
            Left err -> Failed err
            Right started -> Waiting started
        }

  LoginFailed err -> H.modify_ _ { phase = Failed err }

  Reset -> H.modify_ _ { phase = Entering }

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
