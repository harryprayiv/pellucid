-- | Top of the component tree: decides between the login screen and the
-- | signed-in shell, and maps the hash route to a screen.
module Component.Root (component) where

import Prelude

import App.Backend (SessionSummary, printAppError)
import App.Monad (Env)
import App.Route (Route(..), navigate, parseRoute)
import Component.Feed as Feed
import Component.Login as Login
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Pellucid.Entity (AccountId(..))
import Routing.Hash (getHash, matchesWith)
import Type.Proxy (Proxy(..))

type State = { phase :: Phase, route :: Route }

data Phase
  = Booting
  | BootFailed String
  | LoggedOut
  | LoggedIn SessionSummary

data Action
  = Initialize
  | SessionChanged (Maybe SessionSummary)
  | RouteChanged Route
  | Go Route
  | Logout

type Slots =
  ( login :: forall q. H.Slot q Void Unit
  , feed :: forall q. H.Slot q Void Unit
  )

_login = Proxy :: Proxy "login"
_feed = Proxy :: Proxy "feed"

component :: forall q i o m. MonadAff m => MonadAsk Env m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { phase: Booting, route: Home }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. MonadAff m => MonadAsk Env m => State -> H.ComponentHTML Action Slots m
render state = case state.phase of
  Booting -> HH.main [ cls "centered" ] [ HH.p [ cls "muted" ] [ HH.text "Starting\x2026" ] ]
  BootFailed message -> HH.main [ cls "centered" ] [ HH.p [ cls "error" ] [ HH.text message ] ]
  LoggedOut -> HH.main_ [ HH.slot_ _login unit Login.component unit ]
  LoggedIn session ->
    HH.div [ cls "shell" ]
      [ HH.main [ cls "content" ] [ screen state.route ]
      , HH.nav [ cls "tabbar" ]
          [ tab state.route Home "\x2302" "Home"
          , tab state.route Public "\x1F310" "Public"
          , tab state.route Notifications "\x1F514" "Alerts"
          , case session.account of
              Just account -> tab state.route (Profile (AccountId account.id)) "\x1F464" "Me"
              Nothing -> HH.text ""
          , HH.button [ cls "tab", HP.type_ HP.ButtonButton, HP.title "Log out", HE.onClick \_ -> Logout ]
              [ HH.span [ cls "tab-icon" ] [ HH.text "\x23FB" ], HH.span [ cls "tab-label" ] [ HH.text "Log out" ] ]
          ]
      ]

tab :: forall m. Route -> Route -> String -> String -> H.ComponentHTML Action Slots m
tab current target icon label =
  HH.button
    [ cls (if current == target then "tab tab-active" else "tab")
    , HP.type_ HP.ButtonButton
    , HE.onClick \_ -> Go target
    ]
    [ HH.span [ cls "tab-icon" ] [ HH.text icon ], HH.span [ cls "tab-label" ] [ HH.text label ] ]

screen :: forall m. MonadAff m => MonadAsk Env m => Route -> H.ComponentHTML Action Slots m
screen = case _ of
  Home -> HH.slot_ _feed unit Feed.component Feed.HomeFeed
  Public -> HH.slot_ _feed unit Feed.component Feed.PublicFeed
  Notifications -> notPorted "Notifications"
  Profile id -> notPorted ("Profile " <> unwrap id)
  StatusDetail id -> notPorted ("Post " <> unwrap id)

notPorted :: forall m. String -> H.ComponentHTML Action Slots m
notPorted what =
  HH.section [ cls "placeholder" ]
    [ HH.h1_ [ HH.text what ]
    , HH.p [ cls "muted" ] [ HH.text "This screen has not been ported yet." ]
    ]

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    backend <- asks _.backend
    void $ H.subscribe $ HS.makeEmitter \push -> backend.onSessionChanged (push <<< SessionChanged)
    void $ H.subscribe $ HS.makeEmitter \push ->
      matchesWith parseRoute \_ new -> push (RouteChanged new)
    initial <- liftEffect getHash
    H.modify_ _ { route = maybe Home identity (parseRoute initial) }
    result <- liftAff backend.sessionGet
    H.modify_ _
      { phase = case result of
          Left err -> BootFailed (printAppError err)
          Right Nothing -> LoggedOut
          Right (Just session) -> LoggedIn session
      }

  SessionChanged session -> do
    H.modify_ _ { phase = maybe LoggedOut LoggedIn session, route = Home }
    liftEffect (navigate Home)

  RouteChanged route -> H.modify_ _ { route = route }

  Go route -> liftEffect (navigate route)

  Logout -> do
    backend <- asks _.backend
    result <- liftAff backend.logout
    case result of
      -- The shell emits session-changed on success; a failure only means the
      -- file could not be removed, and the UI still has to let the user out.
      Left _ -> H.modify_ _ { phase = LoggedOut }
      Right _ -> pure unit

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
