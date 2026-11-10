-- | The way into everything that is not a tab: the profile editor, the
-- | viewer's saved posts and lists, and logging out. Logging out takes two
-- | taps, like every other action here that cannot be undone from the app.
module Component.Settings
  ( Input
  , component
  ) where

import Prelude

import App.Monad (Env)
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import View.Common (cls, screenHeader)

-- | What the session knows: the instance and the handle on it.
type Input = { domain :: String, acct :: Maybe String }

type State = { input :: Input, logoutArmed :: Boolean }

data Action
  = Receive Input
  | Back
  | Go Route
  | ArmLogout
  | DisarmLogout
  | ConfirmLogout

component :: forall m. MonadAff m => MonadAsk Env m => H.Component Screen.Query Input Screen.Output m
component = H.mkComponent
  { initialState: \input -> { input, logoutArmed: false }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "settings" ]
    [ screenHeader Back "Settings" []
    , group "Account"
        [ entry "Edit profile" EditProfile
        , entry "Follow requests" FollowRequests
        , entry "Messages" Conversations
        ]
    , group "Your posts"
        [ entry "Bookmarks" Bookmarks
        , entry "Likes" Likes
        , entry "Followed hashtags" FollowedTags
        ]
    , group "Privacy"
        [ entry "Muted accounts" Muted
        , entry "Blocked accounts" Blocked
        ]
    , HH.div [ cls "settings-group" ]
        [ HH.h2_ [ HH.text "Session" ]
        , HH.p [ cls "muted settings-line" ]
            [ HH.text case state.input.acct of
                Just acct -> "Signed in as @" <> acct <> " on " <> state.input.domain
                Nothing -> "Signed in on " <> state.input.domain
            ]
        , if state.logoutArmed then
            HH.div [ cls "confirm" ]
              [ HH.span_ [ HH.text "Log out of this device?" ]
              , HH.button [ cls "confirm-yes", HP.type_ HP.ButtonButton, HE.onClick \_ -> ConfirmLogout ]
                  [ HH.text "Log out" ]
              , HH.button [ cls "ghost", HP.type_ HP.ButtonButton, HE.onClick \_ -> DisarmLogout ] [ HH.text "Stay" ]
              ]
          else
            HH.button [ cls "settings-entry danger", HP.type_ HP.ButtonButton, HE.onClick \_ -> ArmLogout ]
              [ HH.text "Log out" ]
        , HH.p [ cls "muted settings-line" ]
            [ HH.text "Logging out removes the session from this device. The server keeps the token until it expires; revoke it from the instance's own settings page if that matters." ]
        ]
    ]
  where
  group title entries = HH.div [ cls "settings-group" ] ([ HH.h2_ [ HH.text title ] ] <> entries)

  entry label route =
    HH.button [ cls "settings-entry", HP.type_ HP.ButtonButton, HE.onClick \_ -> Go route ]
      [ HH.span_ [ HH.text label ], HH.span [ cls "muted" ] [ HH.text "\x203A" ] ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  Back -> liftEffect goBack
  Go route -> liftEffect (navigate route)
  ArmLogout -> H.modify_ _ { logoutArmed = true }
  DisarmLogout -> H.modify_ _ { logoutArmed = false }
  ConfirmLogout -> H.raise Screen.LogoutRequested
