-- | Top of the component tree: decides between the login screen and the
-- | signed-in shell, keeps the stack of mounted screens, and carries what
-- | one screen has to tell the others.
module Component.Root (component) where

import Prelude

import App.Backend (SessionSummary, printAppError)
import App.Monad (Env)
import App.Route (Route(..), navigate, parseRoute)
import App.Screen as Screen
import App.Stack (Entry, Stack)
import App.Stack as Stack
import Component.AccountList as AccountList
import Component.Compose as Compose
import Component.Conversations as Conversations
import Component.EditPost as EditPost
import Component.EditProfile as EditProfile
import Component.Explore as Explore
import Component.Feed as Feed
import Component.FollowedTags as FollowedTags
import Component.Login as Login
import Component.Notifications as Notifications
import Component.Profile as Profile
import Component.Report as Report
import Component.Settings as Settings
import Component.StatusDetail as StatusDetail
import Component.Thread as Thread
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Pixelfed.Entity (AccountId(..), ReportTarget(..))
import Halogen.Query (tellAll)
import Routing.Hash (getHash, matchesWith)
import Type.Proxy (Proxy(..))
import Util.Scroll as Scroll
import View.Common (cls, statusLine)

type State =
  { phase :: Phase
  -- | The route in the address bar, kept while nobody is logged in so the
  -- | first screen after boot is the one that was asked for.
  , wanted :: Route
  , nextKey :: Int
  }

data Phase
  = Booting
  | BootFailed String
  | LoggedOut
  | LoggedIn SessionSummary Stack

data Action
  = Initialize
  | SessionChanged (Maybe SessionSummary)
  | RouteChanged Route
  | Go Route
  | Scrolled
  | Raised Screen.Output

-- | Every screen answers the same queries and raises the same outputs; the
-- | slot index is the stack entry's key.
type ScreenSlot = H.Slot Screen.Query Screen.Output Int

type Slots =
  ( login :: forall q. H.Slot q Void Unit
  , feed :: ScreenSlot
  , explore :: ScreenSlot
  , notifications :: ScreenSlot
  , compose :: ScreenSlot
  , detail :: ScreenSlot
  , profile :: ScreenSlot
  , accounts :: ScreenSlot
  , settings :: ScreenSlot
  , editProfile :: ScreenSlot
  , followedTags :: ScreenSlot
  , conversations :: ScreenSlot
  , thread :: ScreenSlot
  , report :: ScreenSlot
  , editPost :: ScreenSlot
  )

_login = Proxy :: Proxy "login"
_feed = Proxy :: Proxy "feed"
_explore = Proxy :: Proxy "explore"
_notifications = Proxy :: Proxy "notifications"
_compose = Proxy :: Proxy "compose"
_detail = Proxy :: Proxy "detail"
_profile = Proxy :: Proxy "profile"
_accounts = Proxy :: Proxy "accounts"
_settings = Proxy :: Proxy "settings"
_editProfile = Proxy :: Proxy "editProfile"
_followedTags = Proxy :: Proxy "followedTags"
_conversations = Proxy :: Proxy "conversations"
_thread = Proxy :: Proxy "thread"
_report = Proxy :: Proxy "report"
_editPost = Proxy :: Proxy "editPost"

component :: forall q i o m. MonadAff m => MonadAsk Env m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { phase: Booting, wanted: Home, nextKey: 0 }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. MonadAff m => MonadAsk Env m => State -> H.ComponentHTML Action Slots m
render state = case state.phase of
  Booting -> HH.main [ cls "centered" ] [ HH.p [ cls "muted" ] [ HH.text "Starting\x2026" ] ]
  BootFailed message -> HH.main [ cls "centered" ] [ HH.p [ cls "error" ] [ HH.text message ] ]
  LoggedOut -> HH.main_ [ HH.slot_ _login unit Login.component unit ]
  LoggedIn session stack ->
    let
      front = Stack.front stack
      tabRoute = (Stack.base stack).route
    in
      HH.div [ cls "shell" ]
        [ HH.main [ cls "content" ]
            [ HK.div_ (map (mounted session front.key) (NEA.toArray stack)) ]
        , HH.nav [ cls "tabbar" ]
            [ tab (tabRoute == Home || tabRoute == Public) Home "\x2302" "Home"
            , tab (tabRoute == Explore) Explore "\x1F50D" "Explore"
            , tab (tabRoute == Compose) Compose "\x2795" "Post"
            , tab (tabRoute == Notifications) Notifications "\x1F514" "Alerts"
            , tab (tabRoute == Me) Me "\x1F464" "Me"
            ]
        ]

-- | One stack entry. Every entry stays in the document; only the one in
-- | front is displayed.
mounted
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => SessionSummary
  -> Int
  -> Entry
  -> Tuple String (H.ComponentHTML Action Slots m)
mounted session frontKey entry =
  Tuple (show entry.key)
    ( HH.div [ cls (if entry.key == frontKey then "screen" else "screen screen-hidden") ]
        [ screen session entry ]
    )

tab :: forall m. Boolean -> Route -> String -> String -> H.ComponentHTML Action Slots m
tab active target icon label =
  HH.button
    [ cls (if active then "tab tab-active" else "tab")
    , HP.type_ HP.ButtonButton
    , HE.onClick \_ -> Go target
    ]
    [ HH.span [ cls "tab-icon" ] [ HH.text icon ], HH.span [ cls "tab-label" ] [ HH.text label ] ]

screen :: forall m. MonadAff m => MonadAsk Env m => SessionSummary -> Entry -> H.ComponentHTML Action Slots m
screen session entry = case entry.route of
  Home -> feed Feed.HomeFeed
  Public -> feed Feed.PublicFeed
  Bookmarks -> feed Feed.BookmarksFeed
  Likes -> feed Feed.LikesFeed
  Tag name -> feed (Feed.TagFeed name)
  Explore -> HH.slot _explore key Explore.component unit Raised
  Notifications -> HH.slot _notifications key Notifications.component unit Raised
  Compose -> HH.slot _compose key Compose.component unit Raised
  Me -> case me of
    Just id -> HH.slot _profile key Profile.component { id, me } Raised
    Nothing -> statusLine "This session has no account."
  Profile id -> HH.slot _profile key Profile.component { id, me } Raised
  StatusDetail id -> HH.slot _detail key StatusDetail.component { id, me } Raised
  Followers id -> accounts (AccountList.FollowersOf id)
  Following id -> accounts (AccountList.FollowingOf id)
  LikedBy id -> accounts (AccountList.LikedByOf id)
  SharedBy id -> accounts (AccountList.SharedByOf id)
  Muted -> accounts AccountList.MutedAccounts
  Blocked -> accounts AccountList.BlockedAccounts
  FollowRequests -> accounts AccountList.PendingRequests
  Settings ->
    HH.slot _settings key Settings.component
      { domain: session.domain, acct: map _.acct session.account }
      Raised
  EditProfile -> HH.slot _editProfile key EditProfile.component unit Raised
  FollowedTags -> HH.slot _followedTags key FollowedTags.component unit Raised
  Conversations -> HH.slot _conversations key Conversations.component unit Raised
  Thread id -> HH.slot _thread key Thread.component { id } Raised
  ReportPost id -> HH.slot _report key Report.component (AboutPost id) Raised
  ReportAccount id -> HH.slot _report key Report.component (AboutAccount id) Raised
  EditPost id -> HH.slot _editPost key EditPost.component { id, me } Raised
  where
  key = entry.key
  me = map (\a -> AccountId a.id) session.account
  feed source = HH.slot _feed key Feed.component source Raised
  accounts kind = HH.slot _accounts key AccountList.component kind Raised

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    backend <- asks _.backend
    void $ H.subscribe $ HS.makeEmitter \push -> backend.onSessionChanged (push <<< SessionChanged)
    void $ H.subscribe $ HS.makeEmitter \push ->
      matchesWith parseRoute \_ new -> push (RouteChanged new)
    scrolls <- liftEffect (Scroll.scrolls Scrolled)
    void $ H.subscribe scrolls
    initial <- liftEffect getHash
    let wanted = fromMaybe Home (parseRoute initial)
    H.modify_ _ { wanted = wanted }
    result <- liftAff backend.sessionGet
    case result of
      Left err -> H.modify_ _ { phase = BootFailed (printAppError err) }
      Right Nothing -> H.modify_ _ { phase = LoggedOut }
      Right (Just session) -> enter session wanted

  SessionChanged session -> case session of
    Nothing -> H.modify_ _ { phase = LoggedOut, wanted = Home }
    Just s -> do
      enter s Home
      liftEffect (navigate Home)

  RouteChanged route -> do
    state <- H.get
    case state.phase of
      LoggedIn session stack -> do
        here <- liftEffect Scroll.position
        let step = Stack.navigate state.nextKey here route stack
        H.put state { phase = LoggedIn session step.stack, nextKey = step.nextKey, wanted = route }
        -- The render above has already swapped which screen is displayed.
        liftEffect (Scroll.restore step.scrollTo)
      _ -> H.modify_ _ { wanted = route }

  Go route -> do
    state <- H.get
    case state.phase of
      -- Pressing the tab that is already showing goes to the top of it.
      LoggedIn _ stack | (Stack.front stack).route == route -> liftEffect (Scroll.restore 0.0)
      _ -> liftEffect (navigate route)

  Scrolled -> do
    phase <- H.gets _.phase
    case phase of
      LoggedIn _ stack -> do
        near <- liftEffect Scroll.nearBottom
        when near (tellFront (Stack.front stack))
      _ -> pure unit

  Raised output -> case output of
    Screen.LogoutRequested -> do
      backend <- asks _.backend
      result <- liftAff backend.logout
      case result of
        -- The shell emits session-changed on success; a failure only means
        -- the file could not be removed, and the UI still has to let the
        -- person out.
        Left _ -> H.modify_ _ { phase = LoggedOut, wanted = Home }
        Right _ -> pure unit

    Screen.StatusDeleted id -> do
      tellAll _feed (Screen.StatusRemoved id)
      tellAll _profile (Screen.StatusRemoved id)
      tellAll _detail (Screen.StatusRemoved id)

    Screen.StatusChanged status -> do
      tellAll _feed (Screen.StatusUpdated status)
      tellAll _profile (Screen.StatusUpdated status)
      tellAll _detail (Screen.StatusUpdated status)

    Screen.AccountChanged id -> tellAll _profile (Screen.AccountUpdated id)

-- | Starts the signed-in shell on one screen.
enter :: forall o m. SessionSummary -> Route -> H.HalogenM State Action Slots o m Unit
enter session route =
  H.modify_ \s -> s
    { phase = LoggedIn session (Stack.single s.nextKey route)
    , nextKey = s.nextKey + 1
    , wanted = route
    }

-- | Tells the screen in front that the page is near its end. Only the
-- | screens that page a list listen; the rest have no such slot to tell.
tellFront :: forall o m. Entry -> H.HalogenM State Action Slots o m Unit
tellFront entry = case entry.route of
  Home -> H.tell _feed key Screen.NearBottom
  Public -> H.tell _feed key Screen.NearBottom
  Bookmarks -> H.tell _feed key Screen.NearBottom
  Likes -> H.tell _feed key Screen.NearBottom
  Tag _ -> H.tell _feed key Screen.NearBottom
  Notifications -> H.tell _notifications key Screen.NearBottom
  Me -> H.tell _profile key Screen.NearBottom
  Profile _ -> H.tell _profile key Screen.NearBottom
  Followers _ -> H.tell _accounts key Screen.NearBottom
  Following _ -> H.tell _accounts key Screen.NearBottom
  LikedBy _ -> H.tell _accounts key Screen.NearBottom
  SharedBy _ -> H.tell _accounts key Screen.NearBottom
  Muted -> H.tell _accounts key Screen.NearBottom
  Blocked -> H.tell _accounts key Screen.NearBottom
  FollowRequests -> H.tell _accounts key Screen.NearBottom
  FollowedTags -> H.tell _followedTags key Screen.NearBottom
  Conversations -> H.tell _conversations key Screen.NearBottom
  Explore -> pure unit
  Compose -> pure unit
  StatusDetail _ -> pure unit
  Settings -> pure unit
  EditProfile -> pure unit
  Thread _ -> pure unit
  ReportPost _ -> pure unit
  ReportAccount _ -> pure unit
  EditPost _ -> pure unit
  where
  key = entry.key
