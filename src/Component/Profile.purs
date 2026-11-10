-- | An account: header, how the viewer stands to it, and the grid of its
-- | posts. Profile posts paginate by `max_id` because that endpoint sends no
-- | Link header. The viewer's own profile carries the way into the editor
-- | and the settings instead of the follow controls.
module Component.Profile
  ( Input
  , component
  ) where

import Prelude

import App.Backend (AppError, Backend, printAppError)
import App.Loadable (Loadable(..), fromEither)
import App.Monad (Env)
import App.Paged (Paged, Phase(..))
import App.Paged as Paged
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (accountStatusesPager, block, follow, getAccount, getRelationship, mute, runPager, unblock, unfollow, unmute)
import Pixelfed.Entity (Account, AccountId, Relationship, Status, statusId)
import Pixelfed.StatusOps (removeStatus, replaceAll)
import Util.Text (stripTags)
import View.Common (avatar, cls, errorBox, handleName, iconButton, noticeBar, pagedFooter, screenHeader, statusLine)
import View.Tile (postGrid)

type Input = { id :: AccountId, me :: Maybe AccountId }

-- | A change to the relationship. Each is one POST that answers with the
-- | relationship as it now stands.
data Change = ToggleFollow | ToggleMute | Unblock | Block

type State =
  { input :: Input
  , account :: Loadable Account
  , relationship :: Maybe Relationship
  , relationshipBusy :: Boolean
  -- | Blocking asks first: it also removes the follow in both directions.
  , blockArmed :: Boolean
  , posts :: Paged Status
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | Reload
  | Back
  | Go Route
  | Apply Change
  | ArmBlock
  | DisarmBlock
  | LoadMore
  | DismissNotice

component :: forall m. MonadAff m => MonadAsk Env m => H.Component Screen.Query Input Screen.Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState input =
  { input
  , account: Pending
  , relationship: Nothing
  , relationshipBusy: false
  , blockArmed: false
  , posts: Paged.loading
  , notice: Nothing
  }

isSelf :: State -> Boolean
isSelf state = state.input.me == Just state.input.id

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "profile" ]
    [ screenHeader Back (title state)
        ( (if isSelf state then [ iconButton "ghost" "Settings" "\x2699" (Go Settings) ] else [])
            <> [ iconButton "ghost" "Reload" "\x21BB" Reload ]
        )
    , blockPrompt state
    , noticeBar DismissNotice state.notice
    , case state.account of
        Pending -> statusLine "Loading\x2026"
        Missing err -> errorBox Reload err
        Ready account ->
          HH.div_
            [ profileHead state account
            , grid state account
            ]
    ]

title :: State -> String
title state = case state.account of
  Ready a -> "@" <> a.acct
  _ -> "Profile"

profileHead :: forall m. State -> Account -> H.ComponentHTML Action () m
profileHead state account =
  HH.div [ cls "profile-head" ]
    [ HH.div [ cls "profile-top" ]
        [ avatar "avatar avatar-large" account.avatar
        , HH.div [ cls "profile-counts" ]
            [ counter account.statusesCount "posts" Nothing
            , counter account.followersCount "followers" (Just (Followers account.id))
            , counter account.followingCount "following" (Just (Following account.id))
            ]
        ]
    , HH.div [ cls "profile-names" ]
        [ HH.strong_ [ HH.text (handleName account.displayName account.acct) ]
        , HH.span [ cls "acct" ]
            [ HH.text ("@" <> account.acct <> if account.locked then " \x1F512" else "") ]
        , case state.relationship of
            Just r | r.followedBy -> HH.span [ cls "badge" ] [ HH.text "Follows you" ]
            _ -> HH.text ""
        ]
    , let
        bio = trim (fromMaybe (stripTags account.note) account.noteText)
      in
        if bio == "" then HH.text "" else HH.p [ cls "profile-bio" ] [ HH.text bio ]
    , case account.website of
        Just site | trim site /= "" -> HH.p [ cls "profile-site" ] [ HH.text site ]
        _ -> HH.text ""
    , if isSelf state then
        HH.div [ cls "profile-actions" ]
          [ HH.button [ cls "follow follow-secondary", HP.type_ HP.ButtonButton, HE.onClick \_ -> Go EditProfile ]
              [ HH.text "Edit profile" ]
          ]
      else relationControls state account
    ]

counter :: forall m. Int -> String -> Maybe Route -> H.ComponentHTML Action () m
counter n label target = case target of
  Just route ->
    HH.button [ cls "counter counter-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> Go route ] inner
  Nothing -> HH.div [ cls "counter" ] inner
  where
  inner = [ HH.strong_ [ HH.text (show n) ], HH.span [ cls "muted" ] [ HH.text label ] ]

-- | Follow and Message on one row, then the quieter controls. A blocked
-- | account shows Unblock in place of Follow and no Message.
relationControls :: forall m. State -> Account -> H.ComponentHTML Action () m
relationControls state account =
  HH.div_
    [ HH.div [ cls "profile-actions" ]
        ( case state.relationship of
            Just r | r.blocking ->
              [ HH.button
                  [ cls "follow follow-secondary", HP.type_ HP.ButtonButton, HP.disabled busy, HE.onClick \_ -> Apply Unblock ]
                  [ HH.text "Unblock" ]
              ]
            _ ->
              [ followButton state
              , HH.button
                  [ cls "follow follow-secondary", HP.type_ HP.ButtonButton, HE.onClick \_ -> Go (Thread account.id) ]
                  [ HH.text "Message" ]
              ]
        )
    , HH.div [ cls "profile-minor" ]
        ( case state.relationship of
            Nothing -> []
            Just r ->
              [ HH.button [ cls "link", HP.type_ HP.ButtonButton, HP.disabled busy, HE.onClick \_ -> Apply ToggleMute ]
                  [ HH.text (if r.muting then "Unmute" else "Mute") ]
              , if r.blocking then HH.text ""
                else
                  HH.button [ cls "link", HP.type_ HP.ButtonButton, HP.disabled busy, HE.onClick \_ -> ArmBlock ]
                    [ HH.text "Block" ]
              , HH.button [ cls "link", HP.type_ HP.ButtonButton, HE.onClick \_ -> Go (ReportAccount account.id) ]
                  [ HH.text "Report" ]
              ]
        )
    ]
  where
  busy = state.relationshipBusy

followButton :: forall m. State -> H.ComponentHTML Action () m
followButton state =
  let
    label = case state.relationship of
      Nothing -> "\x2026"
      Just r
        | r.following -> "Following"
        | r.requested -> "Requested"
        | otherwise -> "Follow"
    secondary = case state.relationship of
      Just r -> r.following || r.requested
      Nothing -> true
  in
    HH.button
      [ cls (if secondary then "follow follow-secondary" else "follow")
      , HP.type_ HP.ButtonButton
      , HP.disabled (state.relationshipBusy || not (isJust state.relationship))
      , HE.onClick \_ -> Apply ToggleFollow
      ]
      [ HH.text label ]

blockPrompt :: forall m. State -> H.ComponentHTML Action () m
blockPrompt state
  | not state.blockArmed = HH.text ""
  | otherwise =
      HH.div [ cls "confirm" ]
        [ HH.span_ [ HH.text "Block this account? You will stop following each other." ]
        , HH.button
            [ cls "confirm-yes", HP.type_ HP.ButtonButton, HP.disabled state.relationshipBusy, HE.onClick \_ -> Apply Block ]
            [ HH.text "Block" ]
        , HH.button [ cls "ghost", HP.type_ HP.ButtonButton, HE.onClick \_ -> DisarmBlock ] [ HH.text "Cancel" ]
        ]

grid :: forall m. State -> Account -> H.ComponentHTML Action () m
grid state account =
  HH.div_
    [ case state.posts.phase of
        FirstLoading -> statusLine "Loading posts\x2026"
        FirstFailed err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
        _ ->
          if Array.null state.posts.items then
            statusLine
              if account.locked && not following && not (isSelf state) then "This account is private."
              else "No posts yet."
          else postGrid (Go <<< StatusDetail) state.posts.items
    , pagedFooter LoadMore "" state.posts
    ]
  where
  following = case state.relationship of
    Just r -> r.following
    Nothing -> false

handleQuery :: forall a m. MonadAff m => MonadAsk Env m => Screen.Query a -> H.HalogenM State Action () Screen.Output m (Maybe a)
handleQuery = case _ of
  Screen.NearBottom next -> do
    loadMore
    pure (Just next)
  Screen.StatusRemoved id next -> do
    H.modify_ \s -> s { posts = Paged.modifyItems (removeStatus id) s.posts }
    pure (Just next)
  Screen.StatusUpdated status next -> do
    H.modify_ \s -> s { posts = Paged.modifyItems (replaceAll (statusId status) status) s.posts }
    pure (Just next)
  Screen.AccountUpdated id next -> do
    shown <- H.gets _.input.id
    when (id == shown) do
      H.modify_ \s -> initialState s.input
      load
    pure (Just next)

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

  Go route -> liftEffect (navigate route)

  DismissNotice -> H.modify_ _ { notice = Nothing }

  ArmBlock -> H.modify_ _ { blockArmed = true }

  DisarmBlock -> H.modify_ _ { blockArmed = false }

  LoadMore -> loadMore

  Apply change -> do
    state <- H.get
    case state.relationship of
      Just rel | not state.relationshipBusy -> do
        backend <- asks _.backend
        H.modify_ _ { relationshipBusy = true }
        result <- liftAff (requestFor change rel backend state.input.id)
        H.modify_ \s -> case result of
          Left err -> s { relationshipBusy = false, blockArmed = false, notice = Just (printAppError err) }
          Right updated -> s { relationshipBusy = false, blockArmed = false, relationship = Just updated }
      _ -> pure unit

-- | Unfollow on a requested relationship withdraws the request, as
-- | pixelfed-rn does.
requestFor :: Change -> Relationship -> Backend -> AccountId -> Aff (Either AppError Relationship)
requestFor change rel = case change of
  ToggleFollow -> if rel.following || rel.requested then unfollow else follow
  ToggleMute -> if rel.muting then unmute else mute
  Block -> block
  Unblock -> unblock

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  state <- H.get
  let id = state.input.id
  Tuple account relationship <- liftAff $ sequential $
    Tuple
      <$> parallel (getAccount backend id)
      <*> parallel (if isSelf state then pure (Right Nothing) else getRelationship backend id)
  H.modify_ \s ->
    if s.input /= state.input then s
    else s
      { account = fromEither account
      , relationship = case relationship of
          Right r -> r
          Left _ -> Nothing
      , notice = case relationship of
          Left err -> Just (printAppError err)
          Right _ -> s.notice
      }
  case account of
    Right _ -> do
      result <- liftAff (runPager (accountStatusesPager id) backend)
      H.modify_ \s -> if s.input /= state.input then s else s { posts = Paged.first result }
    Left _ -> H.modify_ \s -> s { posts = s.posts { phase = Exhausted } }

loadMore :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
loadMore = do
  state <- H.get
  case Paged.wantsMore state.posts of
    Nothing -> pure unit
    Just pager -> do
      backend <- asks _.backend
      H.modify_ \s -> s { posts = Paged.startMore s.posts }
      result <- liftAff (runPager pager backend)
      H.modify_ \s -> if s.input /= state.input then s else s { posts = Paged.more statusId result s.posts }
