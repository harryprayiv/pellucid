-- | An account: header, follow control and the grid of its posts.
-- | Profile posts paginate by `max_id` because that endpoint sends no Link
-- | header; a page that comes back empty ends the grid.
module Component.Profile
  ( Input
  , component
  ) where

import Prelude

import App.Backend (AppError, printAppError)
import App.Monad (Env)
import App.Route (Route(..), goBack, navigate)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String (take, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (follow, getAccount, getAccountStatuses, getRelationship, unfollow)
import Pixelfed.Entity (Account, AccountId, MediaType(..), Relationship, Status(..), StatusId, captionOf, displayed, firstMedia, statusId)
import Util.Text (stripTags)
import View.Common (avatar, cls, handleName, iconButton, lazy, screenHeader)

type Input = { id :: AccountId, me :: Maybe AccountId }

data Loadable a = Pending | Missing AppError | Ready a

data PostsPhase = PostsLoading | PostsIdle | PostsExhausted | PostsFailed AppError

type State =
  { input :: Input
  , account :: Loadable Account
  , relationship :: Maybe Relationship
  , relationshipBusy :: Boolean
  , posts :: Array Status
  , postsPhase :: PostsPhase
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | Reload
  | Back
  | ToggleFollow
  | LoadMore
  | OpenPost StatusId
  | DismissNotice

component :: forall q o m. MonadAff m => MonadAsk Env m => H.Component q Input o m
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
  , account: Pending
  , relationship: Nothing
  , relationshipBusy: false
  , posts: []
  , postsPhase: PostsLoading
  , notice: Nothing
  }

isSelf :: State -> Boolean
isSelf state = state.input.me == Just state.input.id

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "profile" ]
    [ screenHeader Back (title state) [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , case state.notice of
        Nothing -> HH.text ""
        Just text ->
          HH.div [ cls "notice" ]
            [ HH.span_ [ HH.text text ], iconButton "ghost" "Dismiss" "\x2715" DismissNotice ]
    , case state.account of
        Pending -> HH.p [ cls "status-line" ] [ HH.text "Loading\x2026" ]
        Missing err ->
          HH.div [ cls "error-box" ]
            [ HH.p_ [ HH.text (printAppError err) ]
            , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> Reload ] [ HH.text "Try again" ]
            ]
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
            [ counter account.statusesCount "posts"
            , counter account.followersCount "followers"
            , counter account.followingCount "following"
            ]
        ]
    , HH.div [ cls "profile-names" ]
        [ HH.strong_ [ HH.text (handleName account.displayName account.acct) ]
        , HH.span [ cls "acct" ]
            [ HH.text ("@" <> account.acct <> if account.locked then " \x1F512" else "") ]
        ]
    , let
        bio = trim (fromMaybe (stripTags account.note) account.noteText)
      in
        if bio == "" then HH.text "" else HH.p [ cls "profile-bio" ] [ HH.text bio ]
    , case account.website of
        Just site | trim site /= "" -> HH.p [ cls "profile-site" ] [ HH.text site ]
        _ -> HH.text ""
    , if isSelf state then HH.text "" else followButton state
    ]

counter :: forall m. Int -> String -> H.ComponentHTML Action () m
counter n label =
  HH.div [ cls "counter" ]
    [ HH.strong_ [ HH.text (show n) ], HH.span [ cls "muted" ] [ HH.text label ] ]

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
      , HE.onClick \_ -> ToggleFollow
      ]
      [ HH.text label ]

grid :: forall m. State -> Account -> H.ComponentHTML Action () m
grid state account =
  HH.div_
    [ if Array.null state.posts then
        case state.postsPhase of
          PostsLoading -> HH.p [ cls "status-line" ] [ HH.text "Loading posts\x2026" ]
          PostsFailed err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
          _ ->
            HH.p [ cls "status-line" ]
              [ HH.text
                  if account.locked && not (followedBy state) && not (isSelf state) then "This account is private."
                  else "No posts yet."
              ]
      else HH.div [ cls "grid" ] (map tile state.posts)
    , HH.div [ cls "feed-footer" ] case state.postsPhase of
        PostsLoading | not (Array.null state.posts) -> [ HH.p [ cls "status-line" ] [ HH.text "Loading more\x2026" ] ]
        PostsIdle -> [ HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadMore ] [ HH.text "Load more" ] ]
        PostsFailed err | not (Array.null state.posts) ->
          [ HH.p_ [ HH.text (printAppError err) ]
          , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> LoadMore ] [ HH.text "Try again" ]
          ]
        _ -> []
    ]
  where
  followedBy st = case st.relationship of
    Just r -> r.following
    Nothing -> false

tile :: forall m. Status -> H.ComponentHTML Action () m
tile status =
  let
    Status shown = displayed status
  in
    HH.button
      [ cls "tile", HP.type_ HP.ButtonButton, HE.onClick \_ -> OpenPost shown.id ]
      case firstMedia shown of
        Just m | not shown.sensitive ->
          [ HH.img
              [ cls "tile-image"
              , HP.src (fromMaybe (fromMaybe m.url m.optimizedUrl) m.previewUrl)
              , HP.alt (fromMaybe "" m.description)
              , lazy
              ]
          , if m.mediaType == VideoMedia || m.mediaType == Gifv then HH.span [ cls "tile-badge" ] [ HH.text "\x25B6" ]
            else if Array.length shown.mediaAttachments > 1 then HH.span [ cls "tile-badge" ] [ HH.text "\x29C9" ]
            else HH.text ""
          ]
        Just _ -> [ HH.span [ cls "tile-text" ] [ HH.text "Sensitive" ] ]
        Nothing -> [ HH.span [ cls "tile-text" ] [ HH.text (excerpt (captionOf shown)) ] ]

excerpt :: String -> String
excerpt s = if String.length s > 80 then take 77 s <> "\x2026" else s

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> load

  Receive input -> do
    current <- H.gets _.input
    when (input /= current) do
      H.put (initialState input)
      load

  Reload -> do
    H.modify_ \s -> (initialState s.input)
    load

  Back -> liftEffect goBack

  DismissNotice -> H.modify_ _ { notice = Nothing }

  OpenPost id -> liftEffect (navigate (StatusDetail id))

  ToggleFollow -> do
    state <- H.get
    case state.relationship of
      Just rel | not state.relationshipBusy -> do
        backend <- asks _.backend
        H.modify_ _ { relationshipBusy = true }
        let action = if rel.following || rel.requested then unfollow else follow
        result <- liftAff (action backend state.input.id)
        H.modify_ \s -> case result of
          Left err -> s { relationshipBusy = false, notice = Just (printAppError err) }
          Right updated -> s { relationshipBusy = false, relationship = Just updated }
      _ -> pure unit

  LoadMore -> do
    state <- H.get
    case state.postsPhase, Array.last state.posts of
      PostsIdle, Just lastPost -> fetchPosts (Just (statusId lastPost))
      PostsFailed _, Just lastPost -> fetchPosts (Just (statusId lastPost))
      PostsFailed _, Nothing -> fetchPosts Nothing
      _, _ -> pure unit

load :: forall o m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () o m Unit
load = do
  backend <- asks _.backend
  state <- H.get
  let id = state.input.id
  Tuple account relationship <- liftAff $ sequential $
    Tuple
      <$> parallel (getAccount backend id)
      <*> parallel (if isSelf state then pure (Right Nothing) else getRelationship backend id)
  H.modify_ \s -> s
    { account = case account of
        Left err -> Missing err
        Right a -> Ready a
    , relationship = case relationship of
        Right r -> r
        Left _ -> Nothing
    , notice = case relationship of
        Left err -> Just (printAppError err)
        Right _ -> s.notice
    }
  case account of
    Right _ -> fetchPosts Nothing
    Left _ -> H.modify_ _ { postsPhase = PostsIdle }

fetchPosts :: forall o m. MonadAff m => MonadAsk Env m => Maybe StatusId -> H.HalogenM State Action () o m Unit
fetchPosts maxId = do
  backend <- asks _.backend
  id <- H.gets _.input.id
  H.modify_ _ { postsPhase = PostsLoading }
  result <- liftAff (getAccountStatuses backend id maxId)
  H.modify_ \s -> case result of
    Left err -> s { postsPhase = PostsFailed err }
    Right page ->
      let
        known = Set.fromFoldable (map statusId s.posts)
        fresh = Array.filter (\st -> not (Set.member (statusId st) known)) page
      in
        s
          { posts = s.posts <> fresh
          , postsPhase = if Array.null fresh then PostsExhausted else PostsIdle
          }