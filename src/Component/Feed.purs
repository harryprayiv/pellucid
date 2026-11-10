-- | A list of posts drawn as cards: the home and public timelines, the
-- | viewer's bookmarks and likes, and a hashtag's feed. Owns per-card UI
-- | state (revealed sensitive media, in-flight toggles) keyed by status id.
module Component.Feed
  ( Source(..)
  , component
  ) where

import Prelude

import App.Backend (printAppError)
import App.Monad (Env)
import App.Paged (Paged, Phase(..))
import App.Paged as Paged
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (Pager, bookmarksPager, favouritesPager, followTag, getTag, homePager, publicPager, runPager, tagPager, unfollowTag)
import Pixelfed.Entity (HashtagInfo, Status, StatusId, displayed, statusId)
import Pixelfed.StatusOps (Toggle)
import Pixelfed.StatusOps as Ops
import View.Common (cls, errorBox, iconButton, noticeBar, pagedFooter, screenHeader, statusLine)
import View.StatusCard (CardAction(..), handleCardNavigation, statusCard)

data Source
  = HomeFeed
  | PublicFeed
  | BookmarksFeed
  | LikesFeed
  | TagFeed String

derive instance Eq Source

type State =
  { source :: Source
  , list :: Paged Status
  , now :: Maybe Instant
  , revealed :: Set StatusId
  , busy :: Set StatusId
  , notice :: Maybe String
  -- | Only for a hashtag feed: the tag as the server describes it.
  , tag :: Maybe HashtagInfo
  , tagBusy :: Boolean
  }

data Action
  = Initialize
  | Receive Source
  | Refresh
  | LoadMore
  | Card StatusId CardAction
  | DismissNotice
  | Back
  | Go Route
  | ToggleTagFollow

component :: forall m. MonadAff m => MonadAsk Env m => H.Component Screen.Query Source Screen.Output m
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

initialState :: Source -> State
initialState source =
  { source
  , list: Paged.loading
  , now: Nothing
  , revealed: Set.empty
  , busy: Set.empty
  , notice: Nothing
  , tag: Nothing
  , tagBusy: false
  }

pagerFor :: Source -> Pager Status
pagerFor = case _ of
  HomeFeed -> homePager
  PublicFeed -> publicPager
  BookmarksFeed -> bookmarksPager
  LikesFeed -> favouritesPager
  TagFeed name -> tagPager name

emptyText :: Source -> String
emptyText = case _ of
  HomeFeed -> "Nothing here yet. Follow someone and their posts appear here."
  PublicFeed -> "Nothing here yet."
  BookmarksFeed -> "No bookmarks yet."
  LikesFeed -> "No likes yet."
  TagFeed _ -> "No posts with this hashtag."

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "feed" ]
    [ header state
    , noticeBar DismissNotice state.notice
    , case state.list.phase of
        FirstLoading -> statusLine "Loading\x2026"
        FirstFailed err -> errorBox Refresh err
        _ ->
          HH.div_
            [ if Array.null state.list.items then statusLine (emptyText state.source)
              else HH.div [ cls "cards" ] (map (card state) state.list.items)
            , pagedFooter LoadMore "You're all caught up." state.list
            ]
    ]

header :: forall m. State -> H.ComponentHTML Action () m
header state = case state.source of
  HomeFeed -> timelineHeader true
  PublicFeed -> timelineHeader false
  BookmarksFeed -> screenHeader Back "Bookmarks" [ refresh ]
  LikesFeed -> screenHeader Back "Likes" [ refresh ]
  TagFeed name ->
    HH.div_
      [ screenHeader Back ("#" <> name) [ refresh ]
      , HH.div [ cls "tag-head" ]
          [ HH.span [ cls "muted" ]
              [ HH.text case state.tag >>= _.count of
                  Just n -> show n <> if n == 1 then " post" else " posts"
                  Nothing -> ""
              ]
          , case state.tag of
              Nothing -> HH.text ""
              Just tag ->
                HH.button
                  [ cls (if tag.following then "follow follow-secondary" else "follow")
                  , HP.type_ HP.ButtonButton
                  , HP.disabled state.tagBusy
                  , HE.onClick \_ -> ToggleTagFollow
                  ]
                  [ HH.text (if tag.following then "Following" else "Follow") ]
          ]
      ]
  where
  refresh = iconButton "ghost" "Refresh" "\x21BB" Refresh

  timelineHeader home =
    HH.header [ cls "feed-header" ]
      [ HH.div [ cls "segmented" ]
          [ segment home "Home" Home
          , segment (not home) "Public" Public
          ]
      , HH.div [ cls "screen-header-right" ]
          [ iconButton "ghost" "Messages" "\x2709" (Go Conversations)
          , refresh
          ]
      ]

  segment active label route =
    HH.button
      [ cls (if active then "segment segment-active" else "segment")
      , HP.type_ HP.ButtonButton
      , HE.onClick \_ -> Go route
      ]
      [ HH.text label ]

card :: forall m. State -> Status -> H.ComponentHTML Action () m
card state status =
  let
    id = statusId (displayed status)
  in
    statusCard
      { now: state.now
      , revealed: Set.member id state.revealed
      , busy: Set.member id state.busy
      , commentsOpenDetail: true
      }
      (Card id)
      status

handleQuery :: forall a m. MonadAff m => MonadAsk Env m => Screen.Query a -> H.HalogenM State Action () Screen.Output m (Maybe a)
handleQuery = case _ of
  Screen.NearBottom next -> do
    loadMore
    pure (Just next)
  Screen.StatusRemoved id next -> do
    H.modify_ \s -> s { list = Paged.modifyItems (Ops.removeStatus id) s.list }
    pure (Just next)
  Screen.StatusUpdated status next -> do
    H.modify_ \s -> s { list = Paged.modifyItems (Ops.replaceAll (statusId status) status) s.list }
    pure (Just next)
  Screen.AccountUpdated _ next -> pure (Just next)

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Receive source -> do
    current <- H.gets _.source
    when (source /= current) do
      H.modify_ \s -> (initialState source) { now = s.now }
      load

  Refresh -> do
    H.modify_ _ { list = Paged.loading, notice = Nothing }
    load

  LoadMore -> loadMore

  DismissNotice -> H.modify_ _ { notice = Nothing }

  Back -> liftEffect goBack

  Go route -> liftEffect (navigate route)

  ToggleTagFollow -> do
    state <- H.get
    case state.source, state.tag of
      TagFeed name, Just tag | not state.tagBusy -> do
        backend <- asks _.backend
        H.modify_ _ { tagBusy = true }
        result <- liftAff ((if tag.following then unfollowTag else followTag) backend name)
        H.modify_ \s -> case result of
          Left err -> s { tagBusy = false, notice = Just (printAppError err) }
          -- The follow answer carries no count, so the one held is kept.
          Right answer -> s { tagBusy = false, tag = Just tag { following = answer.following } }
      _, _ -> pure unit

  Card id action -> case handleCardNavigation action of
    Just go -> liftEffect go
    Nothing -> case action of
      Reveal -> H.modify_ \s -> s { revealed = Set.insert id s.revealed }
      ToggleFavourite -> toggle id Ops.ToggleFavourite
      ToggleBookmark -> toggle id Ops.ToggleBookmark
      ToggleShare -> toggle id Ops.ToggleShare
      _ -> pure unit

-- | Fetches the first chunk, and for a hashtag feed the tag itself beside
-- | it. An answer for a source the screen has since left is dropped.
load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  source <- H.gets _.source
  stamp <- liftEffect now
  case source of
    TagFeed name -> void $ H.fork do
      tag <- liftAff (getTag backend name)
      H.modify_ \s -> if s.source /= source then s else s { tag = either' tag }
    _ -> pure unit
  result <- liftAff (runPager (pagerFor source) backend)
  H.modify_ \s ->
    if s.source /= source then s
    else s { now = Just stamp, list = Paged.first result }
  where
  either' = case _ of
    Right tag -> Just tag
    Left _ -> Nothing

loadMore :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
loadMore = do
  state <- H.get
  case Paged.wantsMore state.list of
    Nothing -> pure unit
    Just pager -> do
      backend <- asks _.backend
      H.modify_ \s -> s { list = Paged.startMore s.list }
      result <- liftAff (runPager pager backend)
      stamp <- liftEffect now
      H.modify_ \s ->
        if s.source /= state.source then s
        else s { now = Just stamp, list = Paged.more statusId result s.list }

-- | Flips a like, bookmark or share on the displayed status with the given
-- | id, marking it busy meanwhile and merging the server's answer back.
toggle :: forall m. MonadAff m => MonadAsk Env m => StatusId -> Toggle -> H.HalogenM State Action () Screen.Output m Unit
toggle id which = do
  state <- H.get
  case Array.find (\st -> statusId (displayed st) == id) state.list.items of
    Just target | not (Set.member id state.busy) -> do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (Ops.request which (displayed target) backend id)
      case result of
        Left err -> H.modify_ \s -> s { busy = Set.delete id s.busy, notice = Just (printAppError err) }
        Right fromServer -> do
          let merged = Ops.merge which (displayed target) fromServer
          H.modify_ \s -> s
            { busy = Set.delete id s.busy
            , list = Paged.modifyItems (Ops.replaceAll id merged) s.list
            }
          -- Screens underneath that show the same post take the new copy.
          H.raise (Screen.StatusChanged merged)
    _ -> pure unit
