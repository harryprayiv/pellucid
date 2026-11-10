-- | The Explore tab: a search box over accounts and hashtags, and below it,
-- | while nothing is being searched for, what the instance says is trending.
-- | Pixelfed's search endpoint never returns posts, so there is no posts tab.
module Component.Explore (component) where

import Prelude

import App.Backend (AppError, printAppError)
import App.Loadable (Loadable(..), fromEither)
import App.Monad (Env)
import App.Route (Route(..), navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (TrendingRange(..), popularAccounts, search, trendingPosts, trendingTags)
import Pixelfed.Entity (Account, SearchResults, Status, TrendingTag)
import View.Common (accountRow, avatar, cls, handleName, iconButton, statusLine)
import View.Tile (postGrid)
import Web.Event.Event (Event, preventDefault)

-- | The server validates `q` as 1 to 100 characters.
maxQueryLength :: Int
maxQueryLength = 100

data Search
  = NotSearching
  | Searching String
  | Found String SearchResults
  | SearchFailed String AppError

type State =
  { query :: String
  , search :: Search
  , tags :: Loadable (Array TrendingTag)
  , accounts :: Loadable (Array Account)
  , posts :: Loadable (Array Status)
  , range :: TrendingRange
  }

data Action
  = Initialize
  | Reload
  | SetQuery String
  | Submit Event
  | ClearSearch
  | SetRange TrendingRange
  | Open Route

component :: forall i m. MonadAff m => MonadAsk Env m => H.Component Screen.Query i Screen.Output m
component = H.mkComponent
  { initialState: \_ ->
      { query: ""
      , search: NotSearching
      , tags: Pending
      , accounts: Pending
      , posts: Pending
      , range: Daily
      }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "explore" ]
    [ HH.header [ cls "feed-header" ]
        [ HH.h1_ [ HH.text "Explore" ]
        , iconButton "ghost" "Reload" "\x21BB" Reload
        ]
    , HH.form [ cls "search-form", HE.onSubmit Submit ]
        [ HH.input
            [ HP.type_ HP.InputSearch
            , HP.value state.query
            , HP.placeholder "Search people and hashtags"
            , HP.attr (HH.AttrName "maxlength") (show maxQueryLength)
            , HP.attr (HH.AttrName "autocapitalize") "none"
            , HE.onValueInput SetQuery
            ]
        , HH.button [ HP.type_ HP.ButtonSubmit, HP.disabled (trim state.query == "" || isSearching state.search) ]
            [ HH.text "Search" ]
        ]
    , case state.search of
        NotSearching -> discover state
        Searching term -> statusLine ("Searching for " <> term <> "\x2026")
        SearchFailed _ err ->
          HH.div [ cls "error-box" ]
            [ HH.p_ [ HH.text (printAppError err) ]
            , HH.button [ cls "secondary", HP.type_ HP.ButtonButton, HE.onClick \_ -> ClearSearch ] [ HH.text "Clear" ]
            ]
        Found term results -> found term results
    ]

isSearching :: Search -> Boolean
isSearching = case _ of
  Searching _ -> true
  _ -> false

found :: forall m. String -> SearchResults -> H.ComponentHTML Action () m
found term results =
  HH.div [ cls "search-results" ]
    [ HH.div [ cls "section-head" ]
        [ HH.h2_ [ HH.text ("Results for " <> term) ]
        , HH.button [ cls "link", HP.type_ HP.ButtonButton, HE.onClick \_ -> ClearSearch ] [ HH.text "Clear" ]
        ]
    , if Array.null results.accounts && Array.null results.hashtags then statusLine "Nothing found."
      else HH.text ""
    , if Array.null results.accounts then HH.text ""
      else
        HH.div_
          [ HH.h3_ [ HH.text "People" ]
          , HH.div [ cls "account-rows" ] (map (\a -> accountRow (Open (Profile a.id)) a []) results.accounts)
          ]
    , if Array.null results.hashtags then HH.text ""
      else
        HH.div_
          [ HH.h3_ [ HH.text "Hashtags" ]
          , HH.div [ cls "tag-rows" ] (map tagRow results.hashtags)
          ]
    ]
  where
  tagRow tag =
    HH.button [ cls "tag-row", HP.type_ HP.ButtonButton, HE.onClick \_ -> Open (Tag tag.name) ]
      [ HH.strong_ [ HH.text ("#" <> tag.name) ]
      , HH.span [ cls "muted" ]
          [ HH.text case tag.count of
              Just n -> show n <> if n == 1 then " post" else " posts"
              Nothing -> ""
          ]
      ]

discover :: forall m. State -> H.ComponentHTML Action () m
discover state =
  HH.div [ cls "discover" ]
    [ section "Trending hashtags" state.tags \tags ->
        if Array.null tags then statusLine "Nothing trending right now."
        else HH.div [ cls "chips" ] (map chip tags)
    , section "Popular accounts" state.accounts \accounts ->
        if Array.null accounts then statusLine "No suggestions right now."
        else HH.div [ cls "people-strip" ] (map person accounts)
    , HH.div [ cls "section-head" ]
        [ HH.h2_ [ HH.text "Trending posts" ]
        , HH.div [ cls "segmented" ]
            [ rangeButton Daily "Today"
            , rangeButton Monthly "Month"
            , rangeButton Yearly "Year"
            ]
        ]
    , case state.posts of
        Pending -> statusLine "Loading\x2026"
        Missing err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
        Ready [] -> statusLine "Nothing trending in this range."
        Ready posts -> postGrid (Open <<< StatusDetail) posts
    ]
  where
  section :: forall a. String -> Loadable a -> (a -> H.ComponentHTML Action () m) -> H.ComponentHTML Action () m
  section title loadable body =
    HH.div_
      [ HH.div [ cls "section-head" ] [ HH.h2_ [ HH.text title ] ]
      , case loadable of
          Pending -> statusLine "Loading\x2026"
          Missing err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
          Ready a -> body a
      ]

  chip tag =
    HH.button [ cls "chip", HP.type_ HP.ButtonButton, HE.onClick \_ -> Open (Tag tag.name) ]
      [ HH.text ("#" <> tag.name) ]

  person account =
    HH.button [ cls "person", HP.type_ HP.ButtonButton, HE.onClick \_ -> Open (Profile account.id) ]
      [ avatar "avatar avatar-large" account.avatar
      , HH.span [ cls "person-name" ] [ HH.text (handleName account.displayName account.username) ]
      ]

  rangeButton range label =
    HH.button
      [ cls (if state.range == range then "segment segment-active" else "segment")
      , HP.type_ HP.ButtonButton
      , HE.onClick \_ -> SetRange range
      ]
      [ HH.text label ]

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> loadDiscover

  Reload -> do
    H.modify_ _ { tags = Pending, accounts = Pending, posts = Pending }
    loadDiscover

  SetQuery text -> H.modify_ _ { query = String.take maxQueryLength text }

  ClearSearch -> H.modify_ _ { search = NotSearching, query = "" }

  Open route -> liftEffect (navigate route)

  SetRange range -> do
    current <- H.gets _.range
    when (range /= current) do
      backend <- asks _.backend
      H.modify_ _ { range = range, posts = Pending }
      result <- liftAff (trendingPosts backend range)
      -- Dropped when the person has picked another range meanwhile.
      H.modify_ \s -> if s.range /= range then s else s { posts = fromEither result }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    let term = trim state.query
    when (term /= "" && not (isSearching state.search)) do
      backend <- asks _.backend
      H.modify_ _ { search = Searching term }
      result <- liftAff (search backend term)
      H.modify_ \s -> case s.search of
        -- Cleared while the request was out: the answer is not wanted.
        Searching waiting | waiting == term -> s
          { search = case result of
              Left err -> SearchFailed term err
              Right results -> Found term results
          }
        _ -> s

loadDiscover :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
loadDiscover = do
  backend <- asks _.backend
  range <- H.gets _.range
  answers <- liftAff $ sequential $
    { tags: _, accounts: _, posts: _ }
      <$> parallel (trendingTags backend)
      <*> parallel (popularAccounts backend)
      <*> parallel (trendingPosts backend range)
  H.modify_ \s -> s
    { tags = fromEither answers.tags
    , accounts = fromEither answers.accounts
    , posts = if s.range /= range then s.posts else fromEither answers.posts
    }
