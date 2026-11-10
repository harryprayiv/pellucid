-- | A list of accounts: who follows someone, whom they follow, who liked or
-- | shared a post, and the viewer's own muted, blocked and requesting
-- | accounts. The last three carry an action on each row.
module Component.AccountList
  ( Kind(..)
  , component
  ) where

import Prelude

import App.Backend (AppError, Backend, printAppError)
import App.Monad (Env)
import App.Paged (Paged, Phase(..))
import App.Paged as Paged
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (Pager, authorizeFollowRequest, blocksPager, followRequests, followersPager, followingPager, likedByPager, mutesPager, rejectFollowRequest, runPager, sharedByPager, unblock, unmute)
import Pixelfed.Entity (Account, AccountId, Relationship, StatusId)
import View.Common (accountRow, cls, errorBox, iconButton, noticeBar, pagedFooter, screenHeader, statusLine)

data Kind
  = FollowersOf AccountId
  | FollowingOf AccountId
  | LikedByOf StatusId
  | SharedByOf StatusId
  | MutedAccounts
  | BlockedAccounts
  | PendingRequests

derive instance Eq Kind

-- | What a row's button does. Each one, on success, takes the row out of
-- | the list.
data RowAction = Unmute | Unblock | Accept | Reject

type State =
  { kind :: Kind
  , list :: Paged Account
  , busy :: Set AccountId
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Receive Kind
  | Reload
  | LoadMore
  | Back
  | Open AccountId
  | Act RowAction AccountId
  | DismissNotice

component :: forall m. MonadAff m => MonadAsk Env m => H.Component Screen.Query Kind Screen.Output m
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

initialState :: Kind -> State
initialState kind = { kind, list: Paged.loading, busy: Set.empty, notice: Nothing }

pagerFor :: Kind -> Pager Account
pagerFor = case _ of
  FollowersOf id -> followersPager id
  FollowingOf id -> followingPager id
  LikedByOf id -> likedByPager id
  SharedByOf id -> sharedByPager id
  MutedAccounts -> mutesPager
  BlockedAccounts -> blocksPager
  PendingRequests -> followRequests

title :: Kind -> String
title = case _ of
  FollowersOf _ -> "Followers"
  FollowingOf _ -> "Following"
  LikedByOf _ -> "Likes"
  SharedByOf _ -> "Shares"
  MutedAccounts -> "Muted accounts"
  BlockedAccounts -> "Blocked accounts"
  PendingRequests -> "Follow requests"

emptyText :: Kind -> String
emptyText = case _ of
  FollowersOf _ -> "No followers to show."
  FollowingOf _ -> "Not following anyone, or the list is hidden."
  LikedByOf _ -> "No likes yet."
  SharedByOf _ -> "No shares yet."
  MutedAccounts -> "You have not muted anyone."
  BlockedAccounts -> "You have not blocked anyone."
  PendingRequests -> "No follow requests."

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "account-list" ]
    [ screenHeader Back (title state.kind) [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , noticeBar DismissNotice state.notice
    , case state.list.phase of
        FirstLoading -> statusLine "Loading\x2026"
        FirstFailed err -> errorBox Reload err
        _ ->
          HH.div_
            [ if Array.null state.list.items then statusLine (emptyText state.kind)
              else HH.div [ cls "account-rows" ] (map (row state) state.list.items)
            , pagedFooter LoadMore "That's everyone." state.list
            ]
    ]

row :: forall m. State -> Account -> H.ComponentHTML Action () m
row state account = accountRow (Open account.id) account case state.kind of
  MutedAccounts -> [ button "Unmute" Unmute ]
  BlockedAccounts -> [ button "Unblock" Unblock ]
  PendingRequests -> [ button "Accept" Accept, button "Reject" Reject ]
  _ -> []
  where
  button label action =
    HH.button
      [ cls (if isPrimary action then "small" else "small secondary")
      , HP.type_ HP.ButtonButton
      , HP.disabled (Set.member account.id state.busy)
      , HE.onClick \_ -> Act action account.id
      ]
      [ HH.text label ]

  isPrimary = case _ of
    Accept -> true
    _ -> false

handleQuery :: forall a m. MonadAff m => MonadAsk Env m => Screen.Query a -> H.HalogenM State Action () Screen.Output m (Maybe a)
handleQuery = case _ of
  Screen.NearBottom next -> do
    loadMore
    pure (Just next)
  Screen.StatusRemoved _ next -> pure (Just next)
  Screen.StatusUpdated _ next -> pure (Just next)
  Screen.AccountUpdated _ next -> pure (Just next)

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Receive kind -> do
    current <- H.gets _.kind
    when (kind /= current) do
      H.put (initialState kind)
      load

  Reload -> do
    H.modify_ \s -> initialState s.kind
    load

  LoadMore -> loadMore

  Back -> liftEffect goBack

  Open id -> liftEffect (navigate (Profile id))

  DismissNotice -> H.modify_ _ { notice = Nothing }

  Act action id -> do
    busy <- H.gets _.busy
    when (not (Set.member id busy)) do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (perform action backend id)
      H.modify_ \s -> case result of
        Left err -> s { busy = Set.delete id s.busy, notice = Just (printAppError err) }
        Right _ -> s
          { busy = Set.delete id s.busy
          , list = Paged.modifyItems (Array.filter (\a -> a.id /= id)) s.list
          }

perform :: RowAction -> Backend -> AccountId -> Aff (Either AppError Relationship)
perform = case _ of
  Unmute -> unmute
  Unblock -> unblock
  Accept -> authorizeFollowRequest
  Reject -> rejectFollowRequest

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  kind <- H.gets _.kind
  result <- liftAff (runPager (pagerFor kind) backend)
  H.modify_ \s -> if s.kind /= kind then s else s { list = Paged.first result }

loadMore :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
loadMore = do
  state <- H.get
  case Paged.wantsMore state.list of
    Nothing -> pure unit
    Just pager -> do
      backend <- asks _.backend
      H.modify_ \s -> s { list = Paged.startMore s.list }
      result <- liftAff (runPager pager backend)
      H.modify_ \s -> if s.kind /= state.kind then s else s { list = Paged.more _.id result s.list }
