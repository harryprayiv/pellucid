-- | The hashtags the viewer follows. A row opens the tag's feed; its button
-- | stops following and takes the row out.
module Component.FollowedTags (component) where

import Prelude

import App.Backend (printAppError)
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
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (followedTagsPager, runPager, unfollowTag)
import Pixelfed.Entity (HashtagInfo, tagKey)
import View.Common (cls, errorBox, iconButton, noticeBar, pagedFooter, screenHeader, statusLine)

type State =
  { list :: Paged HashtagInfo
  , busy :: Set String
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Reload
  | LoadMore
  | Back
  | Open String
  | Unfollow String
  | DismissNotice

component :: forall i m. MonadAff m => MonadAsk Env m => H.Component Screen.Query i Screen.Output m
component = H.mkComponent
  { initialState: \_ -> { list: Paged.loading, busy: Set.empty, notice: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "followed-tags" ]
    [ screenHeader Back "Followed hashtags" [ iconButton "ghost" "Reload" "\x21BB" Reload ]
    , noticeBar DismissNotice state.notice
    , case state.list.phase of
        FirstLoading -> statusLine "Loading\x2026"
        FirstFailed err -> errorBox Reload err
        _ ->
          HH.div_
            [ if Array.null state.list.items then statusLine "You do not follow any hashtags. Open one and press Follow."
              else HH.div [ cls "tag-rows" ] (map row state.list.items)
            , pagedFooter LoadMore "" state.list
            ]
    ]
  where
  row tag =
    HH.div [ cls "account-row" ]
      [ HH.button [ cls "account-main", HP.type_ HP.ButtonButton, HE.onClick \_ -> Open tag.name ]
          [ HH.strong_ [ HH.text ("#" <> tag.name) ] ]
      , HH.div [ cls "account-actions" ]
          [ HH.button
              [ cls "small secondary"
              , HP.type_ HP.ButtonButton
              , HP.disabled (Set.member (tagKey tag.name) state.busy)
              , HE.onClick \_ -> Unfollow tag.name
              ]
              [ HH.text "Unfollow" ]
          ]
      ]

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

  Reload -> do
    H.modify_ _ { list = Paged.loading, notice = Nothing }
    load

  LoadMore -> loadMore

  Back -> liftEffect goBack

  Open name -> liftEffect (navigate (Tag name))

  DismissNotice -> H.modify_ _ { notice = Nothing }

  Unfollow name -> do
    let key = tagKey name
    busy <- H.gets _.busy
    when (not (Set.member key busy)) do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert key s.busy }
      result <- liftAff (unfollowTag backend name)
      H.modify_ \s -> case result of
        Left err -> s { busy = Set.delete key s.busy, notice = Just (printAppError err) }
        Right _ -> s
          { busy = Set.delete key s.busy
          , list = Paged.modifyItems (Array.filter (\t -> tagKey t.name /= key)) s.list
          }

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  result <- liftAff (runPager followedTagsPager backend)
  H.modify_ _ { list = Paged.first result }

loadMore :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
loadMore = do
  state <- H.get
  case Paged.wantsMore state.list of
    Nothing -> pure unit
    Just pager -> do
      backend <- asks _.backend
      H.modify_ \s -> s { list = Paged.startMore s.list }
      result <- liftAff (runPager pager backend)
      H.modify_ \s -> s { list = Paged.more (tagKey <<< _.name) result s.list }
