-- | One post with its comments and a reply box. Comments are the context
-- | endpoint's descendants, shown flat in the order the server gives them.
-- | The viewer's own post can be edited, and deleted after a second tap;
-- | anyone else's can be reported.
module Component.StatusDetail
  ( Input
  , component
  ) where

import Prelude

import App.Backend (printAppError)
import App.Loadable (Loadable(..), fromEither)
import App.Monad (Env)
import App.Route (Route(..), goBack, navigate)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (deleteStatus, getContext, getStatus, postReply)
import Pixelfed.Entity (AccountId, Status(..), StatusId, captionOf, displayed, statusId)
import Pixelfed.StatusOps (Toggle)
import Pixelfed.StatusOps as Ops
import View.Common (agoLabel, avatar, cls, errorBox, handleName, iconButton, noticeBar, screenHeader, statusLine)
import View.StatusCard (CardAction(..), captionView, handleCardNavigation, statusCard)
import Web.Event.Event (Event, preventDefault)

type Input = { id :: StatusId, me :: Maybe AccountId }

type Reply = { text :: String, sending :: Boolean, error :: Maybe String }

-- | Deleting the post takes two taps: the first arms, the second sends.
data Deletion = NotArmed | Armed | Deleting

type State =
  { input :: Input
  , main :: Loadable Status
  , comments :: Loadable (Array Status)
  , now :: Maybe Instant
  , revealed :: Set StatusId
  , busy :: Set StatusId
  , reply :: Reply
  , deletion :: Deletion
  , notice :: Maybe String
  }

data Action
  = Initialize
  | Receive Input
  | Reload
  | Back
  | Card StatusId CardAction
  | SetReply String
  | SubmitReply Event
  | DeleteComment StatusId
  | ArmDelete
  | CancelDelete
  | ConfirmDelete
  | Go Route
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
  , main: Pending
  , comments: Pending
  , now: Nothing
  , revealed: Set.empty
  , busy: Set.empty
  , reply: { text: "", sending: false, error: Nothing }
  , deletion: NotArmed
  , notice: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "detail" ]
    [ screenHeader Back "Post" (headerActions state <> [ iconButton "ghost" "Reload" "\x21BB" Reload ])
    , deletePrompt state
    , noticeBar DismissNotice state.notice
    , case state.main of
        Pending -> statusLine "Loading\x2026"
        Missing err -> errorBox Reload err
        Ready status ->
          let
            id = statusId (displayed status)
            Status shown = displayed status
          in
            HH.div_
              [ statusCard
                  { now: state.now
                  , revealed: Set.member id state.revealed
                  , busy: Set.member id state.busy
                  , commentsOpenDetail: false
                  }
                  (Card id)
                  status
              , HH.section [ cls "comments" ]
                  [ HH.h2_ [ HH.text "Comments" ]
                  , case state.comments of
                      Pending -> statusLine "Loading comments\x2026"
                      Missing err -> HH.p [ cls "error" ] [ HH.text (printAppError err) ]
                      Ready [] -> statusLine "No comments yet."
                      Ready comments -> HH.div [ cls "comment-list" ] (map (commentRow state) comments)
                  , if shown.commentsDisabled then HH.p [ cls "muted" ] [ HH.text "Comments are off for this post." ]
                    else replyForm state.reply
                  ]
              ]
    ]

-- | The viewer's own post can be edited and deleted; anyone else's can be
-- | reported. Nothing is offered until the post has loaded.
headerActions :: forall m. State -> Array (H.ComponentHTML Action () m)
headerActions state = case state.main of
  Ready status ->
    let
      id = statusId (displayed status)
    in
      if isMine state then
        [ iconButton "ghost" "Edit post" "\x270E" (Go (EditPost id))
        , iconButton "ghost" "Delete post" "\x1F5D1" ArmDelete
        ]
      else [ iconButton "ghost" "Report post" "\x2691" (Go (ReportPost id)) ]
  _ -> []

-- | True when the main post (the displayed one, for a share) is the viewer's.
isMine :: State -> Boolean
isMine state = case state.main of
  Ready st ->
    let
      Status shown = displayed st
    in
      state.input.me == Just shown.account.id
  _ -> false

deletePrompt :: forall m. State -> H.ComponentHTML Action () m
deletePrompt state = case state.deletion of
  NotArmed -> HH.text ""
  Armed ->
    HH.div [ cls "confirm" ]
      [ HH.span_ [ HH.text "Delete this post? This cannot be undone." ]
      , HH.button [ cls "confirm-yes", HP.type_ HP.ButtonButton, HE.onClick \_ -> ConfirmDelete ] [ HH.text "Delete" ]
      , HH.button [ cls "ghost", HP.type_ HP.ButtonButton, HE.onClick \_ -> CancelDelete ] [ HH.text "Keep" ]
      ]
  Deleting -> HH.div [ cls "confirm" ] [ HH.span_ [ HH.text "Deleting\x2026" ] ]

commentRow :: forall m. State -> Status -> H.ComponentHTML Action () m
commentRow state comment =
  let
    Status c = comment
    mine = state.input.me == Just c.account.id
  in
    HH.article [ cls "comment" ]
      [ HH.button
          [ cls "avatar-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> Card c.id (OpenAccount c.account.id) ]
          [ avatar "avatar avatar-small" c.account.avatar ]
      , HH.div [ cls "comment-body" ]
          [ HH.div [ cls "comment-head" ]
              [ HH.strong_ [ HH.text (handleName c.account.displayName c.account.acct) ]
              , HH.span [ cls "card-time", HP.title c.createdAt ] [ HH.text (agoLabel state.now c.createdAt) ]
              ]
          , HH.p [ cls "comment-text" ] (captionView (Card c.id) c.mentions (captionOf c))
          , HH.div [ cls "comment-actions" ]
              [ HH.button
                  [ cls ("action action-small" <> if c.favourited then " action-active" else "")
                  , HP.type_ HP.ButtonButton
                  , HP.disabled (Set.member c.id state.busy)
                  , HP.title (if c.favourited then "Unlike" else "Like")
                  , HE.onClick \_ -> Card c.id ToggleFavourite
                  ]
                  [ HH.text (if c.favourited then "\x2665" else "\x2661")
                  , if c.favouritesCount > 0 then HH.span [ cls "count" ] [ HH.text (" " <> show c.favouritesCount) ]
                    else HH.text ""
                  ]
              , if mine then
                  HH.button
                    [ cls "action action-small"
                    , HP.type_ HP.ButtonButton
                    , HP.disabled (Set.member c.id state.busy)
                    , HP.title "Delete comment"
                    , HE.onClick \_ -> DeleteComment c.id
                    ]
                    [ HH.text "\x1F5D1" ]
                else HH.text ""
              ]
          ]
      ]

replyForm :: forall m. Reply -> H.ComponentHTML Action () m
replyForm reply =
  HH.form [ cls "reply-form", HE.onSubmit SubmitReply ]
    [ HH.textarea
        [ HP.value reply.text
        , HP.placeholder "Add a comment"
        , HP.rows 2
        , HP.disabled reply.sending
        , HE.onValueInput SetReply
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit, HP.disabled (reply.sending || trim reply.text == "") ]
        [ HH.text (if reply.sending then "Posting\x2026" else "Post") ]
    , case reply.error of
        Just err -> HH.p [ cls "error" ] [ HH.text err ]
        Nothing -> HH.text ""
    ]

handleQuery :: forall a m. Screen.Query a -> H.HalogenM State Action () Screen.Output m (Maybe a)
handleQuery = case _ of
  Screen.NearBottom next -> pure (Just next)
  Screen.StatusRemoved _ next -> pure (Just next)
  Screen.StatusUpdated status next -> do
    let id = statusId status
    H.modify_ \s -> s
      { main = case s.main of
          Ready m -> Ready (Ops.replaceDisplayed id status m)
          other -> other
      , comments = case s.comments of
          Ready cs -> Ready (Ops.replaceAll id status cs)
          other -> other
      }
    pure (Just next)
  Screen.AccountUpdated _ next -> pure (Just next)

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Initialize -> load

  Go route -> liftEffect (navigate route)

  Receive input -> do
    current <- H.gets _.input
    when (input /= current) do
      H.put (initialState input)
      load

  Reload -> do
    H.modify_ _ { main = Pending, comments = Pending, deletion = NotArmed, notice = Nothing }
    load

  Back -> liftEffect goBack

  DismissNotice -> H.modify_ _ { notice = Nothing }

  Card id action -> case action of
    -- Already on the detail screen; the comments are below.
    OpenStatus _ -> pure unit
    Reveal -> H.modify_ \s -> s { revealed = Set.insert id s.revealed }
    ToggleFavourite -> toggle id Ops.ToggleFavourite
    ToggleBookmark -> toggle id Ops.ToggleBookmark
    ToggleShare -> toggle id Ops.ToggleShare
    other -> case handleCardNavigation other of
      Just go -> liftEffect go
      Nothing -> pure unit

  SetReply text -> H.modify_ \s -> s { reply = s.reply { text = text } }

  SubmitReply event -> do
    liftEffect (preventDefault event)
    state <- H.get
    let text = trim state.reply.text
    case state.main of
      Ready main | text /= "" && not state.reply.sending -> do
        backend <- asks _.backend
        H.modify_ \s -> s { reply = s.reply { sending = true, error = Nothing } }
        result <- liftAff (postReply backend (statusId (displayed main)) text)
        H.modify_ \s -> case result of
          Left err -> s { reply = s.reply { sending = false, error = Just (printAppError err) } }
          Right comment -> s
            { reply = { text: "", sending: false, error: Nothing }
            , comments = case s.comments of
                Ready cs -> Ready (Array.snoc cs comment)
                other -> other
            , main = adjustReplies 1 s.main
            }
        when (isRight result) announceMain
      _ -> pure unit

  ArmDelete -> do
    state <- H.get
    when (isMine state) case state.deletion of
      NotArmed -> H.modify_ _ { deletion = Armed }
      _ -> pure unit

  CancelDelete -> H.modify_ \s -> case s.deletion of
    Armed -> s { deletion = NotArmed }
    _ -> s

  ConfirmDelete -> do
    state <- H.get
    case state.main, state.deletion of
      Ready main, Armed | isMine state -> do
        backend <- asks _.backend
        H.modify_ _ { deletion = Deleting }
        result <- liftAff (deleteStatus backend (statusId (displayed main)))
        case result of
          Left err -> H.modify_ _ { deletion = NotArmed, notice = Just (printAppError err) }
          -- The post is gone. Every screen still holding it is told, then
          -- this one leaves, since it has nothing left to show.
          Right _ -> do
            H.raise (Screen.StatusDeleted (statusId (displayed main)))
            liftEffect goBack
      _, _ -> pure unit

  DeleteComment id -> do
    state <- H.get
    when (not (Set.member id state.busy)) do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (deleteStatus backend id)
      H.modify_ \s -> case result of
        Left err -> s { busy = Set.delete id s.busy, notice = Just (printAppError err) }
        Right _ -> s
          { busy = Set.delete id s.busy
          , comments = case s.comments of
              Ready cs -> Ready (Array.filter (\c -> statusId c /= id) cs)
              other -> other
          , main = adjustReplies (-1) s.main
          }
      when (isRight result) announceMain

-- | Tells the other screens the main post's comment count moved.
announceMain :: forall m. H.HalogenM State Action () Screen.Output m Unit
announceMain = do
  main <- H.gets _.main
  case main of
    Ready st -> H.raise (Screen.StatusChanged (displayed st))
    _ -> pure unit

adjustReplies :: Int -> Loadable Status -> Loadable Status
adjustReplies delta = case _ of
  Ready st -> Ready (bump st)
  other -> other
  where
  bump (Status top) = case top.reblog of
    Just (Status inner) -> Status top { reblog = Just (Status inner { replyCount = max 0 (inner.replyCount + delta) }) }
    Nothing -> Status top { replyCount = max 0 (top.replyCount + delta) }

load :: forall m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () Screen.Output m Unit
load = do
  backend <- asks _.backend
  id <- H.gets _.input.id
  stamp <- liftEffect now
  Tuple main context <- liftAff $ sequential $
    Tuple <$> parallel (getStatus backend id) <*> parallel (getContext backend id)
  H.modify_ \s -> s
    { now = Just stamp
    , main = fromEither main
    , comments = fromEither (map _.descendants context)
    }

-- | Flips a like or bookmark on the main post or on a comment.
toggle :: forall m. MonadAff m => MonadAsk Env m => StatusId -> Toggle -> H.HalogenM State Action () Screen.Output m Unit
toggle id which = do
  state <- H.get
  let
    target = case state.main of
      Ready st | statusId (displayed st) == id -> Just (displayed st)
      _ -> case state.comments of
        Ready cs -> Array.find (\c -> statusId c == id) cs
        _ -> Nothing
  case target of
    Just st | not (Set.member id state.busy) -> do
      backend <- asks _.backend
      H.modify_ \s -> s { busy = Set.insert id s.busy }
      result <- liftAff (Ops.request which st backend id)
      case result of
        Left err -> H.modify_ \s -> s { busy = Set.delete id s.busy, notice = Just (printAppError err) }
        Right fromServer -> do
          let merged = Ops.merge which st fromServer
          H.modify_ \s -> s
            { busy = Set.delete id s.busy
            , main = case s.main of
                Ready m -> Ready (Ops.replaceDisplayed id merged m)
                other -> other
            , comments = case s.comments of
                Ready cs -> Ready (Ops.replaceAll id merged cs)
                other -> other
            }
          -- Screens underneath that show the same post take the new copy.
          H.raise (Screen.StatusChanged merged)
    _ -> pure unit
