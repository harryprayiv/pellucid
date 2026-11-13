-- | A new post. Files upload as soon as they are picked (POST /api/v2/media
-- | through the shell). Pressing Post saves each attachment's alt text and
-- | then creates the status with the media ids, in that order and from that
-- | one place, so no description is still on its way when the status is
-- | made. Pixelfed refuses a status with neither media nor a parent, so Post
-- | stays disabled until every upload is done.
module Component.Compose (component) where

import Prelude

import App.Backend (printAppError)
import App.Monad (Env)
import App.Route (Route(StatusDetail), goBack, navigate)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Either (Either(..), isRight)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (composeSettings, createPost, updateMediaDescription, uploadMedia)
import Pixelfed.Decode (decodeVisibility)
import Pixelfed.Entity (ComposeSettings, MediaAttachment, MediaId, Visibility(..), statusId, visibilityName)
import Util.File (mimeOf, readBytes, takeFiles)
import View.Common (cls, iconButton, screenHeader)
import Web.Event.Event (Event, preventDefault, target)
import Web.File.File (File)
import Web.File.File as File
import Web.HTML.HTMLInputElement as Input

-- | One picked file and what has become of it.
type Item =
  { key :: Int
  , name :: String
  , mime :: String
  , sizeBytes :: Number
  , phase :: ItemPhase
  , altText :: String
  -- | The description the server currently holds for this attachment.
  , altSaved :: String
  , altError :: Maybe String
  }

data ItemPhase = Uploading | Uploaded MediaAttachment | Rejected String

type State =
  { settings :: ComposeSettings
  , items :: Array Item
  , nextKey :: Int
  , caption :: String
  , visibility :: Visibility
  , sensitive :: Boolean
  , spoilerText :: String
  , commentsDisabled :: Boolean
  , posting :: Posting
  , notice :: Maybe String
  }

data Posting = Idle | Posting | PostFailed String

data Action
  = Initialize
  | Back
  | FilesPicked Event
  | RemoveItem Int
  | SetAlt Int String
  | SetCaption String
  | SetVisibility String
  | SetSensitive Boolean
  | SetSpoiler String
  | SetCommentsDisabled Boolean
  | Submit Event
  | DismissNotice

-- | Pixelfed's own defaults (config/pixelfed.php), used until the instance
-- | answers and kept when it does not.
defaultSettings :: ComposeSettings
defaultSettings =
  { allowedMediaTypes: [ "image/jpeg", "image/jpg", "image/png", "image/gif" ]
  , maxCaptionLength: 500
  , maxFileSizeKb: 15000
  , maxMediaAttachments: 4
  , maxAltTextLength: 1000
  , defaultVisibility: Nothing
  }

-- | `spoiler_text` is validated as max:140 by `POST /api/v1/statuses`.
maxSpoilerLength :: Int
maxSpoilerLength = 140

-- | The visibility a new post starts with: the instance's default when it
-- | is one a post can have, public otherwise.
startVisibility :: ComposeSettings -> Visibility
startVisibility settings = case settings.defaultVisibility of
  Just v | v == Public || v == Unlisted || v == Private -> v
  _ -> Public

component :: forall q i o m. MonadAff m => MonadAsk Env m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

initialState :: State
initialState =
  { settings: defaultSettings
  , items: []
  , nextKey: 1
  , caption: ""
  , visibility: Public
  , sensitive: false
  , spoilerText: ""
  , commentsDisabled: false
  , posting: Idle
  , notice: Nothing
  }

-- | Everything uploaded, nothing in flight or rejected, nothing too long.
canPost :: State -> Boolean
canPost s =
  not (Array.null s.items)
    && Array.all isUploaded s.items
    && notPosting
    && String.length s.caption <= s.settings.maxCaptionLength
    && (not s.sensitive || String.length s.spoilerText <= maxSpoilerLength)
  where
  isUploaded it = case it.phase of
    Uploaded _ -> true
    _ -> false

  notPosting = case s.posting of
    Posting -> false
    _ -> true

uploadedIds :: State -> Array MediaId
uploadedIds s = Array.mapMaybe
  ( \it -> case it.phase of
      Uploaded m -> Just m.id
      _ -> Nothing
  )
  s.items

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "compose" ]
    [ screenHeader Back "New post" []
    , case state.notice of
        Nothing -> HH.text ""
        Just text ->
          HH.div [ cls "notice" ]
            [ HH.span_ [ HH.text text ], iconButton "ghost" "Dismiss" "\x2715" DismissNotice ]
    , HH.form [ cls "compose-form", HE.onSubmit Submit ]
        [ mediaSection state
        , HH.label [ cls "field" ]
            [ HH.span [ cls "field-label" ]
                [ HH.text "Caption"
                , HH.span [ cls (if String.length state.caption > state.settings.maxCaptionLength then "counter-text error" else "counter-text muted") ]
                    [ HH.text (show (String.length state.caption) <> "/" <> show state.settings.maxCaptionLength) ]
                ]
            , HH.textarea
                [ cls "compose-caption"
                , HP.value state.caption
                , HP.placeholder "Write a caption"
                , HP.rows 4
                , HP.attr (HH.AttrName "maxlength") (show state.settings.maxCaptionLength)
                , HE.onValueInput SetCaption
                ]
            ]
        , HH.label [ cls "field" ]
            [ HH.span [ cls "field-label" ] [ HH.text "Who can see this" ]
            , HH.select [ cls "compose-select", HE.onValueChange SetVisibility ]
                (map (visibilityOption state.visibility) [ Public, Unlisted, Private ])
            ]
        , HH.label [ cls "check" ]
            [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked state.sensitive, HE.onChecked SetSensitive ]
            , HH.span_ [ HH.text "Sensitive content" ]
            ]
        , if state.sensitive then
            HH.label [ cls "field" ]
              [ HH.span [ cls "field-label" ]
                  [ HH.text "Content warning"
                  , HH.span [ cls (if String.length state.spoilerText > maxSpoilerLength then "counter-text error" else "counter-text muted") ]
                      [ HH.text (show (String.length state.spoilerText) <> "/" <> show maxSpoilerLength) ]
                  ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.value state.spoilerText
                  , HP.placeholder "Why it is hidden"
                  , HP.attr (HH.AttrName "maxlength") (show maxSpoilerLength)
                  , HE.onValueInput SetSpoiler
                  ]
              ]
          else HH.text ""
        , HH.label [ cls "check" ]
            [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked state.commentsDisabled, HE.onChecked SetCommentsDisabled ]
            , HH.span_ [ HH.text "Turn off comments" ]
            ]
        , case state.posting of
            PostFailed err -> HH.p [ cls "error" ] [ HH.text err ]
            _ -> HH.text ""
        , HH.button
            [ cls "compose-submit", HP.type_ HP.ButtonSubmit, HP.disabled (not (canPost state)) ]
            [ HH.text case state.posting of
                Posting -> "Posting\x2026"
                _ -> "Post"
            ]
        , if Array.null state.items then HH.p [ cls "muted compose-hint" ] [ HH.text "Add at least one photo or video to post." ]
          else HH.text ""
        ]
    ]

visibilityOption :: forall m. Visibility -> Visibility -> H.ComponentHTML Action () m
visibilityOption current v =
  HH.option [ HP.value (visibilityName v), HP.selected (v == current) ] [ HH.text (label v) ]
  where
  label = case _ of
    Public -> "Public"
    Unlisted -> "Unlisted"
    Private -> "Followers only"
    other -> visibilityName other

mediaSection :: forall m. State -> H.ComponentHTML Action () m
mediaSection state =
  let
    room = state.settings.maxMediaAttachments - Array.length state.items
  in
    HH.div [ cls "compose-media" ]
      [ HH.div [ cls "compose-items" ] (map (itemView state.settings) state.items)
      , if room <= 0 then HH.text ""
        else
          HH.label [ cls "compose-pick" ]
            [ HH.input
                ( [ HP.type_ HP.InputFile
                  , HP.multiple (room > 1)
                  , HE.onChange FilesPicked
                  ] <> case state.settings.allowedMediaTypes of
                    [] -> []
                    types -> [ HP.attr (HH.AttrName "accept") (joinWith "," types) ]
                )
            , HH.span_
                [ HH.text
                    if Array.null state.items then "Add photos or videos"
                    else "Add more (" <> show room <> " left)"
                ]
            ]
      , HH.p [ cls "muted compose-limits" ]
          [ HH.text
              ( "Up to " <> show state.settings.maxMediaAttachments <> " files, "
                  <> megabytes state.settings.maxFileSizeKb
                  <> " MB each."
              )
          ]
      ]

itemView :: forall m. ComposeSettings -> Item -> H.ComponentHTML Action () m
itemView settings it =
  HH.div [ cls "compose-item" ]
    [ HH.div [ cls "compose-item-head" ]
        [ case it.phase of
            Uploaded m -> HH.img [ cls "compose-thumb", HP.src (fromMaybe m.url m.previewUrl), HP.alt "" ]
            Uploading -> HH.span [ cls "compose-thumb compose-thumb-blank" ] [ HH.text "\x2026" ]
            Rejected _ -> HH.span [ cls "compose-thumb compose-thumb-blank error" ] [ HH.text "!" ]
        , HH.div [ cls "compose-item-meta" ]
            [ HH.span [ cls "compose-item-name" ] [ HH.text it.name ]
            , HH.span [ cls "muted" ]
                [ HH.text case it.phase of
                    Uploading -> "Uploading\x2026"
                    Uploaded _ -> "Uploaded"
                    Rejected why -> why
                ]
            ]
        , iconButton "ghost" "Remove" "\x2715" (RemoveItem it.key)
        ]
    , case it.phase of
        Uploaded _ ->
          HH.div [ cls "compose-alt" ]
            [ HH.textarea
                [ HP.value it.altText
                , HP.placeholder "Describe this for people who cannot see it"
                , HP.rows 2
                , HP.attr (HH.AttrName "maxlength") (show settings.maxAltTextLength)
                , HE.onValueInput (SetAlt it.key)
                ]
            , HH.span [ cls "muted counter-text" ]
                [ HH.text (show (String.length it.altText) <> "/" <> show settings.maxAltTextLength) ]
            , case it.altError of
                Just err -> HH.p [ cls "error" ] [ HH.text err ]
                Nothing -> HH.text ""
            ]
        _ -> HH.text ""
    ]

-- | Kilobytes as the server counts them, shown the way Pixelfed shows them.
megabytes :: Int -> String
megabytes kb = show (kb / 1000)

handleAction :: forall o m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    backend <- asks _.backend
    result <- liftAff (composeSettings backend)
    H.modify_ \s -> case result of
      Left err -> s { notice = Just ("Using default upload limits. " <> printAppError err) }
      Right settings -> s { settings = settings, visibility = startVisibility settings }

  Back -> liftEffect goBack

  DismissNotice -> H.modify_ _ { notice = Nothing }

  FilesPicked event -> do
    files <- liftEffect $ case target event >>= Input.fromEventTarget of
      Nothing -> pure []
      Just input -> takeFiles input
    state <- H.get
    let
      room = max 0 (state.settings.maxMediaAttachments - Array.length state.items)
      admitted = Array.take room files
      dropped = Array.length files - Array.length admitted
    when (dropped > 0) do
      H.modify_ _ { notice = Just ("Only " <> show state.settings.maxMediaAttachments <> " files per post; " <> show dropped <> " left out.") }
    for_ admitted addFile

  RemoveItem key -> H.modify_ \s -> s { items = Array.filter (\it -> it.key /= key) s.items }

  SetAlt key text -> modifyItem key _ { altText = text, altError = Nothing }

  SetCaption text -> H.modify_ \s -> s
    { caption = text
    , posting = case s.posting of
        PostFailed _ -> Idle
        other -> other
    }

  SetVisibility raw -> H.modify_ _ { visibility = decodeVisibility raw }

  SetSensitive on -> H.modify_ _ { sensitive = on }

  SetSpoiler text -> H.modify_ _ { spoilerText = text }

  SetCommentsDisabled on -> H.modify_ _ { commentsDisabled = on }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    when (canPost state) do
      H.modify_ _ { posting = Posting }
      -- Alt text typed but not yet saved goes to the server before the post
      -- is created, so the attachment carries it from the start.
      saved <- flushAlt
      if not saved then H.modify_ _ { posting = PostFailed "Alt text could not be saved. Fix it or remove the file." }
      else do
        backend <- asks _.backend
        current <- H.get
        result <- liftAff $ createPost backend
          { caption: trim current.caption
          , mediaIds: uploadedIds current
          , visibility: current.visibility
          , sensitive: current.sensitive
          , spoilerText: if current.sensitive then trim current.spoilerText else ""
          , commentsDisabled: current.commentsDisabled
          }
        case result of
          Left err -> H.modify_ _ { posting = PostFailed (printAppError err) }
          Right status -> do
            -- The composer stays mounted under the post it leads to, so it
            -- is emptied here: coming back must not offer the same post again.
            H.modify_ \s -> initialState { settings = s.settings, visibility = startVisibility s.settings }
            liftEffect (navigate (StatusDetail (statusId status)))

-- | Adds one picked file: a row, then an upload in the background. Files
-- | the instance would refuse are rejected here with the reason, and keep
-- | their row so the person sees what happened.
addFile :: forall o m. MonadAff m => MonadAsk Env m => File -> H.HalogenM State Action () o m Unit
addFile file = do
  state <- H.get
  let
    key = state.nextKey
    mime = mimeOf file
    sizeBytes = File.size file
    verdict
      | sizeBytes > toNumber state.settings.maxFileSizeKb * 1024.0 =
          Rejected ("Larger than " <> megabytes state.settings.maxFileSizeKb <> " MB")
      | not (Array.null state.settings.allowedMediaTypes) && not (Array.elem mime state.settings.allowedMediaTypes) =
          Rejected ("This server does not accept " <> mime)
      | otherwise = Uploading
    item =
      { key
      , name: File.name file
      , mime
      , sizeBytes
      , phase: verdict
      , altText: ""
      , altSaved: ""
      , altError: Nothing
      }
  H.modify_ \s -> s { items = Array.snoc s.items item, nextKey = key + 1 }
  case verdict of
    Uploading -> void $ H.fork do
      backend <- asks _.backend
      outcome <- liftAff do
        bytes <- attempt (readBytes file)
        case bytes of
          Left err -> pure (Left ("Could not read the file: " <> message err))
          Right buffer -> do
            uploaded <- uploadMedia backend { bytes: buffer, filename: File.name file, mime, description: Nothing }
            pure case uploaded of
              Left err -> Left (printAppError err)
              Right m -> Right m
      modifyItem key _
        { phase = case outcome of
            Left why -> Rejected why
            Right m -> Uploaded m
        }
    _ -> pure unit

modifyItem :: forall o m. Int -> (Item -> Item) -> H.HalogenM State Action () o m Unit
modifyItem key f = H.modify_ \s -> s { items = map (\it -> if it.key == key then f it else it) s.items }

needsAltSave :: Item -> Boolean
needsAltSave it = case it.phase of
  Uploaded _ -> trim it.altText /= it.altSaved
  _ -> false

-- | PUT /api/v1/media/{id} with the description, for every attachment whose
-- | text differs from what the server holds, in parallel; false when any
-- | save failed. Pressing Post is the only thing that calls this. Saving
-- | when a field lost focus as well, as this screen once did, meant that the
-- | press on Post which took the focus away arrived while that save was
-- | under way and was dropped.
flushAlt :: forall o m. MonadAff m => MonadAsk Env m => H.HalogenM State Action () o m Boolean
flushAlt = do
  backend <- asks _.backend
  pending <- H.gets (Array.mapMaybe uploadedPending <<< _.items)
  results <- liftAff $ parTraverse
    (\(Tuple it m) -> Tuple it.key <$> updateMediaDescription backend m.id (trim it.altText))
    pending
  H.modify_ \s -> s
    { items = map
        ( \it -> case Array.find (\(Tuple k _) -> k == it.key) results of
            Just (Tuple _ (Right updated)) -> it { altSaved = fromMaybe (trim it.altText) updated.description, altError = Nothing }
            Just (Tuple _ (Left err)) -> it { altError = Just (printAppError err) }
            Nothing -> it
        )
        s.items
    }
  pure (Array.all (\(Tuple _ r) -> isRight r) results)
  where
  uploadedPending it = case it.phase of
    Uploaded m | needsAltSave it -> Just (Tuple it m)
    _ -> Nothing