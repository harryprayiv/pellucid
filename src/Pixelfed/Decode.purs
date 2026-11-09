-- | JSON decoders for `Pixelfed.Entity`. Written against the server's
-- | transformers rather than the Mastodon spec: Pixelfed omits some fields,
-- | nulls others, and adds its own when `_pe=1` is requested.
module Pixelfed.Decode
  ( decodeAccount
  , decodeStatus
  , decodeStatuses
  , decodeMedia
  , decodeRelationship
  , decodeRelationships
  , decodeContext
  , decodeNotification
  , decodeNotifications
  , decodeComposeSettings
  , decodeVisibility
  ) where

import Prelude

import Data.Argonaut.Core (Json, isNull, toNumber, toString)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either, either, hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)
import Data.String as String
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Pixelfed.Entity (Account, AccountId(..), ComposeSettings, Context, LikedBy, MediaAttachment, MediaId(..), MediaType(..), Mention, Notification, NotificationId(..), NotificationKind(..), PfType(..), Place, Relationship, Status(..), StatusId(..), Tag, Visibility(..))

decodeStatuses :: Json -> Either JsonDecodeError (Array Status)
decodeStatuses = decodeArray decodeStatus

decodeStatus :: Json -> Either JsonDecodeError Status
decodeStatus json = do
  obj <- decodeJson json
  id <- StatusId <$> obj .: "id"
  uri <- obj .: "uri"
  url <- obj .:? "url"
  createdAt <- obj .: "created_at"
  editedAt <- obj .:? "edited_at"
  account <- obj .: "account" >>= decodeAccount
  content <- fromMaybe "" <$> obj .:? "content"
  contentText <- obj .:? "content_text"
  visibility <- decodeVisibility <$> (fromMaybe "public" <$> obj .:? "visibility")
  sensitive <- fromMaybe false <$> obj .:? "sensitive"
  spoilerText <- fromMaybe "" <$> obj .:? "spoiler_text"
  pfType <- decodePfType <$> (fromMaybe "" <$> obj .:? "pf_type")
  mediaAttachments <- obj .:? "media_attachments" >>= traverse (decodeArray decodeMedia) >>> map (fromMaybe [])
  favouritesCount <- lenientInt 0 <$> obj .:? "favourites_count"
  reblogsCount <- lenientInt 0 <$> obj .:? "reblogs_count"
  replyCount <- lenientInt 0 <$> obj .:? "reply_count"
  favourited <- fromMaybe false <$> obj .:? "favourited"
  reblogged <- fromMaybe false <$> obj .:? "reblogged"
  bookmarked <- fromMaybe false <$> obj .:? "bookmarked"
  commentsDisabled <- fromMaybe false <$> obj .:? "comments_disabled"
  inReplyToId <- map StatusId <$> obj .:? "in_reply_to_id"
  inReplyToAccountId <- map AccountId <$> obj .:? "in_reply_to_account_id"
  reblog <- obj .:? "reblog" >>= traverse decodeStatus
  tags <- obj .:? "tags" >>= traverse (decodeArray decodeTag) >>> map (fromMaybe [])
  mentions <- obj .:? "mentions" >>= traverse (decodeArray decodeMention) >>> map (fromMaybe [])
  likedBy <- obj .:? "liked_by" >>= traverse decodeLikedBy >>> map join
  place <- obj .:? "place" >>= traverse decodePlace >>> map join
  local <- fromMaybe false <$> obj .:? "local"
  pinned <- fromMaybe false <$> obj .:? "pinned"
  pure $ Status
    { id
    , uri
    , url
    , createdAt
    , editedAt
    , account
    , content
    , contentText
    , visibility
    , sensitive
    , spoilerText
    , pfType
    , mediaAttachments
    , favouritesCount
    , reblogsCount
    , replyCount
    , favourited
    , reblogged
    , bookmarked
    , commentsDisabled
    , inReplyToId
    , inReplyToAccountId
    , reblog
    , tags
    , mentions
    , likedBy
    , place
    , local
    , pinned
    }

decodeAccount :: Json -> Either JsonDecodeError Account
decodeAccount json = do
  obj <- decodeJson json
  id <- AccountId <$> obj .: "id"
  username <- obj .: "username"
  acct <- obj .: "acct"
  displayName <- fromMaybe "" <$> obj .:? "display_name"
  note <- fromMaybe "" <$> obj .:? "note"
  noteText <- obj .:? "note_text"
  url <- obj .: "url"
  avatar <- fromMaybe "" <$> obj .:? "avatar"
  locked <- fromMaybe false <$> obj .:? "locked"
  followersCount <- lenientInt 0 <$> obj .:? "followers_count"
  followingCount <- lenientInt 0 <$> obj .:? "following_count"
  statusesCount <- lenientInt 0 <$> obj .:? "statuses_count"
  local <- fromMaybe false <$> obj .:? "local"
  isAdmin <- fromMaybe false <$> obj .:? "is_admin"
  createdAt <- fromMaybe "" <$> obj .:? "created_at"
  website <- obj .:? "website"
  headerBg <- obj .:? "header_bg"
  pronouns <- fromMaybe [] <$> obj .:? "pronouns"
  hasStory <- fromMaybe false <$> obj .:? "has_story"
  pure
    { id
    , username
    , acct
    , displayName
    , note
    , noteText
    , url
    , avatar
    , locked
    , followersCount
    , followingCount
    , statusesCount
    , local
    , isAdmin
    , createdAt
    , website
    , headerBg
    , pronouns
    , hasStory
    }

decodeMedia :: Json -> Either JsonDecodeError MediaAttachment
decodeMedia json = do
  obj <- decodeJson json
  id <- MediaId <$> obj .: "id"
  mediaType <- decodeMediaType <$> (fromMaybe "unknown" <$> obj .:? "type")
  -- `POST /api/v2/media` answers with `url: null` and the file in `preview_url`.
  urlField <- obj .:? "url"
  previewUrl <- obj .:? "preview_url"
  let url = fromMaybe (fromMaybe "" previewUrl) urlField
  optimizedUrl <- obj .:? "optimized_url"
  description <- obj .:? "description"
  blurhash <- obj .:? "blurhash"
  isNsfw <- fromMaybe false <$> obj .:? "is_nsfw"
  mime <- obj .:? "mime"
  meta <- obj .:? "meta"
  dimensions <- case meta of
    Nothing -> pure Nothing
    Just m -> decodeDimensions m
  pure { id, mediaType, url, previewUrl, optimizedUrl, description, blurhash, isNsfw, mime, dimensions }

-- | `meta.original.{width,height}` when the server knows the size.
decodeDimensions :: Object Json -> Either JsonDecodeError (Maybe { width :: Int, height :: Int })
decodeDimensions meta = do
  original <- meta .:? "original"
  case original of
    Nothing -> pure Nothing
    Just o -> do
      width <- o .:? "width"
      height <- o .:? "height"
      pure case width, height of
        Just w, Just h -> Just { width: w, height: h }
        _, _ -> Nothing

decodeTag :: Json -> Either JsonDecodeError Tag
decodeTag json = do
  obj <- decodeJson json
  name <- obj .: "name"
  url <- fromMaybe "" <$> obj .:? "url"
  pure { name, url }

decodeMention :: Json -> Either JsonDecodeError Mention
decodeMention json = do
  obj <- decodeJson json
  id <- AccountId <$> obj .: "id"
  username <- obj .: "username"
  acct <- fromMaybe username <$> obj .:? "acct"
  url <- fromMaybe "" <$> obj .:? "url"
  pure { id, username, acct, url }

-- | `{ username: null, others: false }` means nobody to show; it decodes to
-- | `Nothing` so the view does not have to special-case it.
decodeLikedBy :: Json -> Either JsonDecodeError (Maybe LikedBy)
decodeLikedBy json = do
  obj <- decodeJson json
  username <- obj .:? "username"
  case username of
    Nothing -> pure Nothing
    Just u -> do
      id <- map AccountId <$> obj .:? "id"
      others <- fromMaybe false <$> obj .:? "others"
      totalCount <- lenientInt 0 <$> obj .:? "total_count"
      pure (Just { id, username: Just u, others, totalCount })

decodePlace :: Json -> Either JsonDecodeError (Maybe Place)
decodePlace json
  | isNull json = pure Nothing
  | otherwise = do
      obj <- decodeJson json
      name <- obj .:? "name"
      country <- obj .:? "country"
      pure (map (\n -> { name: n, country }) name)

decodeRelationships :: Json -> Either JsonDecodeError (Array Relationship)
decodeRelationships = decodeArray decodeRelationship

decodeRelationship :: Json -> Either JsonDecodeError Relationship
decodeRelationship json = do
  obj <- decodeJson json
  id <- AccountId <$> obj .: "id"
  following <- fromMaybe false <$> obj .:? "following"
  followedBy <- fromMaybe false <$> obj .:? "followed_by"
  blocking <- fromMaybe false <$> obj .:? "blocking"
  muting <- fromMaybe false <$> obj .:? "muting"
  requested <- fromMaybe false <$> obj .:? "requested"
  pure { id, following, followedBy, blocking, muting, requested }

decodeContext :: Json -> Either JsonDecodeError Context
decodeContext json = do
  obj <- decodeJson json
  ancestors <- obj .:? "ancestors" >>= traverse decodeStatuses >>> map (fromMaybe [])
  descendants <- obj .:? "descendants" >>= traverse decodeStatuses >>> map (fromMaybe [])
  pure { ancestors, descendants }

decodeNotifications :: Json -> Either JsonDecodeError (Array Notification)
decodeNotifications = decodeArray decodeNotification

decodeNotification :: Json -> Either JsonDecodeError Notification
decodeNotification json = do
  obj <- decodeJson json
  id <- NotificationId <$> obj .: "id"
  kind <- decodeKind <$> (fromMaybe "" <$> obj .:? "type")
  createdAt <- fromMaybe "" <$> obj .:? "created_at"
  let
    -- An actor or a post the server could not load, or one with a shape
    -- these decoders do not know, becomes Nothing rather than an error.
    account = lenient (obj .:? "account" >>= traverse decodeAccount)
    status = lenient (obj .:? "status" >>= traverse decodeStatus)
    tagged = lenient (obj .:? "tagged" >>= traverse decodeTagged)
  pure { id, kind, createdAt, account, status, tagged }
  where
  lenient :: forall a. Either JsonDecodeError (Maybe a) -> Maybe a
  lenient = either (const Nothing) identity

decodeTagged :: Json -> Either JsonDecodeError { username :: String, statusId :: StatusId }
decodeTagged json = do
  obj <- decodeJson json
  username <- obj .: "username"
  statusId <- StatusId <$> obj .: "status_id"
  pure { username, statusId }

decodeKind :: String -> NotificationKind
decodeKind raw = case toLower raw of
  "follow" -> NFollow
  "mention" -> NMention
  "share" -> NShare
  "reblog" -> NShare
  "favourite" -> NFavourite
  "like" -> NFavourite
  "comment" -> NComment
  "tagged" -> NTagged
  "modlog" -> NModlog
  "direct" -> NDirect
  "story:react" -> NStoryReact
  "story:comment" -> NStoryComment
  other -> NOther other

-- | `allowed_media_types` arrives as an array, or as one comma-separated
-- | string when the instance configured a single type.
decodeComposeSettings :: Json -> Either JsonDecodeError ComposeSettings
decodeComposeSettings json = do
  obj <- decodeJson json
  typesJson <- obj .:? "allowed_media_types"
  let
    allowedMediaTypes = case typesJson of
      Nothing -> []
      Just j -> case hush (decodeJson j) of
        Just (arr :: Array String) -> arr
        Nothing -> case hush (decodeJson j) of
          Just (one :: String) -> map String.trim (String.split (String.Pattern ",") one)
          Nothing -> []
  maxCaptionLength <- lenientInt 500 <$> obj .:? "max_caption_length"
  maxFileSizeKb <- lenientInt 15000 <$> obj .:? "max_file_size"
  maxMediaAttachments <- lenientInt 4 <$> obj .:? "max_media_attachments"
  maxAltTextLength <- lenientInt 1000 <$> obj .:? "max_altext_length"
  defaultScope <- obj .:? "default_scope"
  pure
    { allowedMediaTypes
    , maxCaptionLength
    , maxFileSizeKb
    , maxMediaAttachments
    , maxAltTextLength
    , defaultVisibility: map decodeVisibility defaultScope
    }

decodeVisibility :: String -> Visibility
decodeVisibility raw = case toLower raw of
  "public" -> Public
  "unlisted" -> Unlisted
  "private" -> Private
  "direct" -> Direct
  "draft" -> Draft
  other -> OtherVisibility other

decodePfType :: String -> PfType
decodePfType raw = case toLower raw of
  "photo" -> Photo
  "photo:album" -> PhotoAlbum
  "video" -> Video
  "video:album" -> VideoAlbum
  "photo:video:album" -> PhotoVideoAlbum
  "share" -> Share
  "text" -> TextPost
  other -> OtherType other

decodeMediaType :: String -> MediaType
decodeMediaType raw = case toLower raw of
  "image" -> Image
  "video" -> VideoMedia
  "gifv" -> Gifv
  "audio" -> Audio
  _ -> UnknownMedia

-- | Counters arrive as numbers, but Pixelfed has shipped them as strings and
-- | as null. Anything unreadable falls back to the default.
lenientInt :: Int -> Maybe Json -> Int
lenientInt dflt = case _ of
  Nothing -> dflt
  Just json -> fromMaybe dflt $ case toNumber json of
    Just n -> Int.fromNumber n
    Nothing -> toString json >>= Int.fromString
