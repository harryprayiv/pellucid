-- | JSON decoders for `Pellucid.Entity`. Written against the server's
-- | transformers rather than the Mastodon spec: Pellucid omits some fields,
-- | nulls others, and adds its own when `_pe=1` is requested.
module Pellucid.Decode
  ( decodeAccount
  , decodeStatus
  , decodeStatuses
  , decodeMedia
  ) where

import Prelude

import Data.Argonaut.Core (Json, isNull, toNumber, toString)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Pellucid.Entity (Account, AccountId(..), LikedBy, MediaAttachment, MediaId(..), MediaType(..), Mention, PfType(..), Place, Status(..), StatusId(..), Tag, Visibility(..))

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
  url <- obj .: "url"
  previewUrl <- obj .:? "preview_url"
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

-- | Counters arrive as numbers, but Pellucid has shipped them as strings and
-- | as null. Anything unreadable falls back to the default.
lenientInt :: Int -> Maybe Json -> Int
lenientInt dflt = case _ of
  Nothing -> dflt
  Just json -> fromMaybe dflt $ case toNumber json of
    Just n -> Int.fromNumber n
    Nothing -> toString json >>= Int.fromString
