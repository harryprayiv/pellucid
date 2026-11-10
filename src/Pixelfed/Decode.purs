-- | JSON decoders for `Pixelfed.Entity`. Written against the server's
-- | transformers rather than the Mastodon spec: Pixelfed omits some fields,
-- | nulls others, adds its own when `_pe=1` is requested, and sends several
-- | straight from the database row, so that a flag arrives as `0` or `1`
-- | and a number can arrive as a string.
-- |
-- | The rule every decoder here follows: only what identifies a thing can
-- | fail its decode (a status's id and account, an account's id and
-- | username, an attachment's id). Every other field is read through the
-- | tolerant readers at the bottom of this module, which accept each
-- | encoding the server is known to use and fall back to a default for
-- | anything else. One field of an unexpected type must not take a whole
-- | timeline with it.
module Pixelfed.Decode
  ( decodeAccount
  , decodeAccounts
  , decodeHashtag
  , decodeHashtags
  , decodeTrendingTags
  , decodeSearch
  , decodeMessage
  , decodeThread
  , decodeConversations
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

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json, isNull, toArray, toBoolean, toNumber, toObject, toString)
import Data.Argonaut.Decode (JsonDecodeError(..))
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower)
import Data.String as String
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Pixelfed.Entity (Account, AccountId(..), ComposeSettings, Context, Conversation, HashtagInfo, LikedBy, MediaAttachment, MediaId(..), MediaType(..), Mention, Message, MessageId(..), Notification, NotificationId(..), NotificationKind(..), PfType(..), Place, Relationship, SearchResults, Status(..), StatusId(..), Tag, Thread, TrendingTag, Visibility(..))
import Util.Text (stripTags)

-- | A list of posts is strict about its entries: one that cannot be
-- | identified fails the page with an error the person sees.
decodeStatuses :: Json -> Either JsonDecodeError (Array Status)
decodeStatuses = strictArray decodeStatus

decodeStatus :: Json -> Either JsonDecodeError Status
decodeStatus json = do
  obj <- object json
  id <- StatusId <$> required obj "id"
  account <- requiredJson obj "account" >>= decodeAccount >>> atKey "account"
  -- A share wraps another post. When that one is present it has to be
  -- readable: a wrapper without its post has nothing to show.
  reblog <- atKey "reblog" (traverse decodeStatus (field obj "reblog"))
  pure $ Status
    { id
    , uri: textOr "" obj "uri"
    , url: text obj "url"
    , createdAt: textOr "" obj "created_at"
    , editedAt: text obj "edited_at"
    , account
    , content: textOr "" obj "content"
    , contentText: text obj "content_text"
    , visibility: decodeVisibility (textOr "public" obj "visibility")
    , sensitive: flag obj "sensitive"
    , spoilerText: textOr "" obj "spoiler_text"
    , pfType: decodePfType (textOr "" obj "pf_type")
    , mediaAttachments: list decodeMedia obj "media_attachments"
    , favouritesCount: int 0 obj "favourites_count"
    , reblogsCount: int 0 obj "reblogs_count"
    , replyCount: int 0 obj "reply_count"
    , favourited: flag obj "favourited"
    , reblogged: flag obj "reblogged"
    , bookmarked: flag obj "bookmarked"
    , commentsDisabled: flag obj "comments_disabled"
    , inReplyToId: StatusId <$> text obj "in_reply_to_id"
    , inReplyToAccountId: AccountId <$> text obj "in_reply_to_account_id"
    , reblog
    , tags: list decodeTag obj "tags"
    , mentions: list decodeMention obj "mentions"
    , likedBy: field obj "liked_by" >>= decodeLikedBy
    , place: field obj "place" >>= decodePlace
    , local: flag obj "local"
    , pinned: flag obj "pinned"
    }

decodeAccount :: Json -> Either JsonDecodeError Account
decodeAccount json = do
  obj <- object json
  id <- AccountId <$> required obj "id"
  username <- required obj "username"
  pure
    { id
    , username
    -- A local account's handle is its username; the server sends both.
    , acct: textOr username obj "acct"
    , displayName: textOr "" obj "display_name"
    , note: textOr "" obj "note"
    , noteText: text obj "note_text"
    , url: textOr "" obj "url"
    , avatar: textOr "" obj "avatar"
    , locked: flag obj "locked"
    , followersCount: int 0 obj "followers_count"
    , followingCount: int 0 obj "following_count"
    , statusesCount: int 0 obj "statuses_count"
    , local: flag obj "local"
    , isAdmin: flag obj "is_admin"
    , createdAt: textOr "" obj "created_at"
    , website: text obj "website"
    , headerBg: text obj "header_bg"
    , pronouns: list (note (TypeMismatch "String") <<< toString) obj "pronouns"
    , hasStory: flag obj "has_story"
    }

decodeAccounts :: Json -> Either JsonDecodeError (Array Account)
decodeAccounts = lenientArray decodeAccount

-- | `MediaTransformer` sends `is_nsfw`, `width` and `height` as the database
-- | row holds them: `0` or `1` (or null) for the flag, and numbers that some
-- | database drivers hand over as strings.
decodeMedia :: Json -> Either JsonDecodeError MediaAttachment
decodeMedia json = do
  obj <- object json
  id <- MediaId <$> required obj "id"
  let
    previewUrl = text obj "preview_url"
    original = nested obj "meta" >>= \meta -> nested meta "original"
  pure
    { id
    , mediaType: decodeMediaType (textOr "unknown" obj "type")
    -- `POST /api/v2/media` answers with `url: null` and the file in `preview_url`.
    , url: fromMaybe (fromMaybe "" previewUrl) (text obj "url")
    , previewUrl
    , optimizedUrl: text obj "optimized_url"
    , description: text obj "description"
    , blurhash: text obj "blurhash"
    , isNsfw: flag obj "is_nsfw"
    , mime: text obj "mime"
    , dimensions: do
        o <- original
        width <- intMaybe o "width"
        height <- intMaybe o "height"
        pure { width, height }
    }

decodeTag :: Json -> Either JsonDecodeError Tag
decodeTag json = do
  obj <- object json
  name <- required obj "name"
  pure { name, url: textOr "" obj "url" }

decodeMention :: Json -> Either JsonDecodeError Mention
decodeMention json = do
  obj <- object json
  id <- AccountId <$> required obj "id"
  username <- required obj "username"
  pure { id, username, acct: textOr username obj "acct", url: textOr "" obj "url" }

-- | `{ username: null, others: false }` means nobody to show; it reads as
-- | `Nothing` so the view does not have to special-case it.
decodeLikedBy :: Json -> Maybe LikedBy
decodeLikedBy json = do
  obj <- toObject json
  username <- text obj "username"
  pure
    { id: AccountId <$> text obj "id"
    , username: Just username
    , others: flag obj "others"
    , totalCount: int 0 obj "total_count"
    }

decodePlace :: Json -> Maybe Place
decodePlace json = do
  obj <- toObject json
  name <- text obj "name"
  pure { name, country: text obj "country" }

decodeRelationships :: Json -> Either JsonDecodeError (Array Relationship)
decodeRelationships = lenientArray decodeRelationship

decodeRelationship :: Json -> Either JsonDecodeError Relationship
decodeRelationship json = do
  obj <- object json
  id <- AccountId <$> required obj "id"
  pure
    { id
    , following: flag obj "following"
    , followedBy: flag obj "followed_by"
    , blocking: flag obj "blocking"
    , muting: flag obj "muting"
    , requested: flag obj "requested"
    }

decodeContext :: Json -> Either JsonDecodeError Context
decodeContext json = do
  obj <- object json
  ancestors <- atKey "ancestors" (fromMaybe [] <$> traverse decodeStatuses (field obj "ancestors"))
  descendants <- atKey "descendants" (fromMaybe [] <$> traverse decodeStatuses (field obj "descendants"))
  pure { ancestors, descendants }

decodeNotifications :: Json -> Either JsonDecodeError (Array Notification)
decodeNotifications = strictArray decodeNotification

decodeNotification :: Json -> Either JsonDecodeError Notification
decodeNotification json = do
  obj <- object json
  id <- NotificationId <$> required obj "id"
  pure
    { id
    , kind: decodeKind (textOr "" obj "type")
    , createdAt: textOr "" obj "created_at"
    -- An actor or a post the server could not load, or one these decoders
    -- cannot identify, is absent rather than an error.
    , account: field obj "account" >>= decodeAccount >>> hush
    , status: field obj "status" >>= decodeStatus >>> hush
    , tagged: field obj "tagged" >>= decodeTagged
    }

-- | `tagged` is `{ username, post_url, status_id, profile_id }`. The server
-- | sends `status_id` as a bare number, which JavaScript cannot hold exactly
-- | at the size of a Pixelfed id, so the id is taken from the last segment
-- | of `post_url` when that is a number. `status_id` is used only when it
-- | arrived exact (see `text`); with neither, the notification carries no
-- | tagged post and its row opens the person who tagged instead.
decodeTagged :: Json -> Maybe { username :: String, statusId :: StatusId }
decodeTagged json = do
  obj <- toObject json
  username <- text obj "username"
  statusId <- (text obj "post_url" >>= idFromUrl) <|> text obj "status_id"
  pure { username, statusId: StatusId statusId }
  where
  idFromUrl :: String -> Maybe String
  idFromUrl url = do
    last <- Array.last (String.split (String.Pattern "/") url)
    if last /= "" && isDigits last then Just last else Nothing

  isDigits :: String -> Boolean
  isDigits = Array.all (\c -> c >= String.codePointFromChar '0' && c <= String.codePointFromChar '9') <<< String.toCodePointArray

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
  obj <- object json
  let
    allowedMediaTypes = case field obj "allowed_media_types" of
      Nothing -> []
      Just j -> case toArray j, toString j of
        Just entries, _ -> Array.mapMaybe toString entries
        _, Just one -> Array.filter (_ /= "") (map String.trim (String.split (String.Pattern ",") one))
        _, _ -> []
  pure
    { allowedMediaTypes
    , maxCaptionLength: int 500 obj "max_caption_length"
    , maxFileSizeKb: int 15000 obj "max_file_size"
    , maxMediaAttachments: int 4 obj "max_media_attachments"
    , maxAltTextLength: int 1000 obj "max_altext_length"
    , defaultVisibility: decodeVisibility <$> text obj "default_scope"
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

decodeHashtag :: Json -> Either JsonDecodeError HashtagInfo
decodeHashtag json = do
  obj <- object json
  name <- required obj "name"
  -- `following` is absent from search results; `count` is only sent with `_pe=1`.
  pure { name, following: flag obj "following", count: intMaybe obj "count" }

decodeHashtags :: Json -> Either JsonDecodeError (Array HashtagInfo)
decodeHashtags = lenientArray decodeHashtag

-- | `TrendingHashtagService::getTrending`: `{ id, total, name: "#tag",
-- | hashtag: "tag", url }`.
decodeTrendingTags :: Json -> Either JsonDecodeError (Array TrendingTag)
decodeTrendingTags = lenientArray \json -> do
  obj <- object json
  name <- case text obj "hashtag", text obj "name" of
    Just bare, _ -> pure bare
    Nothing, Just display -> pure (fromMaybe display (String.stripPrefix (String.Pattern "#") display))
    Nothing, Nothing -> Left (AtKey "hashtag" MissingValue)
  pure { name, total: int 0 obj "total" }

-- | `GET /api/v2/search`: `{ accounts, hashtags, statuses }`.
decodeSearch :: Json -> Either JsonDecodeError SearchResults
decodeSearch json = do
  obj <- object json
  pure { accounts: list decodeAccount obj "accounts", hashtags: list decodeHashtag obj "hashtags" }

-- | A direct message in the thread endpoint's own shape. Older servers omit
-- | `created_at` from the answer to a send.
decodeMessage :: Json -> Either JsonDecodeError Message
decodeMessage json = do
  obj <- object json
  id <- MessageId <$> required obj "id"
  pure
    { id
    , isAuthor: flag obj "isAuthor"
    , kind: textOr "text" obj "type"
    , text: textOr "" obj "text"
    , media: text obj "media"
    , createdAt: textOr "" obj "created_at"
    , timeAgo: textOr "" obj "timeAgo"
    , seen: flag obj "seen"
    }

decodeThread :: Json -> Either JsonDecodeError Thread
decodeThread json = do
  obj <- object json
  id <- AccountId <$> required obj "id"
  let username = textOr "" obj "username"
  pure
    { id
    , name: textOr username obj "name"
    , username
    , avatar: textOr "" obj "avatar"
    , messages: list decodeMessage obj "messages"
    }

-- | `GET /api/v1/conversations`. The entry's own id is a number on older
-- | servers and a string on newer ones; `last_status` is a status-shaped
-- | wrapper around the last message.
decodeConversations :: Json -> Either JsonDecodeError (Array Conversation)
decodeConversations = lenientArray \json -> do
  obj <- object json
  id <- required obj "id"
  last <- note (AtKey "last_status" MissingValue) (nested obj "last_status")
  lastId <- required last "id"
  pure
    { id
    , unread: flag obj "unread"
    , accounts: list decodeAccount obj "accounts"
    , lastId
    , lastText: fromMaybe (stripTags (textOr "" last "content")) (text last "content_text")
    , lastAt: textOr "" last "created_at"
    }

-- Tolerant readers ------------------------------------------------------------
--
-- Each takes the object and a key. None of them fails: a field that is
-- absent, null or of a type the reader does not know reads as nothing, or
-- as the reader's default.

-- | The value under a key, unless it is absent or null.
field :: Object Json -> String -> Maybe Json
field obj key = Object.lookup key obj >>= \value -> if isNull value then Nothing else Just value

-- | A string, or a whole number written as one. Ids arrive both ways.
-- |
-- | A number is accepted only where JavaScript holds it exactly, which is
-- | up to 2^53 - 1. A Pixelfed id is larger than that, so one sent as a
-- | bare number has already lost its last digits by the time it is read
-- | here, and what is left names some other post or nothing at all. Such a
-- | number reads as absent.
text :: Object Json -> String -> Maybe String
text obj key = field obj key >>= \value -> toString value <|> (toNumber value >>= wholeNumber)
  where
  wholeNumber :: Number -> Maybe String
  wholeNumber n
    | n > largestExact || n < negate largestExact = Nothing
    -- `show` writes a whole number with a trailing `.0` and anything else
    -- without, which is the test for whole.
    | otherwise = String.stripSuffix (String.Pattern ".0") (show n)

  largestExact :: Number
  largestExact = 9007199254740991.0

textOr :: String -> Object Json -> String -> String
textOr fallback obj key = fromMaybe fallback (text obj key)

-- | A flag as `true`/`false`, as `1`/`0`, or as one of those in a string.
-- | A transformer that casts sends a boolean. One that passes the database
-- | column through sends a number, or a string where the database driver
-- | returns numbers as strings. Anything else is false.
flag :: Object Json -> String -> Boolean
flag obj key = case field obj key of
  Nothing -> false
  Just value -> case toBoolean value, toNumber value, toString value of
    Just b, _, _ -> b
    _, Just n, _ -> n /= 0.0
    _, _, Just s -> toLower (String.trim s) == "true" || String.trim s == "1"
    _, _, _ -> false

-- | A whole number as a number or as a string of digits.
intMaybe :: Object Json -> String -> Maybe Int
intMaybe obj key = field obj key >>= \value -> case toNumber value of
  Just n -> Int.fromNumber n <|> Just (Int.round n)
  Nothing -> toString value >>= String.trim >>> Int.fromString

-- | Counters arrive as numbers, but Pixelfed has shipped them as strings and
-- | as null. Anything unreadable falls back to the default.
int :: Int -> Object Json -> String -> Int
int fallback obj key = fromMaybe fallback (intMaybe obj key)

-- | An object under a key.
nested :: Object Json -> String -> Maybe (Object Json)
nested obj key = field obj key >>= toObject

-- | An array under a key with the entries that cannot be read left out;
-- | anything that is not an array reads as empty.
list :: forall a. (Json -> Either JsonDecodeError a) -> Object Json -> String -> Array a
list decode obj key = case field obj key >>= toArray of
  Just entries -> Array.mapMaybe (hush <<< decode) entries
  Nothing -> []

-- Strict readers --------------------------------------------------------------

object :: Json -> Either JsonDecodeError (Object Json)
object = note (TypeMismatch "Object") <<< toObject

-- | What identifies a thing: present, and a string or a whole number that
-- | arrived exact.
required :: Object Json -> String -> Either JsonDecodeError String
required obj key = case field obj key of
  Nothing -> Left (AtKey key MissingValue)
  Just _ -> note (AtKey key (TypeMismatch "String or exact whole number")) (text obj key)

requiredJson :: Object Json -> String -> Either JsonDecodeError Json
requiredJson obj key = note (AtKey key MissingValue) (field obj key)

atKey :: forall a. String -> Either JsonDecodeError a -> Either JsonDecodeError a
atKey key = case _ of
  Left err -> Left (AtKey key err)
  Right a -> Right a

-- | Every entry must decode; the error names the index of the one that
-- | did not.
strictArray :: forall a. (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Array a)
strictArray decode json = do
  entries <- note (TypeMismatch "Array") (toArray json)
  traverse (\{ index, value } -> lmapIndex index (decode value)) (Array.mapWithIndex (\index value -> { index, value }) entries)
  where
  lmapIndex index = case _ of
    Left err -> Left (AtIndex index err)
    Right a -> Right a

-- | A list in which an entry that does not decode is dropped instead of
-- | failing the page. Used for lists of accounts, hashtags and messages,
-- | where the server itself filters out what it could not load and has
-- | shipped `[]` and `false` in place of an entry.
lenientArray :: forall a. (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Array a)
lenientArray decode json = Array.mapMaybe (hush <<< decode) <$> note (TypeMismatch "Array") (toArray json)

note :: forall a. JsonDecodeError -> Maybe a -> Either JsonDecodeError a
note err = case _ of
  Just a -> Right a
  Nothing -> Left err