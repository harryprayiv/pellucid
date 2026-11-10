-- | The subset of Pixelfed's API entities the client renders. Field names
-- | follow the wire format (`app/Transformer/Api/*.php` in the Pixelfed
-- | repository) translated to camelCase. Decoders live in `Pixelfed.Decode`.
module Pixelfed.Entity
  ( AccountId(..)
  , StatusId(..)
  , MediaId(..)
  , Visibility(..)
  , PfType(..)
  , MediaType(..)
  , Account
  , MediaAttachment
  , Dimensions
  , Tag
  , Mention
  , LikedBy
  , Place
  , Status(..)
  , StatusRecord
  , Relationship
  , Context
  , NotificationId(..)
  , NotificationKind(..)
  , Notification
  , Tagged
  , ComposeSettings
  , NewPost
  , HashtagInfo
  , TrendingTag
  , SearchResults
  , ProfileEdit
  , PostEdit
  , MessageId(..)
  , Message
  , Thread
  , Conversation
  , ReportTarget(..)
  , ReportReason(..)
  , reportReasons
  , reportReasonName
  , reportReasonLabel
  , tagKey
  , statusId
  , displayed
  , isAlbum
  , captionOf
  , firstMedia
  , visibilityName
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (toLower, trim)
import Util.Text (stripTags)

newtype AccountId = AccountId String

derive instance Newtype AccountId _
derive newtype instance Eq AccountId
derive newtype instance Ord AccountId
derive newtype instance Show AccountId

newtype StatusId = StatusId String

derive instance Newtype StatusId _
derive newtype instance Eq StatusId
derive newtype instance Ord StatusId
derive newtype instance Show StatusId

newtype MediaId = MediaId String

derive instance Newtype MediaId _
derive newtype instance Eq MediaId
derive newtype instance Ord MediaId
derive newtype instance Show MediaId

-- | Pixelfed sends `public`, `unlisted`, `private`, `direct`; pixelfed-rn also
-- | lists `draft`. Anything new keeps decoding instead of breaking the feed.
data Visibility = Public | Unlisted | Private | Direct | Draft | OtherVisibility String

derive instance Eq Visibility

-- | The wire name, as accepted by `POST /api/v1/statuses`.
visibilityName :: Visibility -> String
visibilityName = case _ of
  Public -> "public"
  Unlisted -> "unlisted"
  Private -> "private"
  Direct -> "direct"
  Draft -> "draft"
  OtherVisibility other -> other

-- | `pf_type` as set by `Status::setType()` on the server.
data PfType
  = Photo
  | PhotoAlbum
  | Video
  | VideoAlbum
  | PhotoVideoAlbum
  | Share
  | TextPost
  | OtherType String

derive instance Eq PfType

data MediaType = Image | VideoMedia | Gifv | Audio | UnknownMedia

derive instance Eq MediaType

type Account =
  { id :: AccountId
  , username :: String
  , acct :: String
  , displayName :: String
  , note :: String
  , noteText :: Maybe String
  , url :: String
  , avatar :: String
  , locked :: Boolean
  , followersCount :: Int
  , followingCount :: Int
  , statusesCount :: Int
  , local :: Boolean
  , isAdmin :: Boolean
  , createdAt :: String
  , website :: Maybe String
  , headerBg :: Maybe String
  , pronouns :: Array String
  , hasStory :: Boolean
  }

type Dimensions = { width :: Int, height :: Int }

type MediaAttachment =
  { id :: MediaId
  , mediaType :: MediaType
  , url :: String
  , previewUrl :: Maybe String
  , optimizedUrl :: Maybe String
  , description :: Maybe String
  , blurhash :: Maybe String
  , isNsfw :: Boolean
  , mime :: Maybe String
  , dimensions :: Maybe Dimensions
  }

type Tag = { name :: String, url :: String }

type Mention = { id :: AccountId, username :: String, acct :: String, url :: String }

-- | `LikeService::likedBy`: empty when nobody the viewer knows liked the post.
type LikedBy =
  { id :: Maybe AccountId
  , username :: Maybe String
  , others :: Boolean
  , totalCount :: Int
  }

type Place = { name :: String, country :: Maybe String }

-- | A newtype because `reblog` refers back to `Status`.
newtype Status = Status StatusRecord

type StatusRecord =
  { id :: StatusId
  , uri :: String
  , url :: Maybe String
  , createdAt :: String
  , editedAt :: Maybe String
  , account :: Account
  , content :: String
  , contentText :: Maybe String
  , visibility :: Visibility
  , sensitive :: Boolean
  , spoilerText :: String
  , pfType :: PfType
  , mediaAttachments :: Array MediaAttachment
  , favouritesCount :: Int
  , reblogsCount :: Int
  , replyCount :: Int
  , favourited :: Boolean
  , reblogged :: Boolean
  , bookmarked :: Boolean
  , commentsDisabled :: Boolean
  , inReplyToId :: Maybe StatusId
  , inReplyToAccountId :: Maybe AccountId
  , reblog :: Maybe Status
  , tags :: Array Tag
  , mentions :: Array Mention
  , likedBy :: Maybe LikedBy
  , place :: Maybe Place
  , local :: Boolean
  , pinned :: Boolean
  }

-- | `RelationshipTransformer` / `RelationshipService::get`: how the viewer
-- | stands to one account.
type Relationship =
  { id :: AccountId
  , following :: Boolean
  , followedBy :: Boolean
  , blocking :: Boolean
  , muting :: Boolean
  , requested :: Boolean
  }

-- | A hashtag as `GET /api/v1/tags/{name}`, `/api/v1/followed_tags` and the
-- | search endpoint give it. `count` is only sent with `_pe=1`.
type HashtagInfo =
  { name :: String
  , following :: Boolean
  , count :: Maybe Int
  }

-- | One entry of `GET /api/v1.1/discover/posts/hashtags`. `name` is the bare
-- | tag (the server's `hashtag` field), without the leading `#`.
type TrendingTag = { name :: String, total :: Int }

-- | `GET /api/v2/search`. Pixelfed always answers `statuses: []`, so the
-- | client keeps only the two lists that can hold something.
type SearchResults =
  { accounts :: Array Account
  , hashtags :: Array HashtagInfo
  }

-- | What `POST /api/v1/accounts/update_credentials` accepts from the profile
-- | editor. The server limits: display name 30, bio 200, website 120.
type ProfileEdit =
  { displayName :: String
  , note :: String
  , website :: String
  , locked :: Boolean
  }

-- | What `PUT /api/v1/statuses/{id}` accepts. `mediaIds` must list every
-- | attachment the post keeps, in order: the server deletes any it holds
-- | that the list leaves out.
type PostEdit =
  { caption :: String
  , sensitive :: Boolean
  , spoilerText :: String
  , mediaIds :: Array MediaId
  }

newtype MessageId = MessageId String

derive instance Newtype MessageId _
derive newtype instance Eq MessageId
derive newtype instance Ord MessageId
derive newtype instance Show MessageId

-- | One direct message as `/api/v1.1/direct/thread` lists it. `kind` is the
-- | server's `type` (text, emoji, photo, video, story:react and others).
type Message =
  { id :: MessageId
  , isAuthor :: Boolean
  , kind :: String
  , text :: String
  , media :: Maybe String
  , createdAt :: String
  , timeAgo :: String
  , seen :: Boolean
  }

-- | `GET /api/v1.1/direct/thread?pid=`: the other person and one page of
-- | messages, newest first.
type Thread =
  { id :: AccountId
  , name :: String
  , username :: String
  , avatar :: String
  , messages :: Array Message
  }

-- | One entry of `GET /api/v1/conversations`. `accounts` are the other
-- | participants; `lastId` is the last message's id, which the list is
-- | ordered and deduplicated by.
type Conversation =
  { id :: String
  , unread :: Boolean
  , accounts :: Array Account
  , lastId :: String
  , lastText :: String
  , lastAt :: String
  }

-- | What a report is about. `POST /api/v1.1/report` names these `post` and `user`.
data ReportTarget = AboutPost StatusId | AboutAccount AccountId

derive instance Eq ReportTarget

-- | `ApiV1Dot1Controller::REPORT_TYPES`.
data ReportReason
  = Spam
  | Sensitive
  | Abusive
  | Underage
  | Violence
  | Copyright
  | Impersonation
  | Scam
  | Terrorism

derive instance Eq ReportReason

reportReasons :: Array ReportReason
reportReasons = [ Spam, Sensitive, Abusive, Underage, Violence, Copyright, Impersonation, Scam, Terrorism ]

-- | The wire name.
reportReasonName :: ReportReason -> String
reportReasonName = case _ of
  Spam -> "spam"
  Sensitive -> "sensitive"
  Abusive -> "abusive"
  Underage -> "underage"
  Violence -> "violence"
  Copyright -> "copyright"
  Impersonation -> "impersonation"
  Scam -> "scam"
  Terrorism -> "terrorism"

reportReasonLabel :: ReportReason -> String
reportReasonLabel = case _ of
  Spam -> "Spam"
  Sensitive -> "Sensitive content without a warning"
  Abusive -> "Abusive or harmful"
  Underage -> "Underage account"
  Violence -> "Violence"
  Copyright -> "Copyright infringement"
  Impersonation -> "Impersonation"
  Scam -> "Scam or fraud"
  Terrorism -> "Terrorism"

-- | `GET /api/v1/statuses/{id}/context`.
type Context =
  { ancestors :: Array Status
  , descendants :: Array Status
  }

newtype NotificationId = NotificationId String

derive instance Newtype NotificationId _
derive newtype instance Eq NotificationId
derive newtype instance Ord NotificationId
derive newtype instance Show NotificationId

-- | `NotificationTransformer::replaceTypeVerb` output. Unknown verbs are kept.
data NotificationKind
  = NFollow
  | NMention
  | NShare
  | NFavourite
  | NComment
  | NTagged
  | NModlog
  | NDirect
  | NStoryReact
  | NStoryComment
  | NOther String

derive instance Eq NotificationKind

type Tagged = { username :: String, statusId :: StatusId }

-- | One entry of `GET /api/v1/notifications`. `account` is the actor; it and
-- | `status` are optional because the server drops entities it can no longer
-- | load, and because a status that fails to decode must not take the whole
-- | page with it.
type Notification =
  { id :: NotificationId
  , kind :: NotificationKind
  , createdAt :: String
  , account :: Maybe Account
  , status :: Maybe Status
  , tagged :: Maybe Tagged
  }

-- | `GET /api/v1.1/compose/settings`. Sizes are kilobytes, as Laravel's
-- | `max:` rule on uploads counts them.
type ComposeSettings =
  { allowedMediaTypes :: Array String
  , maxCaptionLength :: Int
  , maxFileSizeKb :: Int
  , maxMediaAttachments :: Int
  , maxAltTextLength :: Int
  , defaultVisibility :: Maybe Visibility
  }

-- | What the composer sends to `POST /api/v1/statuses`.
type NewPost =
  { caption :: String
  , mediaIds :: Array MediaId
  , visibility :: Visibility
  , sensitive :: Boolean
  , spoilerText :: String
  , commentsDisabled :: Boolean
  }

statusId :: Status -> StatusId
statusId (Status s) = s.id

-- | Plain caption: `content_text` when the server sent it, otherwise the
-- | HTML `content` reduced to text.
captionOf :: StatusRecord -> String
captionOf s = trim case s.contentText of
  Just t -> t
  Nothing -> stripTags s.content

firstMedia :: StatusRecord -> Maybe MediaAttachment
firstMedia s = Array.head s.mediaAttachments

-- | The status whose media and caption are shown: the boosted one for a
-- | share, the status itself otherwise.
displayed :: Status -> Status
displayed st@(Status s) = case s.reblog of
  Just inner -> inner
  Nothing -> st

isAlbum :: PfType -> Boolean
isAlbum = case _ of
  PhotoAlbum -> true
  VideoAlbum -> true
  PhotoVideoAlbum -> true
  _ -> false

-- | Hashtags compare without case; this is the form lists are keyed by.
tagKey :: String -> String
tagKey = toLower
