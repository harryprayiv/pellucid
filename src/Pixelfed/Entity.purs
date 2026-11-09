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
  , statusId
  , displayed
  , isAlbum
  , captionOf
  , firstMedia
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (trim)
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

-- | `GET /api/v1/statuses/{id}/context`.
type Context =
  { ancestors :: Array Status
  , descendants :: Array Status
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
