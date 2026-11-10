-- | Typed endpoints over the `Backend` seam. Each function names one Pixelfed
-- | route, exactly as pixelfed-rn calls it (including `_pe=1`, which asks for
-- | the Pixelfed entity rather than the Mastodon-compatible one).
-- |
-- | Lists are exposed as a `Pager`: a function that fetches one chunk and,
-- | when the server offers more, the pager for the chunk after it. Pixelfed
-- | paginates in four different ways (see `linkPager`, `maxIdPager` and
-- | `tagPager`); a screen sees none of them.
module Pixelfed.Client
  ( Pager(..)
  , Chunk
  , runPager
  , homePager
  , publicPager
  , bookmarksPager
  , favouritesPager
  , tagPager
  , accountStatusesPager
  , accountPageSize
  , notificationsPager
  , followersPager
  , followingPager
  , likedByPager
  , sharedByPager
  , mutesPager
  , blocksPager
  , followedTagsPager
  , conversationsPager
  , threadPager
  , followRequests
  , authorizeFollowRequest
  , rejectFollowRequest
  , favourite
  , unfavourite
  , bookmark
  , unbookmark
  , reblog
  , unreblog
  , verifyCredentials
  , updateCredentials
  , getStatus
  , getContext
  , postReply
  , deleteStatus
  , editStatus
  , getAccount
  , getRelationship
  , follow
  , unfollow
  , block
  , unblock
  , mute
  , unmute
  , search
  , trendingPosts
  , TrendingRange(..)
  , popularAccounts
  , trendingTags
  , getTag
  , followTag
  , unfollowTag
  , sendMessage
  , deleteMessage
  , report
  , composeSettings
  , uploadMedia
  , updateMediaDescription
  , createPost
  ) where

import Prelude

import App.Backend (AppError(..), ApiRequest, ApiResponse, Backend, Cursor, MediaUpload, Method(..))
import Data.Argonaut.Core (Json, fromArray, fromBoolean, fromObject, fromString, jsonFalse)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Pixelfed.Decode (decodeAccount, decodeAccounts, decodeComposeSettings, decodeContext, decodeConversations, decodeHashtag, decodeHashtags, decodeMedia, decodeMessage, decodeNotifications, decodeRelationship, decodeRelationships, decodeSearch, decodeStatus, decodeStatuses, decodeThread, decodeTrendingTags)
import Pixelfed.Entity (Account, AccountId, ComposeSettings, Context, Conversation, HashtagInfo, MediaAttachment, MediaId, Message, MessageId, NewPost, Notification, PostEdit, ProfileEdit, Relationship, ReportReason, ReportTarget(..), SearchResults, Status, StatusId, TrendingTag, reportReasonName, statusId, visibilityName)

-- Pagination ------------------------------------------------------------------

-- | One chunk of a list and, when there is more, how to fetch what follows.
type Chunk a =
  { items :: Array a
  , next :: Maybe (Pager a)
  }

-- | A list that is fetched a chunk at a time. `next` always goes further
-- | into the list (older posts, more accounts), whatever the server calls
-- | that direction.
newtype Pager a = Pager (Backend -> Aff (Either AppError (Chunk a)))

runPager :: forall a. Pager a -> Backend -> Aff (Either AppError (Chunk a))
runPager (Pager run) = run

-- | Which `Link` relation leads further into a list. Timelines, bookmarks,
-- | notifications, mutes and blocks use `next`. The cursor-paginated account
-- | lists (followers, following, liked by, shared by) label the same
-- | direction `prev`: `ApiV1Controller` writes Laravel's `nextPageUrl()` under
-- | `rel="prev"`.
data Further = ViaNext | ViaPrev

-- | A list paginated by the `Link` header. The cursor is followed by the
-- | shell after it has checked the URL stays on the logged-in instance.
linkPager :: forall a. Further -> (Json -> Either JsonDecodeError (Array a)) -> ApiRequest -> Pager a
linkPager further decode req = Pager \backend -> chunk <$> backend.apiRequest req
  where
  chunk :: Either AppError ApiResponse -> Either AppError (Chunk a)
  chunk result = do
    response <- result
    items <- decodeWith decode response.body
    pure { items, next: map followCursor (pick response) }

  pick :: ApiResponse -> Maybe Cursor
  pick response = case further of
    ViaNext -> response.next
    ViaPrev -> response.prev

  followCursor :: Cursor -> Pager a
  followCursor cursor = Pager \backend -> chunk <$> backend.apiFollow cursor

-- | A list whose endpoint sends no `Link` header: the next chunk is asked
-- | for with `max_id` set to the id of the last item held. An empty chunk
-- | ends the list.
maxIdPager
  :: forall a
   . (a -> String)
  -> (Json -> Either JsonDecodeError (Array a))
  -> (Maybe String -> ApiRequest)
  -> Pager a
maxIdPager idOf decode request = go Nothing
  where
  go :: Maybe String -> Pager a
  go maxId = Pager \backend -> do
    result <- backend.apiRequest (request maxId)
    pure do
      response <- result
      items <- decodeWith decode response.body
      pure { items, next: map (go <<< Just <<< idOf) (Array.last items) }

pixelfedEntity :: Tuple String String
pixelfedEntity = Tuple "_pe" "1"

pageSize :: Tuple String String
pageSize = Tuple "limit" "20"

get :: String -> Array (Tuple String String) -> ApiRequest
get path query = { method: GET, path, query, body: Nothing }

post :: String -> Array (Tuple String String) -> Maybe Json -> ApiRequest
post path query body = { method: POST, path, query, body }

object :: Array (Tuple String Json) -> Json
object = fromObject <<< Object.fromFoldable

-- Timelines -------------------------------------------------------------------

-- | GET /api/v1/timelines/home?_pe=1&limit=20
homePager :: Pager Status
homePager = linkPager ViaNext decodeStatuses (get "/api/v1/timelines/home" [ pixelfedEntity, pageSize ])

-- | GET /api/v1/timelines/public?_pe=1&limit=20 (pixelfed-rn calls this the network feed)
publicPager :: Pager Status
publicPager = linkPager ViaNext decodeStatuses (get "/api/v1/timelines/public" [ pixelfedEntity, pageSize ])

-- | GET /api/v1/bookmarks?_pe=1&limit=20, cursor-paginated with `rel="next"`.
bookmarksPager :: Pager Status
bookmarksPager = linkPager ViaNext decodeStatuses (get "/api/v1/bookmarks" [ pixelfedEntity, pageSize ])

-- | GET /api/v1/favourites?limit=20. This endpoint ignores `_pe` and always
-- | answers with the Mastodon entity, so `pf_type` and `content_text` are
-- | absent and the decoders fall back.
favouritesPager :: Pager Status
favouritesPager = linkPager ViaNext decodeStatuses (get "/api/v1/favourites" [ pageSize ])

-- | GET /api/v1/timelines/tag/{name}?_pe=1&limit=20. Pixelfed 0.15 sends a
-- | `Link` header with an opaque cursor and answers `[]` to a bare `max_id`;
-- | 0.12 sends no `Link` header and pages by `max_id`. The pager follows the
-- | header when there is one and falls back to `max_id` when there is not, so
-- | against a newer server the list ends with one request that comes back
-- | empty.
tagPager :: String -> Pager Status
tagPager name = Pager \backend -> step <$> backend.apiRequest (request Nothing)
  where
  request :: Maybe StatusId -> ApiRequest
  request maxId = get ("/api/v1/timelines/tag/" <> name)
    ( [ pixelfedEntity, pageSize ] <> case maxId of
        Just m -> [ Tuple "max_id" (unwrap m) ]
        Nothing -> []
    )

  step :: Either AppError ApiResponse -> Either AppError (Chunk Status)
  step result = do
    response <- result
    items <- decodeWith decodeStatuses response.body
    pure
      { items
      , next: case response.next of
          Just cursor -> Just (Pager \backend -> step <$> backend.apiFollow cursor)
          Nothing -> map (\last -> Pager \backend -> step <$> backend.apiRequest (request (Just (statusId last))))
            (Array.last items)
      }

-- | Posts per request on a profile; the grid is three wide, so a multiple of three.
accountPageSize :: Int
accountPageSize = 24

-- | GET /api/v1/accounts/{id}/statuses?_pe=1&limit=24[&max_id=...]. This
-- | endpoint sends no Link header.
accountStatusesPager :: AccountId -> Pager Status
accountStatusesPager id = maxIdPager (unwrap <<< statusId) decodeStatuses \maxId ->
  get ("/api/v1/accounts/" <> unwrap id <> "/statuses")
    ( [ pixelfedEntity, Tuple "limit" (show accountPageSize) ] <> case maxId of
        Just m -> [ Tuple "max_id" m ]
        Nothing -> []
    )

-- | GET /api/v1/notifications?_pe=1&limit=20. Paginated by the Link header;
-- | the server caps `limit` at 40.
notificationsPager :: Pager Notification
notificationsPager = linkPager ViaNext decodeNotifications (get "/api/v1/notifications" [ pixelfedEntity, pageSize ])

-- Account lists ---------------------------------------------------------------

-- | GET /api/v1/accounts/{id}/followers?_pe=1&limit=20. A locked account the
-- | viewer does not follow, or one that hides its followers, yields `[]`.
followersPager :: AccountId -> Pager Account
followersPager id = linkPager ViaPrev decodeAccounts
  (get ("/api/v1/accounts/" <> unwrap id <> "/followers") [ pixelfedEntity, pageSize ])

-- | GET /api/v1/accounts/{id}/following?_pe=1&limit=20
followingPager :: AccountId -> Pager Account
followingPager id = linkPager ViaPrev decodeAccounts
  (get ("/api/v1/accounts/" <> unwrap id <> "/following") [ pixelfedEntity, pageSize ])

-- | GET /api/v1/statuses/{id}/favourited_by?_pe=1&limit=20. Only the author
-- | of the post is given a `Link` header; everyone else sees the first page.
likedByPager :: StatusId -> Pager Account
likedByPager id = linkPager ViaPrev decodeAccounts
  (get ("/api/v1/statuses/" <> unwrap id <> "/favourited_by") [ pixelfedEntity, pageSize ])

-- | GET /api/v1/statuses/{id}/reblogged_by?_pe=1&limit=20, same rule.
sharedByPager :: StatusId -> Pager Account
sharedByPager id = linkPager ViaPrev decodeAccounts
  (get ("/api/v1/statuses/" <> unwrap id <> "/reblogged_by") [ pixelfedEntity, pageSize ])

-- | GET /api/v1/mutes?limit=40, page-number pagination behind `rel="next"`.
mutesPager :: Pager Account
mutesPager = linkPager ViaNext decodeAccounts (get "/api/v1/mutes" [ Tuple "limit" "40" ])

-- | GET /api/v1/blocks?limit=40
blocksPager :: Pager Account
blocksPager = linkPager ViaNext decodeAccounts (get "/api/v1/blocks" [ Tuple "limit" "40" ])

-- | GET /api/v1/follow_requests?limit=40. No pagination: one request.
followRequests :: Pager Account
followRequests = Pager \backend -> do
  result <- backend.apiRequest (get "/api/v1/follow_requests" [ Tuple "limit" "40" ])
  pure do
    response <- result
    items <- decodeWith decodeAccounts response.body
    pure { items, next: Nothing }

-- | POST /api/v1/follow_requests/{id}/authorize; answers with the relationship.
authorizeFollowRequest :: Backend -> AccountId -> Aff (Either AppError Relationship)
authorizeFollowRequest backend id =
  decodeBody decodeRelationship <$> backend.apiRequest
    (post ("/api/v1/follow_requests/" <> unwrap id <> "/authorize") [] Nothing)

-- | POST /api/v1/follow_requests/{id}/reject
rejectFollowRequest :: Backend -> AccountId -> Aff (Either AppError Relationship)
rejectFollowRequest backend id =
  decodeBody decodeRelationship <$> backend.apiRequest
    (post ("/api/v1/follow_requests/" <> unwrap id <> "/reject") [] Nothing)

-- Hashtags --------------------------------------------------------------------

-- | GET /api/v1/followed_tags?limit=100, cursor-paginated with `rel="next"`.
followedTagsPager :: Pager HashtagInfo
followedTagsPager = linkPager ViaNext decodeHashtags (get "/api/v1/followed_tags" [ Tuple "limit" "100" ])

-- | GET /api/v1/tags/{name}?_pe=1. An unknown tag answers with
-- | `following: false` and no count rather than 404.
getTag :: Backend -> String -> Aff (Either AppError HashtagInfo)
getTag backend name =
  decodeBody decodeHashtag <$> backend.apiRequest (get ("/api/v1/tags/" <> name) [ pixelfedEntity ])

-- | POST /api/v1/tags/{name}/follow. 422 for a tag no post has used yet.
followTag :: Backend -> String -> Aff (Either AppError HashtagInfo)
followTag backend name =
  decodeBody decodeHashtag <$> backend.apiRequest (post ("/api/v1/tags/" <> name <> "/follow") [] Nothing)

-- | POST /api/v1/tags/{name}/unfollow
unfollowTag :: Backend -> String -> Aff (Either AppError HashtagInfo)
unfollowTag backend name =
  decodeBody decodeHashtag <$> backend.apiRequest (post ("/api/v1/tags/" <> name <> "/unfollow") [] Nothing)

-- Search and discover ---------------------------------------------------------

-- | GET /api/v2/search?q=...&_pe=1&resolve=1. `resolve` lets the server look
-- | up a full handle or URL on another instance. Statuses are never returned.
search :: Backend -> String -> Aff (Either AppError SearchResults)
search backend query =
  decodeBody decodeSearch <$> backend.apiRequest
    (get "/api/v2/search" [ Tuple "q" query, pixelfedEntity, Tuple "resolve" "1" ])

data TrendingRange = Daily | Monthly | Yearly

derive instance Eq TrendingRange

-- | GET /api/v1.1/discover/posts/trending?range=...: up to 30 local public
-- | posts ordered by likes, always as the Pixelfed entity.
trendingPosts :: Backend -> TrendingRange -> Aff (Either AppError (Array Status))
trendingPosts backend range =
  decodeBody decodeStatuses <$> backend.apiRequest
    ( get "/api/v1.1/discover/posts/trending"
        [ Tuple "range" case range of
            Daily -> "daily"
            Monthly -> "monthly"
            Yearly -> "yearly"
        ]
    )

-- | GET /api/v1.1/discover/accounts/popular: up to 16 accounts the viewer
-- | does not follow yet.
popularAccounts :: Backend -> Aff (Either AppError (Array Account))
popularAccounts backend =
  decodeBody decodeAccounts <$> backend.apiRequest (get "/api/v1.1/discover/accounts/popular" [])

-- | GET /api/v1.1/discover/posts/hashtags
trendingTags :: Backend -> Aff (Either AppError (Array TrendingTag))
trendingTags backend =
  decodeBody decodeTrendingTags <$> backend.apiRequest (get "/api/v1.1/discover/posts/hashtags" [])

-- Direct messages -------------------------------------------------------------

-- | GET /api/v1/conversations?limit=20, paginated by the Link header.
conversationsPager :: Pager Conversation
conversationsPager = linkPager ViaNext decodeConversations (get "/api/v1/conversations" [ pageSize ])

-- | GET /api/v1.1/direct/thread?pid={account}[&max_id=...]: eight messages a
-- | request, newest first, older ones by `max_id`.
threadPager :: AccountId -> Pager Message
threadPager account = maxIdPager (unwrap <<< _.id) (map _.messages <<< decodeThread) \maxId ->
  get "/api/v1.1/direct/thread"
    ( [ Tuple "pid" (unwrap account) ] <> case maxId of
        Just m -> [ Tuple "max_id" m ]
        Nothing -> []
    )

-- | POST /api/v1.1/direct/thread/send; answers with the message as stored.
sendMessage :: Backend -> AccountId -> String -> Aff (Either AppError Message)
sendMessage backend to text =
  decodeBody decodeMessage <$> backend.apiRequest
    ( post "/api/v1.1/direct/thread/send" []
        ( Just $ object
            [ Tuple "to_id" (fromString (unwrap to))
            , Tuple "message" (fromString text)
            , Tuple "type" (fromString "text")
            ]
        )
    )

-- | DELETE /api/v1.1/direct/thread/message?id=...; only the author may.
deleteMessage :: Backend -> MessageId -> Aff (Either AppError Unit)
deleteMessage backend id =
  map (const unit) <$> backend.apiRequest
    { method: DELETE, path: "/api/v1.1/direct/thread/message", query: [ Tuple "id" (unwrap id) ], body: Nothing }

-- Moderation ------------------------------------------------------------------

-- | POST /api/v1.1/report. The server refuses a report on the viewer's own
-- | content and a second report on the same object, both with HTTP 400.
report :: Backend -> ReportTarget -> ReportReason -> String -> Aff (Either AppError Unit)
report backend target reason message =
  map (const unit) <$> backend.apiRequest
    ( post "/api/v1.1/report" []
        ( Just $ object $
            [ Tuple "report_type" (fromString (reportReasonName reason))
            , Tuple "object_type" (fromString objectType)
            , Tuple "object_id" (fromString objectId)
            ]
              <>
                if trim message == "" then []
                else [ Tuple "message" (fromString (trim message)) ]
        )
    )
  where
  objectType = case target of
    AboutPost _ -> "post"
    AboutAccount _ -> "user"
  objectId = case target of
    AboutPost id -> unwrap id
    AboutAccount id -> unwrap id

-- Compose ---------------------------------------------------------------------

-- | GET /api/v1.1/compose/settings: the instance's upload limits.
composeSettings :: Backend -> Aff (Either AppError ComposeSettings)
composeSettings backend =
  decodeBody decodeComposeSettings <$> backend.apiRequest (get "/api/v1.1/compose/settings" [])

-- | POST /api/v2/media (multipart, done in Rust); answers with the attachment.
uploadMedia :: Backend -> MediaUpload -> Aff (Either AppError MediaAttachment)
uploadMedia backend upload = decodeBody decodeMedia <$> backend.mediaUpload upload

-- | PUT /api/v1/media/{id} with `description`; answers with the attachment.
updateMediaDescription :: Backend -> MediaId -> String -> Aff (Either AppError MediaAttachment)
updateMediaDescription backend id description =
  decodeBody decodeMedia <$> backend.apiRequest
    { method: PUT
    , path: "/api/v1/media/" <> unwrap id
    , query: []
    , body: Just $ object [ Tuple "description" (fromString description) ]
    }

-- | POST /api/v1/statuses?_pe=1 with media. The server refuses a post with
-- | neither media nor `in_reply_to_id`, so the caller must have uploaded first.
createPost :: Backend -> NewPost -> Aff (Either AppError Status)
createPost backend new =
  decodeBody decodeStatus <$> backend.apiRequest
    ( post "/api/v1/statuses" [ pixelfedEntity ]
        ( Just $ object $
            [ Tuple "status" (fromString new.caption)
            , Tuple "media_ids" (fromArray (map (fromString <<< unwrap) new.mediaIds))
            , Tuple "visibility" (fromString (visibilityName new.visibility))
            , Tuple "sensitive" (fromBoolean new.sensitive)
            , Tuple "comments_disabled" (fromBoolean new.commentsDisabled)
            ]
              <>
                if trim new.spoilerText == "" then []
                else [ Tuple "spoiler_text" (fromString (trim new.spoilerText)) ]
        )
    )

-- | PUT /api/v1/statuses/{id}; answers with the Pixelfed entity. The server
-- | allows ten edits a post. `media_ids` is always sent in full: for an album
-- | the server deletes every attachment the list leaves out.
editStatus :: Backend -> StatusId -> PostEdit -> Aff (Either AppError Status)
editStatus backend id edit =
  decodeBody decodeStatus <$> backend.apiRequest
    { method: PUT
    , path: "/api/v1/statuses/" <> unwrap id
    , query: []
    , body: Just $ object
        [ Tuple "status" (fromString edit.caption)
        , Tuple "sensitive" (fromBoolean edit.sensitive)
        , Tuple "spoiler_text" (fromString (trim edit.spoilerText))
        , Tuple "media_ids" (fromArray (map (fromString <<< unwrap) edit.mediaIds))
        ]
    }

-- Statuses --------------------------------------------------------------------

-- | POST /api/v1/statuses/{id}/favourite?_pe=1, returns the updated status.
favourite :: Backend -> StatusId -> Aff (Either AppError Status)
favourite = statusAction "favourite"

unfavourite :: Backend -> StatusId -> Aff (Either AppError Status)
unfavourite = statusAction "unfavourite"

bookmark :: Backend -> StatusId -> Aff (Either AppError Status)
bookmark = statusAction "bookmark"

unbookmark :: Backend -> StatusId -> Aff (Either AppError Status)
unbookmark = statusAction "unbookmark"

-- | POST /api/v1/statuses/{id}/reblog. Only public posts can be shared; the
-- | answer is the Mastodon entity of the post with `reblogged` set.
reblog :: Backend -> StatusId -> Aff (Either AppError Status)
reblog = statusAction "reblog"

unreblog :: Backend -> StatusId -> Aff (Either AppError Status)
unreblog = statusAction "unreblog"

-- | GET /api/v1/statuses/{id}?_pe=1
getStatus :: Backend -> StatusId -> Aff (Either AppError Status)
getStatus backend id =
  decodeBody decodeStatus <$> backend.apiRequest (get ("/api/v1/statuses/" <> unwrap id) [ pixelfedEntity ])

-- | GET /api/v1/statuses/{id}/context?_pe=1. Pixelfed returns up to 100
-- | descendants and sends no Link header, so this is one request.
getContext :: Backend -> StatusId -> Aff (Either AppError Context)
getContext backend id =
  decodeBody decodeContext <$> backend.apiRequest
    (get ("/api/v1/statuses/" <> unwrap id <> "/context") [ pixelfedEntity ])

-- | POST /api/v1/statuses?_pe=1 with `in_reply_to_id`; returns the new comment.
postReply :: Backend -> StatusId -> String -> Aff (Either AppError Status)
postReply backend parent text =
  decodeBody decodeStatus <$> backend.apiRequest
    ( post "/api/v1/statuses" [ pixelfedEntity ]
        ( Just $ object
            [ Tuple "status" (fromString text)
            , Tuple "in_reply_to_id" (fromString (unwrap parent))
            , Tuple "visibility" (fromString "public")
            , Tuple "sensitive" jsonFalse
            ]
        )
    )

-- | DELETE /api/v1/statuses/{id}. The body (the deleted status) is ignored.
deleteStatus :: Backend -> StatusId -> Aff (Either AppError Unit)
deleteStatus backend id =
  map (const unit) <$> backend.apiRequest
    { method: DELETE, path: "/api/v1/statuses/" <> unwrap id, query: [], body: Nothing }

-- Accounts --------------------------------------------------------------------

-- | GET /api/v1/accounts/verify_credentials?_pe=1
verifyCredentials :: Backend -> Aff (Either AppError Account)
verifyCredentials backend =
  decodeBody decodeAccount <$> backend.apiRequest (get "/api/v1/accounts/verify_credentials" [ pixelfedEntity ])

-- | POST /api/v1/accounts/update_credentials?_pe=1; answers with the account.
-- | Every field is sent, so clearing one clears it on the server.
updateCredentials :: Backend -> ProfileEdit -> Aff (Either AppError Account)
updateCredentials backend edit =
  decodeBody decodeAccount <$> backend.apiRequest
    ( post "/api/v1/accounts/update_credentials" [ pixelfedEntity ]
        ( Just $ object
            [ Tuple "display_name" (fromString (trim edit.displayName))
            , Tuple "note" (fromString (trim edit.note))
            , Tuple "website" (fromString (trim edit.website))
            , Tuple "locked" (fromBoolean edit.locked)
            ]
        )
    )

-- | GET /api/v1/accounts/{id}?_pe=1
getAccount :: Backend -> AccountId -> Aff (Either AppError Account)
getAccount backend id =
  decodeBody decodeAccount <$> backend.apiRequest (get ("/api/v1/accounts/" <> unwrap id) [ pixelfedEntity ])

-- | GET /api/v1/accounts/relationships?id[]={id}&_pe=1. `Nothing` when the
-- | server returns no entry, which it does for the viewer's own account.
getRelationship :: Backend -> AccountId -> Aff (Either AppError (Maybe Relationship))
getRelationship backend id =
  map Array.head <<< decodeBody decodeRelationships <$> backend.apiRequest
    (get "/api/v1/accounts/relationships" [ Tuple "id[]" (unwrap id), pixelfedEntity ])

-- | POST /api/v1/accounts/{id}/follow; returns the new relationship, with
-- | `requested` set when the account is locked.
follow :: Backend -> AccountId -> Aff (Either AppError Relationship)
follow = accountAction "follow"

unfollow :: Backend -> AccountId -> Aff (Either AppError Relationship)
unfollow = accountAction "unfollow"

-- | POST /api/v1/accounts/{id}/block; a block also removes the follow in
-- | both directions on the server.
block :: Backend -> AccountId -> Aff (Either AppError Relationship)
block = accountAction "block"

unblock :: Backend -> AccountId -> Aff (Either AppError Relationship)
unblock = accountAction "unblock"

-- | POST /api/v1/accounts/{id}/mute
mute :: Backend -> AccountId -> Aff (Either AppError Relationship)
mute = accountAction "mute"

unmute :: Backend -> AccountId -> Aff (Either AppError Relationship)
unmute = accountAction "unmute"

statusAction :: String -> Backend -> StatusId -> Aff (Either AppError Status)
statusAction verb backend id =
  decodeBody decodeStatus <$> backend.apiRequest
    (post ("/api/v1/statuses/" <> unwrap id <> "/" <> verb) [ pixelfedEntity ] Nothing)

accountAction :: String -> Backend -> AccountId -> Aff (Either AppError Relationship)
accountAction verb backend id =
  decodeBody decodeRelationship <$> backend.apiRequest
    (post ("/api/v1/accounts/" <> unwrap id <> "/" <> verb) [] Nothing)

decodeWith :: forall a. (Json -> Either JsonDecodeError a) -> Json -> Either AppError a
decodeWith decode json = case decode json of
  Left err -> Left (Decode (printJsonDecodeError err))
  Right a -> Right a

decodeBody :: forall a. (Json -> Either JsonDecodeError a) -> Either AppError ApiResponse -> Either AppError a
decodeBody decode result = result >>= \response -> decodeWith decode response.body
