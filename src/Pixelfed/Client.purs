-- | Typed endpoints over the `Backend` seam. Each function names one Pixelfed
-- | route, exactly as pixelfed-rn calls it (including `_pe=1`, which asks for
-- | the Pixelfed entity rather than the Mastodon-compatible one).
module Pixelfed.Client
  ( Page
  , homeTimeline
  , publicTimeline
  , followPage
  , favourite
  , unfavourite
  , bookmark
  , unbookmark
  , verifyCredentials
  , getStatus
  , getContext
  , postReply
  , deleteStatus
  , getAccount
  , getAccountStatuses
  , accountPageSize
  , getRelationship
  , follow
  , unfollow
  ) where

import Prelude

import App.Backend (AppError(..), ApiResponse, Backend, Cursor, Method(..))
import Data.Argonaut.Core (Json, fromObject, fromString, jsonFalse)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object as Object
import Pixelfed.Decode (decodeAccount, decodeContext, decodeRelationship, decodeRelationships, decodeStatus, decodeStatuses)
import Pixelfed.Entity (Account, AccountId, Context, Relationship, Status, StatusId)

-- | One page of a cursor-paginated list. `next` goes further into the past.
type Page a =
  { items :: Array a
  , next :: Maybe Cursor
  , prev :: Maybe Cursor
  }

pixelfedEntity :: Tuple String String
pixelfedEntity = Tuple "_pe" "1"

pageSize :: Tuple String String
pageSize = Tuple "limit" "20"

-- | Posts per request on a profile; the grid is three wide, so a multiple of three.
accountPageSize :: Int
accountPageSize = 24

-- | GET /api/v1/timelines/home?_pe=1&limit=20
homeTimeline :: Backend -> Aff (Either AppError (Page Status))
homeTimeline backend =
  decodePage decodeStatuses <$> backend.apiRequest
    { method: GET, path: "/api/v1/timelines/home", query: [ pixelfedEntity, pageSize ], body: Nothing }

-- | GET /api/v1/timelines/public?_pe=1&limit=20 (pixelfed-rn calls this the network feed)
publicTimeline :: Backend -> Aff (Either AppError (Page Status))
publicTimeline backend =
  decodePage decodeStatuses <$> backend.apiRequest
    { method: GET, path: "/api/v1/timelines/public", query: [ pixelfedEntity, pageSize ], body: Nothing }

-- | Follows a `next` or `prev` cursor from an earlier page of statuses.
followPage :: Backend -> Cursor -> Aff (Either AppError (Page Status))
followPage backend cursor = decodePage decodeStatuses <$> backend.apiFollow cursor

-- | POST /api/v1/statuses/{id}/favourite?_pe=1, returns the updated status.
favourite :: Backend -> StatusId -> Aff (Either AppError Status)
favourite = statusAction "favourite"

unfavourite :: Backend -> StatusId -> Aff (Either AppError Status)
unfavourite = statusAction "unfavourite"

bookmark :: Backend -> StatusId -> Aff (Either AppError Status)
bookmark = statusAction "bookmark"

unbookmark :: Backend -> StatusId -> Aff (Either AppError Status)
unbookmark = statusAction "unbookmark"

-- | GET /api/v1/accounts/verify_credentials?_pe=1
verifyCredentials :: Backend -> Aff (Either AppError Account)
verifyCredentials backend =
  decodeBody decodeAccount <$> backend.apiRequest
    { method: GET, path: "/api/v1/accounts/verify_credentials", query: [ pixelfedEntity ], body: Nothing }

-- | GET /api/v1/statuses/{id}?_pe=1
getStatus :: Backend -> StatusId -> Aff (Either AppError Status)
getStatus backend id =
  decodeBody decodeStatus <$> backend.apiRequest
    { method: GET, path: "/api/v1/statuses/" <> unwrap id, query: [ pixelfedEntity ], body: Nothing }

-- | GET /api/v1/statuses/{id}/context?_pe=1. Pixelfed returns up to 100
-- | descendants and sends no Link header, so this is one request.
getContext :: Backend -> StatusId -> Aff (Either AppError Context)
getContext backend id =
  decodeBody decodeContext <$> backend.apiRequest
    { method: GET, path: "/api/v1/statuses/" <> unwrap id <> "/context", query: [ pixelfedEntity ], body: Nothing }

-- | POST /api/v1/statuses?_pe=1 with `in_reply_to_id`; returns the new comment.
postReply :: Backend -> StatusId -> String -> Aff (Either AppError Status)
postReply backend parent text =
  decodeBody decodeStatus <$> backend.apiRequest
    { method: POST
    , path: "/api/v1/statuses"
    , query: [ pixelfedEntity ]
    , body: Just $ fromObject $ Object.fromFoldable
        [ Tuple "status" (fromString text)
        , Tuple "in_reply_to_id" (fromString (unwrap parent))
        , Tuple "visibility" (fromString "public")
        , Tuple "sensitive" jsonFalse
        ]
    }

-- | DELETE /api/v1/statuses/{id}. The body (the deleted status) is ignored.
deleteStatus :: Backend -> StatusId -> Aff (Either AppError Unit)
deleteStatus backend id =
  map (const unit) <$> backend.apiRequest
    { method: DELETE, path: "/api/v1/statuses/" <> unwrap id, query: [], body: Nothing }

-- | GET /api/v1/accounts/{id}?_pe=1
getAccount :: Backend -> AccountId -> Aff (Either AppError Account)
getAccount backend id =
  decodeBody decodeAccount <$> backend.apiRequest
    { method: GET, path: "/api/v1/accounts/" <> unwrap id, query: [ pixelfedEntity ], body: Nothing }

-- | GET /api/v1/accounts/{id}/statuses?_pe=1&limit=24[&max_id=...]. This
-- | endpoint sends no Link header; the caller pages with the last id it saw.
getAccountStatuses :: Backend -> AccountId -> Maybe StatusId -> Aff (Either AppError (Array Status))
getAccountStatuses backend id maxId =
  decodeBody decodeStatuses <$> backend.apiRequest
    { method: GET
    , path: "/api/v1/accounts/" <> unwrap id <> "/statuses"
    , query:
        [ pixelfedEntity, Tuple "limit" (show accountPageSize) ]
          <> case maxId of
            Just m -> [ Tuple "max_id" (unwrap m) ]
            Nothing -> []
    , body: Nothing
    }

-- | GET /api/v1/accounts/relationships?id[]={id}&_pe=1. `Nothing` when the
-- | server returns no entry, which it does for the viewer's own account.
getRelationship :: Backend -> AccountId -> Aff (Either AppError (Maybe Relationship))
getRelationship backend id =
  map Array.head <<< decodeBody decodeRelationships <$> backend.apiRequest
    { method: GET
    , path: "/api/v1/accounts/relationships"
    , query: [ Tuple "id[]" (unwrap id), pixelfedEntity ]
    , body: Nothing
    }

-- | POST /api/v1/accounts/{id}/follow; returns the new relationship, with
-- | `requested` set when the account is locked.
follow :: Backend -> AccountId -> Aff (Either AppError Relationship)
follow = accountAction "follow"

unfollow :: Backend -> AccountId -> Aff (Either AppError Relationship)
unfollow = accountAction "unfollow"

statusAction :: String -> Backend -> StatusId -> Aff (Either AppError Status)
statusAction verb backend id =
  decodeBody decodeStatus <$> backend.apiRequest
    { method: POST
    , path: "/api/v1/statuses/" <> unwrap id <> "/" <> verb
    , query: [ pixelfedEntity ]
    , body: Nothing
    }

accountAction :: String -> Backend -> AccountId -> Aff (Either AppError Relationship)
accountAction verb backend id =
  decodeBody decodeRelationship <$> backend.apiRequest
    { method: POST
    , path: "/api/v1/accounts/" <> unwrap id <> "/" <> verb
    , query: []
    , body: Nothing
    }

decodeBody :: forall a. (Json -> Either JsonDecodeError a) -> Either AppError ApiResponse -> Either AppError a
decodeBody decode = case _ of
  Left err -> Left err
  Right response -> case decode response.body of
    Left err -> Left (Decode (printJsonDecodeError err))
    Right a -> Right a

decodePage :: forall a. (Json -> Either JsonDecodeError (Array a)) -> Either AppError ApiResponse -> Either AppError (Page a)
decodePage decode = case _ of
  Left err -> Left err
  Right response -> case decode response.body of
    Left err -> Left (Decode (printJsonDecodeError err))
    Right items -> Right { items, next: response.next, prev: response.prev }
