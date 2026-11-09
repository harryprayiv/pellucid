-- | Typed endpoints over the `Backend` seam. Each function names one Pellucid
-- | route, exactly as pellucid-rn calls it (including `_pe=1`, which asks for
-- | the Pellucid entity rather than the Mastodon-compatible one).
module Pellucid.Client
  ( Page
  , homeTimeline
  , publicTimeline
  , followPage
  , favourite
  , unfavourite
  , bookmark
  , unbookmark
  , verifyCredentials
  ) where

import Prelude

import App.Backend (AppError(..), ApiResponse, Backend, Cursor, Method(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Pellucid.Decode (decodeAccount, decodeStatus, decodeStatuses)
import Pellucid.Entity (Account, Status, StatusId)

-- | One page of a cursor-paginated list. `next` goes further into the past.
type Page a =
  { items :: Array a
  , next :: Maybe Cursor
  , prev :: Maybe Cursor
  }

pellucidEntity :: Tuple String String
pellucidEntity = Tuple "_pe" "1"

pageSize :: Tuple String String
pageSize = Tuple "limit" "20"

-- | GET /api/v1/timelines/home?_pe=1&limit=20
homeTimeline :: Backend -> Aff (Either AppError (Page Status))
homeTimeline backend =
  decodePage decodeStatuses <$> backend.apiRequest
    { method: GET, path: "/api/v1/timelines/home", query: [ pellucidEntity, pageSize ], body: Nothing }

-- | GET /api/v1/timelines/public?_pe=1&limit=20 (pellucid-rn calls this the network feed)
publicTimeline :: Backend -> Aff (Either AppError (Page Status))
publicTimeline backend =
  decodePage decodeStatuses <$> backend.apiRequest
    { method: GET, path: "/api/v1/timelines/public", query: [ pellucidEntity, pageSize ], body: Nothing }

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
    { method: GET, path: "/api/v1/accounts/verify_credentials", query: [ pellucidEntity ], body: Nothing }

statusAction :: String -> Backend -> StatusId -> Aff (Either AppError Status)
statusAction verb backend id =
  decodeBody decodeStatus <$> backend.apiRequest
    { method: POST
    , path: "/api/v1/statuses/" <> unwrap id <> "/" <> verb
    , query: [ pellucidEntity ]
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
