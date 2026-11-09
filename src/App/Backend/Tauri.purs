-- | `Backend` implemented over Tauri's IPC. Command names and payload shapes
-- | must match `src-tauri/src/lib.rs`.
module App.Backend.Tauri (backend) where

import Prelude

import App.Backend (AppError(..), Backend, cursorToString, decodeApiResponse, decodeAppError, decodeInstanceProbe, decodeLoginStarted, decodeSessionSummary, decodeUnit, encodeApiRequest)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json, fromObject, fromString, jsonEmptyObject)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Exception (message)
import Foreign.Object as Object

foreign import invokeImpl :: String -> Json -> Effect (Promise Json)

foreign import listenImpl :: String -> (Json -> Effect Unit) -> Effect (Effect Unit)

backend :: Backend
backend =
  { sessionGet: invoke "session_get" jsonEmptyObject decodeSessionSummary
  , instanceProbe: \domain -> invoke "instance_probe" (args [ Tuple "domain" (fromString domain) ]) decodeInstanceProbe
  , loginStart: \domain -> invoke "login_start" (args [ Tuple "domain" (fromString domain) ]) decodeLoginStarted
  , logout: invoke "logout" jsonEmptyObject decodeUnit
  , apiRequest: \req -> invoke "api_request" (args [ Tuple "request" (encodeApiRequest req) ]) decodeApiResponse
  , apiFollow: \cursor -> invoke "api_follow" (args [ Tuple "url" (fromString (cursorToString cursor)) ]) decodeApiResponse
  , onSessionChanged: \handler -> listenImpl "session-changed" \payload ->
      case decodeSessionSummary payload of
        Right summary -> handler summary
        -- A payload the UI cannot read is treated as "logged out" rather than
        -- leaving the UI stuck on a session that may no longer exist.
        Left _ -> handler Nothing
  , onLoginFailed: \handler -> listenImpl "login-failed" (handler <<< decodeAppError)
  }

args :: Array (Tuple String Json) -> Json
args = fromObject <<< Object.fromFoldable

-- | Calls a command and decodes its `Ok` payload. Every failure mode (IPC
-- | rejection, command error, undecodable payload, thrown exception) becomes
-- | an `AppError`.
invoke :: forall a. String -> Json -> (Json -> Either JsonDecodeError a) -> Aff (Either AppError a)
invoke command payload decode = do
  outcome <- attempt (toAffE (invokeImpl command payload))
  pure case outcome of
    Left err -> Left (Protocol (message err))
    Right json -> case decodeOutcome json of
      Left err -> Left (Decode (command <> ": " <> printJsonDecodeError err))
      Right (Left errJson) -> Left (decodeAppError errJson)
      Right (Right value) -> case decode value of
        Left err -> Left (Decode (command <> ": " <> printJsonDecodeError err))
        Right a -> Right a

-- | `{ ok: true, value }` or `{ ok: false, error }`, as produced by the FFI.
decodeOutcome :: Json -> Either JsonDecodeError (Either Json Json)
decodeOutcome json = do
  obj <- decodeJson json
  ok <- obj .: "ok"
  if ok then Right <$> obj .: "value" else Left <$> obj .: "error"
