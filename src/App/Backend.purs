-- | The seam between the UI and the native shell. Everything the UI may ask
-- | the shell to do is a field of `Backend`; the Tauri implementation lives in
-- | `App.Backend.Tauri`. A browser-only implementation for a server that sets
-- | CORS headers would be a second record of the same type.
module App.Backend
  ( AppError(..)
  , printAppError
  , AccountRef
  , SessionSummary
  , InstanceProbe
  , LoginStarted
  , Method(..)
  , ApiRequest
  , ApiResponse
  , MediaUpload
  , Cursor
  , cursorToString
  , Backend
  , decodeAppError
  , decodeSessionSummary
  , decodeInstanceProbe
  , decodeLoginStarted
  , decodeApiResponse
  , encodeApiRequest
  , decodeUnit
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, fromObject, fromString, isNull, jsonNull)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode as Decode
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object

-- | Mirrors `pixelfed_core::AppError` plus `Decode`, which only the UI raises
-- | when a JSON body does not have the shape the decoders expect.
data AppError
  = NoSession
  | InvalidDomain String
  | Network String
  | Http Int String
  | Protocol String
  | Unsupported String
  | Storage String
  | LoginAborted String
  | Decode String

derive instance Eq AppError

instance Show AppError where
  show = printAppError

printAppError :: AppError -> String
printAppError = case _ of
  NoSession -> "Not logged in."
  InvalidDomain m -> "Invalid server address: " <> m
  Network m -> "Network problem: " <> m
  Http status m -> "The server answered HTTP " <> show status <> ": " <> m
  Protocol m -> "Unexpected server behaviour: " <> m
  Unsupported m -> "Unsupported server: " <> m
  Storage m -> "Local storage problem: " <> m
  LoginAborted m -> "Login did not complete: " <> m
  Decode m -> "Could not read the server's response: " <> m

type AccountRef = { id :: String, acct :: String }

type SessionSummary = { domain :: String, account :: Maybe AccountRef }

type InstanceProbe = { domain :: String, software :: String, version :: String }

type LoginStarted = { authorizeUrl :: String }

data Method = GET | POST | PUT | DELETE

derive instance Eq Method

methodName :: Method -> String
methodName = case _ of
  GET -> "GET"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"

-- | `path` must start with `/api/`; the shell refuses anything else.
type ApiRequest =
  { method :: Method
  , path :: String
  , query :: Array (Tuple String String)
  , body :: Maybe Json
  }

-- | An absolute URL taken from a `Link` header. Only the shell may dereference
-- | it, and it checks the URL stays on the logged-in instance.
newtype Cursor = Cursor String

derive instance Eq Cursor

cursorToString :: Cursor -> String
cursorToString (Cursor s) = s

type ApiResponse =
  { status :: Int
  , body :: Json
  , next :: Maybe Cursor
  , prev :: Maybe Cursor
  }

-- | One file for `POST /api/v2/media`. The bytes cross the bridge as a raw
-- | IPC body; the shell never writes them to disk.
type MediaUpload =
  { bytes :: ArrayBuffer
  , filename :: String
  , mime :: String
  , description :: Maybe String
  }

type Backend =
  { sessionGet :: Aff (Either AppError (Maybe SessionSummary))
  , instanceProbe :: String -> Aff (Either AppError InstanceProbe)
  , loginStart :: String -> Aff (Either AppError LoginStarted)
  , logout :: Aff (Either AppError Unit)
  , apiRequest :: ApiRequest -> Aff (Either AppError ApiResponse)
  , apiFollow :: Cursor -> Aff (Either AppError ApiResponse)
  , mediaUpload :: MediaUpload -> Aff (Either AppError ApiResponse)
  -- | Subscribe to session changes. Returns the unsubscribe effect.
  , onSessionChanged :: (Maybe SessionSummary -> Effect Unit) -> Effect (Effect Unit)
  -- | Subscribe to login failures reported after `loginStart` returned.
  , onLoginFailed :: (AppError -> Effect Unit) -> Effect (Effect Unit)
  }

-- Decoders for what crosses the seam ------------------------------------------

decodeAppError :: Json -> AppError
decodeAppError json = case (decodeJson json :: Either JsonDecodeError (Object.Object Json)) of
  Right obj -> fromMaybe (Protocol "unrecognised error payload") do
    kind :: String <- hushE (obj .: "kind")
    let
      message :: String
      message = fromMaybe "" (hushE (obj .: "message"))
    case kind of
      "no_session" -> Just NoSession
      "invalid_domain" -> Just (InvalidDomain message)
      "network" -> Just (Network message)
      "http" -> Just (Http (fromMaybe 0 (hushE (obj .: "status"))) message)
      "protocol" -> Just (Protocol message)
      "unsupported" -> Just (Unsupported message)
      "storage" -> Just (Storage message)
      "login_aborted" -> Just (LoginAborted message)
      _ -> Nothing
  Left _ -> case (Decode.decodeJson json :: Either JsonDecodeError String) of
    -- Tauri itself rejects with a plain string, e.g. an unknown command name.
    Right s -> Protocol s
    Left _ -> Protocol "unrecognised error payload"
  where
  hushE :: forall a. Either JsonDecodeError a -> Maybe a
  hushE = case _ of
    Right a -> Just a
    Left _ -> Nothing

decodeSessionSummary :: Json -> Either JsonDecodeError (Maybe SessionSummary)
decodeSessionSummary json
  | isNull json = pure Nothing
  | otherwise = do
      obj <- decodeJson json
      domain <- obj .: "domain"
      account <- obj .:? "account" >>= case _ of
        Nothing -> pure Nothing
        Just accJson -> do
          acc <- decodeJson accJson
          id <- acc .: "id"
          acct <- acc .: "acct"
          pure (Just { id, acct })
      pure (Just { domain, account })

decodeInstanceProbe :: Json -> Either JsonDecodeError InstanceProbe
decodeInstanceProbe json = do
  obj <- decodeJson json
  domain <- obj .: "domain"
  software <- obj .: "software"
  version <- obj .: "version"
  pure { domain, software, version }

decodeLoginStarted :: Json -> Either JsonDecodeError LoginStarted
decodeLoginStarted json = do
  obj <- decodeJson json
  authorizeUrl <- obj .: "authorize_url"
  pure { authorizeUrl }

decodeApiResponse :: Json -> Either JsonDecodeError ApiResponse
decodeApiResponse json = do
  obj <- decodeJson json
  status <- obj .: "status"
  body <- obj .: "body"
  next <- map Cursor <$> obj .:? "next"
  prev <- map Cursor <$> obj .:? "prev"
  pure { status, body, next, prev }

decodeUnit :: Json -> Either JsonDecodeError Unit
decodeUnit _ = pure unit

-- | Shape expected by `pixelfed_core::ApiRequest` (serde): `query` is an array
-- | of two-element arrays, `body` is null when absent.
encodeApiRequest :: ApiRequest -> Json
encodeApiRequest req = fromObject $ Object.fromFoldable
  [ Tuple "method" (fromString (methodName req.method))
  , Tuple "path" (fromString req.path)
  , Tuple "query" (fromArray (map pair req.query))
  , Tuple "body" (fromMaybe jsonNull req.body)
  ]
  where
  pair (Tuple k v) = fromArray [ fromString k, fromString v ]
