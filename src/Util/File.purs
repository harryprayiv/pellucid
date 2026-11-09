-- | Reading a picked file's bytes. The one foreign call is `file.arrayBuffer()`.
module Util.File
  ( readBytes
  , mimeOf
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (fromMaybe)
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Web.File.File (File)
import Web.File.File as File

foreign import arrayBufferImpl :: File -> Effect (Promise ArrayBuffer)

readBytes :: File -> Aff ArrayBuffer
readBytes = toAffE <<< arrayBufferImpl

-- | The browser's MIME type for the file, or `application/octet-stream`.
mimeOf :: File -> String
mimeOf file = case fromMaybe (MediaType "") (File.type_ file) of
  MediaType "" -> "application/octet-stream"
  MediaType t -> t
