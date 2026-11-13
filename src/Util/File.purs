-- | Picked files: taking them out of a file input, and reading one's bytes.
-- | The two foreign calls are the input's `files` with `value = ""`, and
-- | `file.arrayBuffer()`.
module Util.File
  ( takeFiles
  , readBytes
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
import Web.HTML.HTMLInputElement (HTMLInputElement)

foreign import takeFilesImpl :: HTMLInputElement -> Effect (Array File)

foreign import arrayBufferImpl :: File -> Effect (Promise ArrayBuffer)

-- | The files a file input holds, taken out of it. The input is left empty,
-- | so picking the same file again raises a change event again.
-- |
-- | The copy and the clearing are one effect because their order matters. An
-- | input's `FileList` is the input's own object, and a webview empties that
-- | object in place when the input is cleared. A list read before the clear
-- | and looked at after it holds nothing.
takeFiles :: HTMLInputElement -> Effect (Array File)
takeFiles = takeFilesImpl

readBytes :: File -> Aff ArrayBuffer
readBytes = toAffE <<< arrayBufferImpl

-- | The browser's MIME type for the file, or `application/octet-stream`.
mimeOf :: File -> String
mimeOf file = case fromMaybe (MediaType "") (File.type_ file) of
  MediaType "" -> "application/octet-stream"
  MediaType t -> t