-- | Text helpers. `stripTags` exists only as a display fallback for HTML the
-- | server sends where no plain-text field is available; nothing in the app
-- | renders HTML as HTML.
module Util.Text
  ( stripTags
  ) where

import Prelude

import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)

stripTags :: String -> String
stripTags =
  replace (unsafeRegex "<br\\s*/?>" global) "\n"
    >>> replace (unsafeRegex "</p>\\s*<p>" global) "\n\n"
    >>> replace (unsafeRegex "<[^>]*>" global) ""
    >>> replace (unsafeRegex "&amp;" global) "&"
    >>> replace (unsafeRegex "&lt;" global) "<"
    >>> replace (unsafeRegex "&gt;" global) ">"
    >>> replace (unsafeRegex "&quot;" global) "\""
    >>> replace (unsafeRegex "&#39;" global) "'"
