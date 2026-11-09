-- | Pure parsing of Pellucid's timestamps and a compact "how long ago" label.
-- | Statuses carry `2025-01-01T10:34:12.000Z`, accounts `2025-01-01T10:34:12.612584Z`;
-- | both start with the same 19 characters, which is all this needs.
module Util.Time
  ( parseIso
  , relativeLabel
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime, unInstant)
import Data.Either (hush)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), unformat)
import Data.Int as Int
import Data.List (fromFoldable)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.String (take)

isoFormat :: Formatter
isoFormat = fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder "T"
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  ]

-- | Reads the UTC date and time; fractional seconds and the zone suffix are
-- | ignored, which is correct because the server always sends UTC.
parseIso :: String -> Maybe Instant
parseIso raw = fromDateTime <$> parsed
  where
  parsed :: Maybe DateTime
  parsed = hush (unformat isoFormat (take 19 raw))

-- | `now`, `12s`, `5m`, `3h`, `6d`, `2w`, `4mo`, `1y`.
relativeLabel :: Instant -> Instant -> String
relativeLabel now then_ =
  let
    seconds = Int.floor ((unwrap (unInstant now) - unwrap (unInstant then_)) / 1000.0)
  in
    if seconds < 5 then "now"
    else if seconds < 60 then show seconds <> "s"
    else if seconds < 3600 then show (seconds / 60) <> "m"
    else if seconds < 86400 then show (seconds / 3600) <> "h"
    else if seconds < 7 * 86400 then show (seconds / 86400) <> "d"
    else if seconds < 30 * 86400 then show (seconds / (7 * 86400)) <> "w"
    else if seconds < 365 * 86400 then show (seconds / (30 * 86400)) <> "mo"
    else show (seconds / (365 * 86400)) <> "y"
