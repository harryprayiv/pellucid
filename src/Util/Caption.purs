-- | Splits a plain-text caption into runs of text, hashtags and mentions, so
-- | a view can draw the last two as buttons. Nothing here reads HTML: the
-- | input is `content_text` (or the stripped fallback), a hashtag is linked
-- | by its own name, and a mention is linked only when the server listed it
-- | in the status's `mentions`, which is where the account id comes from.
module Util.Caption
  ( Segment(..)
  , segments
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex, match, regex, split)
import Data.String.Regex.Flags (global, unicode)
import Pixelfed.Entity (AccountId, Mention)

data Segment
  = Plain String
  -- | The tag's name without `#`.
  | TagLink String
  -- | The account and the text as written, including `@`.
  | MentionLink AccountId String

derive instance Eq Segment

instance Show Segment where
  show = case _ of
    Plain text -> "(Plain " <> show text <> ")"
    TagLink name -> "(TagLink " <> show name <> ")"
    MentionLink id text -> "(MentionLink " <> show id <> " " <> show text <> ")"

-- | A `#` or `@` token: the sign, then letters, digits and underscores, and
-- | for a mention optionally dots, hyphens and an `@domain` part. The sign
-- | must not follow a letter, a digit or one of `_ / & # @ .`, which keeps
-- | e-mail addresses and URL fragments as text.
tokenPattern :: String
tokenPattern = "(?<![\\p{L}\\p{N}_/&#@.])(#[\\p{L}\\p{N}_]+|@[\\p{L}\\p{N}_][\\p{L}\\p{N}_.-]*(?:@[\\p{L}\\p{N}_.-]+)?)"

token :: Maybe Regex
token = case regex tokenPattern (global <> unicode) of
  Right r -> Just r
  Left _ -> Nothing

segments :: Array Mention -> String -> Array Segment
segments mentions caption = case token of
  Nothing -> [ Plain caption ]
  Just r ->
    let
      -- `split` on a pattern with one capture group alternates text and
      -- token; `match` tells which pieces were tokens.
      tokens = case match r caption of
        Just found -> Array.catMaybes (NEA.toArray found)
        Nothing -> []
    in
      Array.foldl join [] (map (classify tokens) (split r caption))
  where
  -- Drops empty runs and joins neighbouring text, so a token that turned
  -- out not to be a link leaves no seam.
  join :: Array Segment -> Segment -> Array Segment
  join acc = case _ of
    Plain "" -> acc
    Plain text -> case Array.unsnoc acc of
      Just { init, last: Plain before } -> Array.snoc init (Plain (before <> text))
      _ -> Array.snoc acc (Plain text)
    link -> Array.snoc acc link

  classify :: Array String -> String -> Segment
  classify tokens piece
    | not (Array.elem piece tokens) = Plain piece
    | otherwise = case String.uncons piece of
        Just { head, tail }
          | head == String.codePointFromChar '#' -> TagLink tail
          | head == String.codePointFromChar '@' -> case findMention tail of
              Just mention -> MentionLink mention.id piece
              Nothing -> Plain piece
        _ -> Plain piece

  -- A mention matches by full handle, or by bare username when the caption
  -- wrote only that. Trailing dots belong to the sentence, not the handle.
  findMention :: String -> Maybe Mention
  findMention written =
    let
      wanted = String.toLower (trimDots written)
    in
      Array.find (\m -> String.toLower m.acct == wanted || String.toLower m.username == wanted) mentions

  trimDots :: String -> String
  trimDots s = case String.stripSuffix (String.Pattern ".") s of
    Just shorter -> trimDots shorter
    Nothing -> s
