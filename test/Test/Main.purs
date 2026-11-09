-- | Decoder tests against a payload shaped like `StatusTransformer`,
-- | including the lenient paths: string counters, null `content_text`, the
-- | empty `liked_by` form, a share wrapper and a sparse remote account.
module Test.Main (main) where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (instant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Pellucid.Decode (decodeStatuses)
import Pellucid.Entity (MediaType(..), PfType(..), Status(..), StatusId(..), Visibility(..), displayed, statusId)
import Data.Time.Duration (Milliseconds(..))
import Util.Time (parseIso, relativeLabel)

foreign import fixture :: String

main :: Effect Unit
main = do
  json <- either' "fixture is not JSON" (jsonParser fixture)
  statuses <- either' "decodeStatuses" (lmap printJsonDecodeError (decodeStatuses json))
  check "two statuses" (Array.length statuses == 2)

  Status first <- maybe' "first status" (Array.index statuses 0)
  check "id" (first.id == StatusId "812345678901234567")
  check "album type" (first.pfType == PhotoAlbum)
  check "favourited" first.favourited
  check "favourites_count" (first.favouritesCount == 12)
  check "content_text wins" (first.contentText == Just "Golden hour #sunset & coffee")
  check "two attachments" (Array.length first.mediaAttachments == 2)
  check "image dims" ((_.dimensions =<< Array.index first.mediaAttachments 0) == Just { width: 4032, height: 3024 })
  check "video type" ((map _.mediaType (Array.index first.mediaAttachments 1)) == Just VideoMedia)
  check "video has no dims" ((_.dimensions =<< Array.index first.mediaAttachments 1) == Nothing)
  check "liked_by" ((map _.totalCount first.likedBy) == Just 11)
  check "place" ((map _.name first.place) == Just "Boston")
  check "pronouns" (first.account.pronouns == [ "she/her" ])
  check "created_at parses" (isJust (parseIso first.createdAt))

  second <- maybe' "second status" (Array.index statuses 1)
  let Status wrapper = second
  check "share type" (wrapper.pfType == Share)
  check "string counter" (wrapper.reblogsCount == 0)
  check "null counter" (wrapper.favouritesCount == 0)
  check "empty liked_by" (wrapper.likedBy == Nothing)
  check "null place" (wrapper.place == Nothing)
  check "remote acct" (wrapper.account.acct == "carol@remote.example")
  check "displayed is inner" (statusId (displayed second) == StatusId "700000000000000001")
  let Status inner = displayed second
  check "inner sensitive" inner.sensitive
  check "inner unlisted" (inner.visibility == Unlisted)
  check "inner bookmarked" inner.bookmarked
  check "sparse account" (inner.account.followersCount == 0 && inner.account.displayName == "Dave")
  check "mention" ((map _.username (Array.index inner.mentions 0)) == Just "alice")

  t0 <- maybe' "instant" (instant (Milliseconds 1_000_000_000.0))
  t1 <- maybe' "instant" (instant (Milliseconds (1_000_000_000.0 + 3.0 * 3600.0 * 1000.0)))
  check "relative hours" (relativeLabel t1 t0 == "3h")
  check "relative now" (relativeLabel t0 t0 == "now")

  log "all decoder tests passed"

check :: String -> Boolean -> Effect Unit
check label ok = if ok then log ("ok   " <> label) else throw ("FAIL " <> label)

either' :: forall a. String -> Either String a -> Effect a
either' label = case _ of
  Left err -> throw (label <> ": " <> err)
  Right a -> pure a

maybe' :: forall a. String -> Maybe a -> Effect a
maybe' label = case _ of
  Nothing -> throw (label <> ": missing")
  Just a -> pure a
