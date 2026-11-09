-- | Decoder tests against payloads shaped like the server's transformers,
-- | including the lenient paths: string counters, null `content_text`, the
-- | empty `liked_by` form, a share wrapper, a sparse remote account, a
-- | notification with an unknown verb and an unreadable status, compose
-- | settings with a comma-joined type list, and a fresh upload with no url.
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
import Pixelfed.Decode (decodeComposeSettings, decodeContext, decodeMedia, decodeNotifications, decodeRelationships, decodeStatuses)
import Pixelfed.Entity (AccountId(..), MediaId(..), MediaType(..), NotificationId(..), NotificationKind(..), PfType(..), Status(..), StatusId(..), Visibility(..), displayed, statusId)
import Data.Time.Duration (Milliseconds(..))
import Util.Time (parseIso, relativeLabel)

foreign import fixture :: String

foreign import relationshipFixture :: String

foreign import contextFixture :: String

foreign import notificationsFixture :: String

foreign import composeSettingsFixture :: String

foreign import mediaUploadFixture :: String

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

  relJson <- either' "relationship fixture is not JSON" (jsonParser relationshipFixture)
  rels <- either' "decodeRelationships" (lmap printJsonDecodeError (decodeRelationships relJson))
  rel <- maybe' "relationship" (Array.head rels)
  check "relationship id" (rel.id == AccountId "7")
  check "relationship following" (rel.following && not rel.followedBy && not rel.requested)

  ctxJson <- either' "context fixture is not JSON" (jsonParser contextFixture)
  ctx <- either' "decodeContext" (lmap printJsonDecodeError (decodeContext ctxJson))
  check "no ancestors" (Array.null ctx.ancestors)
  check "one descendant" (Array.length ctx.descendants == 1)
  Status comment <- maybe' "comment" (Array.head ctx.descendants)
  check "comment text" (comment.contentText == Just "Lovely light")
  check "comment parent" (comment.inReplyToId == Just (StatusId "812345678901234567"))
  check "comment type" (comment.pfType == TextPost)

  notifJson <- either' "notifications fixture is not JSON" (jsonParser notificationsFixture)
  notifs <- either' "decodeNotifications" (lmap printJsonDecodeError (decodeNotifications notifJson))
  check "six notifications" (Array.length notifs == 6)
  like <- maybe' "like notification" (Array.index notifs 0)
  check "like id" (like.id == NotificationId "9001")
  check "like kind" (like.kind == NFavourite)
  check "like actor" ((map _.acct like.account) == Just "dave@remote.example")
  check "like status is the liked post" ((map statusId like.status) == Just (StatusId "812345678901234567"))
  commentN <- maybe' "comment notification" (Array.index notifs 1)
  check "comment kind" (commentN.kind == NComment)
  check "comment status is the comment" ((map statusId commentN.status) == Just (StatusId "900000000000000001"))
  followN <- maybe' "follow notification" (Array.index notifs 2)
  check "follow kind" (followN.kind == NFollow)
  check "follow has no status" (not (isJust followN.status))
  shareN <- maybe' "share notification" (Array.index notifs 3)
  check "share kind" (shareN.kind == NShare)
  taggedN <- maybe' "tagged notification" (Array.index notifs 4)
  check "tagged kind" (taggedN.kind == NTagged)
  check "tagged status id" ((map _.statusId taggedN.tagged) == Just (StatusId "700000000000000001"))
  other <- maybe' "unknown notification" (Array.index notifs 5)
  check "unknown verb kept" (other.kind == NOther "poll:ended")
  check "null actor is Nothing" (not (isJust other.account))
  check "unreadable status is Nothing" (not (isJust other.status))

  settingsJson <- either' "compose settings fixture is not JSON" (jsonParser composeSettingsFixture)
  settings <- either' "decodeComposeSettings" (lmap printJsonDecodeError (decodeComposeSettings settingsJson))
  check "comma-joined media types split" (settings.allowedMediaTypes == [ "image/jpeg", "image/png", "image/gif", "video/mp4" ])
  check "caption limit" (settings.maxCaptionLength == 2000)
  check "file size in KB" (settings.maxFileSizeKb == 15000)
  check "attachment limit" (settings.maxMediaAttachments == 10)
  check "alt text limit" (settings.maxAltTextLength == 1000)
  check "default scope" (settings.defaultVisibility == Just Unlisted)

  mediaJson <- either' "media upload fixture is not JSON" (jsonParser mediaUploadFixture)
  uploaded <- either' "decodeMedia" (lmap printJsonDecodeError (decodeMedia mediaJson))
  check "upload id" (uploaded.id == MediaId "555")
  check "null url falls back to preview_url" (uploaded.url == "https://pixelfed.social/storage/m/_v2/7/abc/new.jpg?v=1762680000")
  check "null meta gives no dims" (uploaded.dimensions == Nothing)
  check "null description" (uploaded.description == Nothing)

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
