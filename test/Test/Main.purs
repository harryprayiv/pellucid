-- | Decoder tests against payloads shaped like the server's transformers,
-- | including the lenient paths: string counters, null `content_text`, the
-- | empty `liked_by` form, a share wrapper, a sparse remote account, a
-- | notification with an unknown verb and an unreadable status, compose
-- | settings with a comma-joined type list, a fresh upload with no url,
-- | lists that carry `false` and `[]` in place of an entry, and direct
-- | messages as both Pixelfed 0.12 and 0.15 send them. The pure machines
-- | are tested in `Test.Pure`.
module Test.Main (main) where

import Prelude

import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (instant)
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console (log)
import Pixelfed.Decode (decodeComposeSettings, decodeContext, decodeConversations, decodeHashtag, decodeHashtags, decodeMedia, decodeMessage, decodeNotifications, decodeRelationships, decodeSearch, decodeStatuses, decodeThread, decodeTrendingTags)
import Pixelfed.Entity (AccountId(..), MediaId(..), MediaType(..), MessageId(..), NotificationId(..), NotificationKind(..), PfType(..), Status(..), StatusId(..), Visibility(..), displayed, statusId)
import Pixelfed.StatusOps (Toggle(..), canShare, merge, removeStatus)
import Test.Check (check, either', maybe')
import Test.Pure as Pure
import Util.Time (parseIso, relativeLabel)

foreign import fixture :: String

foreign import relationshipFixture :: String

foreign import contextFixture :: String

foreign import notificationsFixture :: String

foreign import composeSettingsFixture :: String

foreign import mediaUploadFixture :: String

foreign import searchFixture :: String

foreign import hashtagFixture :: String

foreign import followedTagsFixture :: String

foreign import trendingTagsFixture :: String

foreign import conversationsFixture :: String

foreign import threadFixture :: String

foreign import messageSentFixture :: String

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

  -- The toggle merge rules, on the first fixture status.
  let liked = Status first
  let Status afterBookmark = merge ToggleBookmark liked (Status first { bookmarked = true, contentText = Nothing })
  check "bookmark merge takes only the flag" (afterBookmark.bookmarked && afterBookmark.contentText == first.contentText)
  let Status afterShare = merge ToggleShare liked (Status first { reblogged = true, reblogsCount = 0, contentText = Nothing })
  check "share merge takes the flag and moves the count by one"
    (afterShare.reblogged && afterShare.reblogsCount == first.reblogsCount + 1 && afterShare.contentText == first.contentText)
  let Status afterUnshare = merge ToggleShare (Status afterShare) (Status first { reblogged = false })
  check "unshare moves the count back" (not afterUnshare.reblogged && afterUnshare.reblogsCount == first.reblogsCount)
  let Status sameShare = merge ToggleShare liked (Status first { reblogsCount = 99 })
  check "an unchanged share flag leaves the count" (sameShare.reblogsCount == first.reblogsCount)
  let Status afterLike = merge ToggleFavourite liked (Status first { favourited = false, favouritesCount = 11 })
  check "favourite merge takes the server's copy" (not afterLike.favourited && afterLike.favouritesCount == 11)
  check "a public post can be shared" (canShare liked)
  check "an unlisted post cannot be shared" (not (canShare (displayed second)))
  check "removing the inner post removes its share wrapper"
    (map statusId (removeStatus (StatusId "700000000000000001") statuses) == [ StatusId "812345678901234567" ])

  searchJson <- either' "search fixture is not JSON" (jsonParser searchFixture)
  found <- either' "decodeSearch" (lmap printJsonDecodeError (decodeSearch searchJson))
  check "search: unreadable accounts are dropped" (map _.acct found.accounts == [ "bob", "carol@remote.example" ])
  check "search: two hashtags" (map _.name found.hashtags == [ "sunset", "Sunrise" ])
  check "search: hashtag count" ((_.count =<< Array.index found.hashtags 0) == Just 1204)
  check "search: string count" ((_.count =<< Array.index found.hashtags 1) == Just 0)
  check "search: hashtags are not followed by default" (Array.all (not <<< _.following) found.hashtags)

  tagJson <- either' "hashtag fixture is not JSON" (jsonParser hashtagFixture)
  tag <- either' "decodeHashtag" (lmap printJsonDecodeError (decodeHashtag tagJson))
  check "hashtag: following and count" (tag.following && tag.count == Just 1204 && tag.name == "sunset")

  followedJson <- either' "followed tags fixture is not JSON" (jsonParser followedTagsFixture)
  followed <- either' "decodeHashtags" (lmap printJsonDecodeError (decodeHashtags followedJson))
  check "followed tags: the empty entry is dropped" (map _.name followed == [ "sunset", "film" ])
  check "followed tags: no count without _pe" (Array.all (\t -> t.count == Nothing && t.following) followed)

  trendingJson <- either' "trending tags fixture is not JSON" (jsonParser trendingTagsFixture)
  trending <- either' "decodeTrendingTags" (lmap printJsonDecodeError (decodeTrendingTags trendingJson))
  check "trending: bare names, with and without the hashtag field" (map _.name trending == [ "sunset", "film" ])
  check "trending: totals as number and string" (map _.total trending == [ 88, 41 ])

  convJson <- either' "conversations fixture is not JSON" (jsonParser conversationsFixture)
  convs <- either' "decodeConversations" (lmap printJsonDecodeError (decodeConversations convJson))
  check "conversations: the one without a last message is dropped" (Array.length convs == 2)
  check "conversations: string and numeric ids" (map _.id convs == [ "501", "77" ])
  check "conversations: unread flag" (map _.unread convs == [ true, false ])
  check "conversations: content_text wins" ((map _.lastText (Array.index convs 0)) == Just "See you at eight")
  check "conversations: HTML content is reduced to text" ((map _.lastText (Array.index convs 1)) == Just "Thanks & bye")
  check "conversations: the other participant" ((map _.acct <<< Array.head <<< _.accounts =<< Array.index convs 1) == Just "carol@remote.example")

  threadJson <- either' "thread fixture is not JSON" (jsonParser threadFixture)
  thread <- either' "decodeThread" (lmap printJsonDecodeError (decodeThread threadJson))
  check "thread: the other person" (thread.id == AccountId "7" && thread.username == "bob" && thread.name == "Bob")
  check "thread: a message without an id is dropped" (map _.id thread.messages == [ MessageId "9100", MessageId "9099" ])
  check "thread: authorship" (map _.isAuthor thread.messages == [ false, true ])
  check "thread: a photo carries its media and no text"
    ((map (\m -> { kind: m.kind, text: m.text, media: m.media }) (Array.index thread.messages 1)) == Just { kind: "photo", text: "", media: Just "https://pixelfed.social/storage/m/dm/9099.jpg" })
  check "thread: created_at with an offset parses" (Array.all (isJust <<< parseIso <<< _.createdAt) thread.messages)

  sentJson <- either' "sent message fixture is not JSON" (jsonParser messageSentFixture)
  sent <- either' "decodeMessage" (lmap printJsonDecodeError (decodeMessage sentJson))
  check "sent message: 0.12 omits created_at" (sent.id == MessageId "9101" && sent.createdAt == "" && sent.timeAgo == "1s" && sent.isAuthor)

  Pure.run

  log "all tests passed"
