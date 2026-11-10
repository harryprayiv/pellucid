-- | Tests for the pure machines every screen relies on: the paged list, the
-- | navigation stack, caption links and the merge rules for toggles.
module Test.Pure (run) where

import Prelude

import App.Backend (AppError(..))
import App.Paged (Phase(..))
import App.Paged as Paged
import App.Route (Route(..), parseRoute, printRoute)
import App.Stack as Stack
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect (Effect)
import Pixelfed.Client (Chunk, Pager(..))
import Pixelfed.Entity (AccountId(..), Mention, StatusId(..))
import Test.Check (check)
import Util.Caption (Segment(..), segments)

-- | A pager that is never run: the transitions only pass it along.
inert :: Pager Int
inert = Pager \_ -> pure (Right { items: [], next: Nothing })

chunk :: Array Int -> Boolean -> Either AppError (Chunk Int)
chunk items hasNext = Right { items, next: if hasNext then Just inert else Nothing }

isIdle :: forall a. Phase a -> Boolean
isIdle = case _ of
  Idle _ -> true
  _ -> false

isExhausted :: forall a. Phase a -> Boolean
isExhausted = case _ of
  Exhausted -> true
  _ -> false

isMoreFailed :: forall a. Phase a -> Boolean
isMoreFailed = case _ of
  MoreFailed _ _ -> true
  _ -> false

run :: Effect Unit
run = do
  paged
  stack
  routes
  captions

paged :: Effect Unit
paged = do
  let start = Paged.first (chunk [ 1, 2, 3 ] true)
  check "paged: first chunk with more is idle" (isIdle start.phase && start.items == [ 1, 2, 3 ])
  check "paged: idle wants more" (isJust (Paged.wantsMore start))
  check "paged: first chunk without next is exhausted" (isExhausted (Paged.first (chunk [ 1 ] false)).phase)
  check "paged: an empty first chunk is exhausted even with a next" (isExhausted (Paged.first (chunk [] true)).phase)
  check "paged: a failed first chunk wants nothing"
    (isNothing (Paged.wantsMore (Paged.first (Left (Network "down") :: Either AppError (Chunk Int)))))

  let fetching = Paged.startMore start
  check "paged: fetching does not want more" (isNothing (Paged.wantsMore fetching))
  check "paged: starting twice changes nothing" (isNothing (Paged.wantsMore (Paged.startMore fetching)))

  let grown = Paged.more identity (chunk [ 3, 4, 4, 5 ] true) fetching
  check "paged: only items not held are appended, once each" (grown.items == [ 1, 2, 3, 4, 5 ])
  check "paged: more with a next is idle again" (isIdle grown.phase)

  let repeated = Paged.more identity (chunk [ 1, 2 ] true) fetching
  check "paged: a chunk with nothing new ends the list" (isExhausted repeated.phase && repeated.items == [ 1, 2, 3 ])

  let failed = Paged.more identity (Left (Network "down")) fetching
  check "paged: a failed chunk keeps the items" (failed.items == [ 1, 2, 3 ] && isMoreFailed failed.phase)
  check "paged: a failed chunk can be retried" (isJust (Paged.wantsMore failed))

  -- The list was reloaded while a chunk was out: the late answer is dropped.
  let late = Paged.more identity (chunk [ 9 ] true) (Paged.first (chunk [ 7, 8 ] true))
  check "paged: an answer nobody is waiting for is dropped" (late.items == [ 7, 8 ])
  let reloading = Paged.more identity (chunk [ 9 ] true) (Paged.loading :: Paged.Paged Int)
  check "paged: an answer during a reload is dropped" (Array.null reloading.items)

stack :: Effect Unit
stack = do
  let home = Stack.single 0 Home
  check "stack: starts with one screen" (NEA.length home == 1)

  let same = Stack.navigate 1 120.0 Home home
  check "stack: the route on top changes nothing" (NEA.length same.stack == 1 && same.nextKey == 1 && same.scrollTo == 120.0)

  let toPost = Stack.navigate 1 640.0 (StatusDetail (StatusId "5")) home
  check "stack: a step forward mounts a second screen" (NEA.length toPost.stack == 2 && toPost.nextKey == 2)
  check "stack: the new screen starts at the top" (toPost.scrollTo == 0.0)
  check "stack: the covered screen remembers where it was" ((Stack.base toPost.stack).scroll == 640.0)
  check "stack: the covered screen keeps its key" ((Stack.base toPost.stack).key == 0)

  let toProfile = Stack.navigate toPost.nextKey 50.0 (Profile (AccountId "7")) toPost.stack
  let backToPost = Stack.navigate toProfile.nextKey 300.0 (StatusDetail (StatusId "5")) toProfile.stack
  check "stack: the route underneath is a step back" (NEA.length backToPost.stack == 2)
  check "stack: a step back restores the position" (backToPost.scrollTo == 50.0)
  check "stack: a step back mounts nothing" (backToPost.nextKey == toProfile.nextKey)

  let backHome = Stack.navigate backToPost.nextKey 10.0 Home backToPost.stack
  check "stack: back to the tab restores its position" (NEA.length backHome.stack == 1 && backHome.scrollTo == 640.0)

  let toExplore = Stack.navigate toProfile.nextKey 300.0 Explore toProfile.stack
  check "stack: a tab replaces the stack" (NEA.length toExplore.stack == 1 && (Stack.front toExplore.stack).route == Explore)
  check "stack: a tab gets a fresh screen" ((Stack.front toExplore.stack).key == toProfile.nextKey && toExplore.scrollTo == 0.0)

  -- Twenty steps forward: the depth is capped and the tab's screen survives.
  let
    deep = Array.foldl
      (\acc n -> Stack.navigate acc.nextKey 0.0 (Profile (AccountId (show n))) acc.stack)
      { stack: home, nextKey: 1, scrollTo: 0.0 }
      (Array.range 1 20)
  check "stack: depth is capped" (NEA.length deep.stack == Stack.maxDepth)
  check "stack: the tab's screen is never dropped" ((Stack.base deep.stack).route == Home)
  check "stack: the newest screen is on top" ((Stack.front deep.stack).route == Profile (AccountId "20"))
  check "stack: keys are never reused" (Array.length (Array.nub (map _.key (NEA.toArray deep.stack))) == Stack.maxDepth)

routes :: Effect Unit
routes = do
  let
    all =
      [ Home
      , Public
      , Explore
      , Compose
      , Notifications
      , Me
      , Profile (AccountId "7")
      , StatusDetail (StatusId "812345678901234567")
      , Tag "sunset"
      , Followers (AccountId "7")
      , Following (AccountId "7")
      , LikedBy (StatusId "5")
      , SharedBy (StatusId "5")
      , Bookmarks
      , Likes
      , Settings
      , EditProfile
      , FollowRequests
      , Muted
      , Blocked
      , FollowedTags
      , Conversations
      , Thread (AccountId "7")
      , ReportPost (StatusId "5")
      , ReportAccount (AccountId "7")
      , EditPost (StatusId "5")
      ]
  for_ all \route ->
    check ("route round trip: " <> printRoute route) (parseRoute (printRoute route) == Just route)
  check "route: every printed form is distinct" (Array.length (Array.nub (map printRoute all)) == Array.length all)
  check "route: a hashtag with non-ASCII letters survives" (parseRoute (printRoute (Tag "caf\x00E9")) == Just (Tag "caf\x00E9"))
  check "route: an unknown path is no route" (isNothing (parseRoute "/nowhere/at/all"))
  check "route: the empty hash is home" (parseRoute "" == Just Home)

captions :: Effect Unit
captions = do
  let
    alice :: Mention
    alice = { id: AccountId "3", username: "alice", acct: "alice", url: "https://pixelfed.social/alice" }

    carol :: Mention
    carol = { id: AccountId "9", username: "carol", acct: "carol@remote.example", url: "https://remote.example/users/carol" }
  check "caption: plain text is one run" (segments [] "Golden hour" == [ Plain "Golden hour" ])
  check "caption: a hashtag is a link"
    (segments [] "Golden hour #sunset & coffee" == [ Plain "Golden hour ", TagLink "sunset", Plain " & coffee" ])
  check "caption: a hashtag at the start and at the end"
    (segments [] "#film is #alive" == [ TagLink "film", Plain " is ", TagLink "alive" ])
  check "caption: a listed mention is a link"
    (segments [ alice ] "with @alice today" == [ Plain "with ", MentionLink (AccountId "3") "@alice", Plain " today" ])
  check "caption: an unlisted mention stays text, in one run" (segments [] "with @mallory now" == [ Plain "with @mallory now" ])
  check "caption: a full handle matches the listed acct"
    (segments [ carol ] "cc @carol@remote.example" == [ Plain "cc ", MentionLink (AccountId "9") "@carol@remote.example" ])
  check "caption: a bare username matches a remote mention"
    (segments [ carol ] "cc @carol" == [ Plain "cc ", MentionLink (AccountId "9") "@carol" ])
  check "caption: mentions compare without case"
    (segments [ alice ] "@Alice" == [ MentionLink (AccountId "3") "@Alice" ])
  check "caption: an e-mail address is not a mention" (segments [ alice ] "mail bob@alice.example" == [ Plain "mail bob@alice.example" ])
  check "caption: a URL fragment is not a hashtag"
    (segments [] "see https://example.org/page#top" == [ Plain "see https://example.org/page#top" ])
  check "caption: non-ASCII hashtags" (segments [] "#caf\x00E9 #\x65E5\x672C" == [ TagLink "caf\x00E9", Plain " ", TagLink "\x65E5\x672C" ])
  check "caption: an empty caption has no runs" (Array.null (segments [] ""))
