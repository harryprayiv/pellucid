-- | Hash routes. The set mirrors pixelfed-rn's screens. Every route starts
-- | with its own first segment, so no two routes share a prefix.
module App.Route
  ( Route(..)
  , routeCodec
  , printRoute
  , parseRoute
  , navigate
  , goBack
  , isTabRoot
  ) where

import Prelude hiding ((/))

import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Pixelfed.Entity (AccountId(..), StatusId(..))
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex as RD
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash (setHash)
import Web.HTML (window)
import Web.HTML.History (back)
import Web.HTML.Window (history)

data Route
  -- The five tabs. Public sits under the Home tab.
  = Home
  | Public
  | Explore
  | Compose
  | Notifications
  | Me
  -- Screens opened from another screen.
  | Profile AccountId
  | StatusDetail StatusId
  | Tag String
  | Followers AccountId
  | Following AccountId
  | LikedBy StatusId
  | SharedBy StatusId
  | Bookmarks
  | Likes
  | Settings
  | EditProfile
  | FollowRequests
  | Muted
  | Blocked
  | FollowedTags
  | Conversations
  | Thread AccountId
  | ReportPost StatusId
  | ReportAccount AccountId
  | EditPost StatusId

derive instance Eq Route
derive instance Generic Route _

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Public": "public" / noArgs
  , "Explore": "explore" / noArgs
  , "Compose": "compose" / noArgs
  , "Notifications": "notifications" / noArgs
  , "Me": "me" / noArgs
  , "Profile": "profile" / accountId segment
  , "StatusDetail": "status" / statusId segment
  , "Tag": "tag" / segment
  , "Followers": "followers" / accountId segment
  , "Following": "following" / accountId segment
  , "LikedBy": "liked-by" / statusId segment
  , "SharedBy": "shared-by" / statusId segment
  , "Bookmarks": "bookmarks" / noArgs
  , "Likes": "likes" / noArgs
  , "Settings": "settings" / noArgs
  , "EditProfile": "edit-profile" / noArgs
  , "FollowRequests": "follow-requests" / noArgs
  , "Muted": "muted" / noArgs
  , "Blocked": "blocked" / noArgs
  , "FollowedTags": "followed-tags" / noArgs
  , "Conversations": "messages" / noArgs
  , "Thread": "thread" / accountId segment
  , "ReportPost": "report-post" / statusId segment
  , "ReportAccount": "report-account" / accountId segment
  , "EditPost": "edit-post" / statusId segment
  }

accountId :: RouteDuplex' String -> RouteDuplex' AccountId
accountId = as unwrap (Right <<< AccountId)

statusId :: RouteDuplex' String -> RouteDuplex' StatusId
statusId = as unwrap (Right <<< StatusId)

printRoute :: Route -> String
printRoute = RD.print routeCodec

-- | The empty hash is the home route: that is what the address bar holds
-- | when the app starts, and what it returns to when the person goes back
-- | to the first screen. The codec alone reads only `/` as home.
parseRoute :: String -> Maybe Route
parseRoute = case _ of
  "" -> Just Home
  hash -> hush (RD.parse routeCodec hash)

navigate :: Route -> Effect Unit
navigate = setHash <<< printRoute

-- | The webview's own history, which hash navigation feeds.
goBack :: Effect Unit
goBack = window >>= history >>= back

-- | A tab's own screen. Navigating to one replaces the stack of screens.
isTabRoot :: Route -> Boolean
isTabRoot = case _ of
  Home -> true
  Public -> true
  Explore -> true
  Compose -> true
  Notifications -> true
  Me -> true
  _ -> false
