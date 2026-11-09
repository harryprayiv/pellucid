-- | Hash routes. The set mirrors pixelfed-rn's top-level screens.
module App.Route
  ( Route(..)
  , routeCodec
  , printRoute
  , parseRoute
  , navigate
  , goBack
  ) where

import Prelude hiding ((/))

import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
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
  = Home
  | Public
  | Notifications
  | Profile AccountId
  | StatusDetail StatusId

derive instance Eq Route
derive instance Generic Route _

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Public": "public" / noArgs
  , "Notifications": "notifications" / noArgs
  , "Profile": "profile" / accountId segment
  , "StatusDetail": "status" / statusId segment
  }

accountId :: RouteDuplex' String -> RouteDuplex' AccountId
accountId = as unwrap (Right <<< AccountId)

statusId :: RouteDuplex' String -> RouteDuplex' StatusId
statusId = as unwrap (Right <<< StatusId)

printRoute :: Route -> String
printRoute = RD.print routeCodec

parseRoute :: String -> Maybe Route
parseRoute = hush <<< RD.parse routeCodec

navigate :: Route -> Effect Unit
navigate = setHash <<< printRoute

-- | The webview's own history, which hash navigation feeds.
goBack :: Effect Unit
goBack = window >>= history >>= back