-- | Window scrolling: where the page is, whether it is close to its end, and
-- | putting it back where it was. No foreign code; these are the standard
-- | `Window` and `Element` properties.
module Util.Scroll
  ( position
  , restore
  , nearBottom
  , scrolls
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Web.DOM.Element (scrollHeight)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document, innerHeight, scroll, scrollY, toEventTarget)

-- | How far from the end of the page counts as close, in CSS pixels.
margin :: Number
margin = 800.0

position :: Effect Number
position = window >>= scrollY

restore :: Number -> Effect Unit
restore y = window >>= scroll 0 (Int.round y)

-- | True when less than `margin` of the page lies below the viewport.
nearBottom :: Effect Boolean
nearBottom = do
  w <- window
  y <- scrollY w
  viewport <- innerHeight w
  page <- document w >>= body
  case page of
    Nothing -> pure false
    Just element -> do
      height <- scrollHeight (toElement element)
      pure (y + Int.toNumber viewport + margin >= height)

-- | One `a` for every scroll event on the window.
scrolls :: forall a. a -> Effect (HS.Emitter a)
scrolls action = do
  w <- window
  pure (eventListener (EventType "scroll") (toEventTarget w) (const (Just action)))
