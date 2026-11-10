-- | The navigation stack. Hash routing only says which route is current; this
-- | decides whether a route change is a step back, a switch of tab or a step
-- | forward, so that the screen underneath stays mounted with its state and
-- | its scroll position. Pure.
module App.Stack
  ( Entry
  , Stack
  , Step
  , single
  , front
  , base
  , navigate
  , maxDepth
  ) where

import Prelude

import App.Route (Route, isTabRoot)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe)

-- | One mounted screen. `key` identifies the component instance and is never
-- | reused; `scroll` is where the window was when the screen was left.
type Entry =
  { route :: Route
  , key :: Int
  , scroll :: Number
  }

type Stack = NonEmptyArray Entry

-- | The outcome of a route change: the new stack, the next unused key and
-- | the window position to restore.
type Step =
  { stack :: Stack
  , nextKey :: Int
  , scrollTo :: Number
  }

-- | Screens kept mounted at most. Beyond it the oldest one above the tab's
-- | own screen is dropped.
maxDepth :: Int
maxDepth = 12

single :: Int -> Route -> Stack
single key route = NEA.singleton { route, key, scroll: 0.0 }

front :: Stack -> Entry
front = NEA.last

base :: Stack -> Entry
base = NEA.head

-- | `navigate nextKey currentScroll route stack`.
-- |
-- | * The route already on top: nothing changes.
-- | * The route directly underneath: a step back. The top screen is
-- |   unmounted and the one underneath gets its scroll position back.
-- | * A tab's own route: the stack is replaced by that one screen.
-- | * Anything else: a step forward. The current position is recorded on
-- |   the screen being covered and the new one starts at the top.
navigate :: Int -> Number -> Route -> Stack -> Step
navigate nextKey currentScroll route stack
  | (front stack).route == route = { stack, nextKey, scrollTo: currentScroll }
  | otherwise =
      case beneath of
        Just below | below.route == route ->
          { stack: fromMaybe stack (NEA.fromArray (NEA.init stack)), nextKey, scrollTo: below.scroll }
        _ | isTabRoot route -> { stack: single nextKey route, nextKey: nextKey + 1, scrollTo: 0.0 }
        _ ->
          let
            covered = NEA.snoc' (NEA.init stack) ((front stack) { scroll = currentScroll })
            pushed = NEA.snoc covered { route, key: nextKey, scroll: 0.0 }
          in
            { stack: trim pushed, nextKey: nextKey + 1, scrollTo: 0.0 }
      where
      beneath :: Maybe Entry
      beneath = NEA.index stack (NEA.length stack - 2)

      trim :: Stack -> Stack
      trim s
        | NEA.length s > maxDepth = fromMaybe s (NEA.fromArray =<< Array.deleteAt 1 (NEA.toArray s))
        | otherwise = s
