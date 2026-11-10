-- | The state of a list that is fetched a chunk at a time, and its
-- | transitions. Pure: a screen runs the `Pager` and feeds the result back.
-- | Every paginated screen uses this one machine, so the rules (no chunk is
-- | requested twice, no item appears twice, a chunk with nothing new ends
-- | the list) hold everywhere.
module App.Paged
  ( Paged
  , Phase(..)
  , loading
  , first
  , wantsMore
  , startMore
  , more
  , isLoading
  , modifyItems
  ) where

import Prelude

import App.Backend (AppError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Pixelfed.Client (Chunk, Pager)

type Paged a =
  { items :: Array a
  , phase :: Phase a
  }

data Phase a
  -- | The first chunk has been asked for.
  = FirstLoading
  | FirstFailed AppError
  -- | Loaded, and the server offers more.
  | Idle (Pager a)
  -- | The chunk behind this pager has been asked for.
  | LoadingMore (Pager a)
  -- | That chunk failed; the pager is kept so it can be tried again.
  | MoreFailed AppError (Pager a)
  -- | Loaded, and there is nothing further.
  | Exhausted

loading :: forall a. Paged a
loading = { items: [], phase: FirstLoading }

-- | The answer to the first request replaces whatever was held.
first :: forall a. Either AppError (Chunk a) -> Paged a
first = case _ of
  Left err -> { items: [], phase: FirstFailed err }
  Right chunk ->
    { items: chunk.items
    , phase: case chunk.next of
        Just pager | not (Array.null chunk.items) -> Idle pager
        _ -> Exhausted
    }

-- | The pager to run next, when the list is in a state that allows it.
wantsMore :: forall a. Paged a -> Maybe (Pager a)
wantsMore list = case list.phase of
  Idle pager -> Just pager
  MoreFailed _ pager -> Just pager
  _ -> Nothing

-- | Marks the list as fetching. A no-op unless `wantsMore` gave a pager.
startMore :: forall a. Paged a -> Paged a
startMore list = case wantsMore list of
  Just pager -> list { phase = LoadingMore pager }
  Nothing -> list

-- | Appends the items of a further chunk that are not held yet, by key. The
-- | answer is dropped unless the list is still waiting for it, which is what
-- | keeps a late answer from landing in a list that has been reloaded since.
more :: forall a k. Ord k => (a -> k) -> Either AppError (Chunk a) -> Paged a -> Paged a
more key result list = case list.phase of
  LoadingMore pager -> case result of
    Left err -> list { phase = MoreFailed err pager }
    Right chunk ->
      let
        known = Set.fromFoldable (map key list.items)
        fresh = Array.filter (\item -> not (Set.member (key item) known)) (Array.nubByEq (\a b -> key a == key b) chunk.items)
      in
        { items: list.items <> fresh
        , phase: case chunk.next of
            Just next | not (Array.null fresh) -> Idle next
            _ -> Exhausted
        }
  _ -> list

isLoading :: forall a. Paged a -> Boolean
isLoading list = case list.phase of
  FirstLoading -> true
  LoadingMore _ -> true
  _ -> false

modifyItems :: forall a. (Array a -> Array a) -> Paged a -> Paged a
modifyItems f list = list { items = f list.items }
