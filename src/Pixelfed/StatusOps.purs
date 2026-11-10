-- | The three toggles a card offers, what each sends to the server and how the
-- | server's answer is merged back. Shared by every screen that lists statuses.
module Pixelfed.StatusOps
  ( Toggle(..)
  , request
  , merge
  , replaceDisplayed
  , replaceAll
  , removeStatus
  , canShare
  ) where

import Prelude

import App.Backend (AppError, Backend)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Pixelfed.Client (bookmark, favourite, reblog, unbookmark, unfavourite, unreblog)
import Pixelfed.Entity (Status(..), StatusId, Visibility(..), displayed, statusId)

data Toggle = ToggleFavourite | ToggleBookmark | ToggleShare

derive instance Eq Toggle

-- | The request that flips the toggle on the given (displayed) status.
request :: Toggle -> Status -> Backend -> StatusId -> Aff (Either AppError Status)
request toggle (Status s) = case toggle of
  ToggleFavourite -> if s.favourited then unfavourite else favourite
  ToggleBookmark -> if s.bookmarked then unbookmark else bookmark
  ToggleShare -> if s.reblogged then unreblog else reblog

-- | `merge toggle old fromServer`. The favourite endpoints honour `_pe=1` and
-- | return the full Pixelfed entity, so the server's copy replaces ours. The
-- | bookmark and share endpoints always return the Mastodon entity, which
-- | lacks `pf_type`, `content_text` and `liked_by`, so only the flag is
-- | taken. The share count in that answer is read before the server's queue
-- | has counted the share, so it moves by one locally when the flag flips.
merge :: Toggle -> Status -> Status -> Status
merge toggle (Status old) (Status fromServer) = case toggle of
  ToggleFavourite -> Status fromServer
  ToggleBookmark -> Status old { bookmarked = fromServer.bookmarked }
  ToggleShare -> Status old
    { reblogged = fromServer.reblogged
    , reblogsCount =
        if fromServer.reblogged == old.reblogged then old.reblogsCount
        else if fromServer.reblogged then old.reblogsCount + 1
        else max 0 (old.reblogsCount - 1)
    }

-- | Pixelfed shares public posts only (`Status::whereScope('public')`).
canShare :: Status -> Boolean
canShare (Status s) = s.visibility == Public

-- | Replaces the status with `id` whether it sits at the top level or inside
-- | a share wrapper.
replaceDisplayed :: StatusId -> Status -> Status -> Status
replaceDisplayed id updated st@(Status top)
  | top.id == id = updated
  | otherwise = case top.reblog of
      Just inner | statusId inner == id -> Status top { reblog = Just updated }
      _ -> st

replaceAll :: StatusId -> Status -> Array Status -> Array Status
replaceAll id updated = map (replaceDisplayed id updated)

-- | Drops a deleted post, and every share wrapper that showed it.
removeStatus :: StatusId -> Array Status -> Array Status
removeStatus id = Array.filter \st -> statusId st /= id && statusId (displayed st) /= id
