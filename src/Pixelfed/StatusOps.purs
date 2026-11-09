-- | The two toggles a card offers, what each sends to the server and how the
-- | server's answer is merged back. Shared by every screen that lists statuses.
module Pixelfed.StatusOps
  ( Toggle(..)
  , request
  , merge
  , replaceDisplayed
  , replaceAll
  ) where

import Prelude

import App.Backend (AppError, Backend)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Pixelfed.Client (bookmark, favourite, unbookmark, unfavourite)
import Pixelfed.Entity (Status(..), StatusId, statusId)

data Toggle = ToggleFavourite | ToggleBookmark

derive instance Eq Toggle

-- | The request that flips the toggle on the given (displayed) status.
request :: Toggle -> Status -> Backend -> StatusId -> Aff (Either AppError Status)
request toggle (Status s) = case toggle of
  ToggleFavourite -> if s.favourited then unfavourite else favourite
  ToggleBookmark -> if s.bookmarked then unbookmark else bookmark

-- | `merge toggle old fromServer`. The favourite endpoints honour `_pe=1` and
-- | return the full Pixelfed entity, so the server's copy replaces ours. The
-- | bookmark endpoints always return the Mastodon entity, which lacks
-- | `pf_type`, `content_text` and `liked_by`, so only the flag is taken.
merge :: Toggle -> Status -> Status -> Status
merge toggle (Status old) (Status fromServer) = case toggle of
  ToggleFavourite -> Status fromServer
  ToggleBookmark -> Status old { bookmarked = fromServer.bookmarked }

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
