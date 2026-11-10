-- | What `Component.Root` and the screens it mounts say to each other. Every
-- | screen is a component with this query type and this output type, so Root
-- | can address whichever one is on top without knowing which it is.
-- |
-- | Screens stay mounted underneath the one in front (see `App.Stack`), so a
-- | change made on one screen has to reach the others that show the same
-- | post or account. A screen raises an `Output`; Root turns it into the
-- | matching `Query` for every mounted screen.
module App.Screen
  ( Query(..)
  , Output(..)
  ) where

import Pixelfed.Entity (AccountId, Status, StatusId)

data Query a
  -- | The window has scrolled close to the end of the page. Sent only to the
  -- | screen on top; a list answers by fetching its next chunk.
  = NearBottom a
  -- | A post was deleted. A screen that lists it drops it.
  | StatusRemoved StatusId a
  -- | A post changed: liked, bookmarked, shared, edited, or its comment
  -- | count moved. A screen that shows it replaces its copy.
  | StatusUpdated Status a
  -- | An account's own data changed. A screen that shows it reloads.
  | AccountUpdated AccountId a

data Output
  -- | The person asked to log out.
  = LogoutRequested
  | StatusDeleted StatusId
  -- | Carries the displayed post, never a share wrapper.
  | StatusChanged Status
  | AccountChanged AccountId
