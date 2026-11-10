-- | One post as a square in a three-wide grid: the profile's posts and the
-- | trending posts on Explore.
module View.Tile
  ( postTile
  , postGrid
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (take)
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Entity (MediaType(..), Status(..), StatusId, captionOf, displayed, firstMedia)
import View.Common (cls, lazy)

postGrid :: forall w i. (StatusId -> i) -> Array Status -> HH.HTML w i
postGrid open posts = HH.div [ cls "grid" ] (map (postTile open) posts)

-- | The first attachment's preview, a play mark for video, a stack mark for
-- | an album, the word Sensitive in place of sensitive media, and a caption
-- | excerpt for a post without media.
postTile :: forall w i. (StatusId -> i) -> Status -> HH.HTML w i
postTile open status =
  let
    Status shown = displayed status
  in
    HH.button
      [ cls "tile", HP.type_ HP.ButtonButton, HE.onClick \_ -> open shown.id ]
      case firstMedia shown of
        Just m | not shown.sensitive ->
          [ HH.img
              [ cls "tile-image"
              , HP.src (fromMaybe (fromMaybe m.url m.optimizedUrl) m.previewUrl)
              , HP.alt (fromMaybe "" m.description)
              , lazy
              ]
          , if m.mediaType == VideoMedia || m.mediaType == Gifv then HH.span [ cls "tile-badge" ] [ HH.text "\x25B6" ]
            else if Array.length shown.mediaAttachments > 1 then HH.span [ cls "tile-badge" ] [ HH.text "\x29C9" ]
            else HH.text ""
          ]
        Just _ -> [ HH.span [ cls "tile-text" ] [ HH.text "Sensitive" ] ]
        Nothing -> [ HH.span [ cls "tile-text" ] [ HH.text (excerpt (captionOf shown)) ] ]

excerpt :: String -> String
excerpt s = if String.length s > 80 then take 77 s <> "\x2026" else s
