-- | Small rendering helpers shared by every screen.
module View.Common
  ( cls
  , lazy
  , handleName
  , agoLabel
  , avatar
  , iconButton
  , screenHeader
  ) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..))
import Data.String (null, trim)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Util.Time (parseIso, relativeLabel)

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

lazy :: forall r i. HP.IProp r i
lazy = HP.attr (HH.AttrName "loading") "lazy"

-- | The display name when there is one, the handle otherwise.
handleName :: String -> String -> String
handleName displayName acct = if null (trim displayName) then acct else displayName

-- | Compact relative age, empty when either side cannot be read.
agoLabel :: Maybe Instant -> String -> String
agoLabel now createdAt = case now, parseIso createdAt of
  Just n, Just t -> relativeLabel n t
  _, _ -> ""

avatar :: forall w i. String -> String -> HH.HTML w i
avatar className url = HH.img [ cls className, HP.src url, HP.alt "", lazy ]

-- | A borderless button: class, tooltip, visible label, action.
iconButton :: forall w i. String -> String -> String -> i -> HH.HTML w i
iconButton className title label action =
  HH.button
    [ cls className, HP.type_ HP.ButtonButton, HP.title title, HE.onClick \_ -> action ]
    [ HH.text label ]

-- | Sticky top bar: a back action on the left, a title, anything on the right.
screenHeader :: forall w i. i -> String -> Array (HH.HTML w i) -> HH.HTML w i
screenHeader back title right =
  HH.header [ cls "screen-header" ]
    [ iconButton "ghost back" "Back" "\x2190" back
    , HH.h1_ [ HH.text title ]
    , HH.div [ cls "screen-header-right" ] right
    ]