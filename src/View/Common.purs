-- | Small rendering helpers shared by every screen.
module View.Common
  ( cls
  , lazy
  , handleName
  , agoLabel
  , avatar
  , iconButton
  , screenHeader
  , noticeBar
  , errorBox
  , statusLine
  , pagedFooter
  , accountRow
  ) where

import Prelude

import App.Backend (AppError, printAppError)
import App.Paged (Paged, Phase(..))
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..))
import Data.String (null, trim)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Entity (Account)
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
avatar className url
  | null (trim url) = HH.span [ cls (className <> " avatar-blank") ] []
  | otherwise = HH.img [ cls className, HP.src url, HP.alt "", lazy ]

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

-- | A dismissible line for something that went wrong without taking the
-- | screen with it.
noticeBar :: forall w i. i -> Maybe String -> HH.HTML w i
noticeBar dismiss = case _ of
  Nothing -> HH.text ""
  Just text ->
    HH.div [ cls "notice" ]
      [ HH.span_ [ HH.text text ]
      , iconButton "ghost" "Dismiss" "\x2715" dismiss
      ]

-- | A failure that leaves the screen with nothing to show, and a way to retry.
errorBox :: forall w i. i -> AppError -> HH.HTML w i
errorBox retry err =
  HH.div [ cls "error-box" ]
    [ HH.p_ [ HH.text (printAppError err) ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> retry ] [ HH.text "Try again" ]
    ]

statusLine :: forall w i. String -> HH.HTML w i
statusLine text = HH.p [ cls "status-line" ] [ HH.text text ]

-- | What sits under a paged list: nothing while the first chunk loads (the
-- | screen says that itself), a button while more can be fetched, progress,
-- | a retry, or the end. Scrolling near the end presses the button for the
-- | person; the button stays as the fallback.
pagedFooter :: forall w i a. i -> String -> Paged a -> HH.HTML w i
pagedFooter loadMore endText list = HH.div [ cls "feed-footer" ] case list.phase of
  FirstLoading -> []
  FirstFailed _ -> []
  Idle _ -> [ HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> loadMore ] [ HH.text "Load more" ] ]
  LoadingMore _ -> [ statusLine "Loading more\x2026" ]
  MoreFailed err _ ->
    [ HH.p_ [ HH.text (printAppError err) ]
    , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> loadMore ] [ HH.text "Try again" ]
    ]
  Exhausted -> if Array.null list.items || null endText then [] else [ statusLine endText ]

-- | One account in a list: avatar and names open it, anything else sits on
-- | the right.
accountRow :: forall w i. i -> Account -> Array (HH.HTML w i) -> HH.HTML w i
accountRow open account right =
  HH.div [ cls "account-row" ]
    [ HH.button [ cls "account-main", HP.type_ HP.ButtonButton, HE.onClick \_ -> open ]
        [ avatar "avatar" account.avatar
        , HH.span [ cls "account-names" ]
            [ HH.strong_ [ HH.text (handleName account.displayName account.acct) ]
            , HH.span [ cls "acct" ] [ HH.text ("@" <> account.acct <> if account.locked then " \x1F512" else "") ]
            ]
        ]
    , HH.div [ cls "account-actions" ] right
    ]
