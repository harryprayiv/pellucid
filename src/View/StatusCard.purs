-- | Renders one status in the feed. A plain render function rather than a
-- | component: cards have no state of their own, the feed owns it.
module View.StatusCard
  ( CardAction(..)
  , CardContext
  , statusCard
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (null, trim)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pellucid.Entity (AccountId, MediaAttachment, MediaType(..), Status(..), StatusId, displayed)
import Util.Time (parseIso, relativeLabel)

data CardAction
  = ToggleFavourite
  | ToggleBookmark
  | Reveal
  | OpenAccount AccountId
  | OpenStatus StatusId

type CardContext =
  { now :: Maybe Instant
  , revealed :: Boolean
  , busy :: Boolean
  }

statusCard :: forall w i. CardContext -> (CardAction -> i) -> Status -> HH.HTML w i
statusCard ctx emit outer =
  let
    Status shown = displayed outer
    Status top = outer
    boostedBy = case top.reblog of
      Just _ -> Just top.account
      Nothing -> Nothing
    caption = captionText shown.contentText shown.content
    showMedia = not shown.sensitive || ctx.revealed
  in
    HH.article [ cls "card" ]
      [ case boostedBy of
          Just booster -> HH.div [ cls "card-boost" ] [ HH.text (handle booster.displayName booster.acct <> " boosted") ]
          Nothing -> HH.text ""
      , HH.header [ cls "card-header" ]
          [ HH.button
              [ cls "avatar-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenAccount shown.account.id) ]
              [ HH.img [ cls "avatar", HP.src shown.account.avatar, HP.alt "", lazy ] ]
          , HH.div [ cls "card-names" ]
              [ HH.button
                  [ cls "name-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenAccount shown.account.id) ]
                  [ HH.strong_ [ HH.text (handle shown.account.displayName shown.account.acct) ] ]
              , HH.span [ cls "acct" ] [ HH.text ("@" <> shown.account.acct) ]
              ]
          , HH.span [ cls "card-time", HP.title shown.createdAt ] [ HH.text (ago ctx.now shown.createdAt) ]
          ]
      , if Array.null shown.mediaAttachments then HH.text ""
        else if showMedia then mediaBlock shown.mediaAttachments
        else sensitiveCover emit shown.spoilerText
      , HH.div [ cls "card-actions" ]
          [ HH.button
              [ cls ("action" <> if shown.favourited then " action-active" else "")
              , HP.type_ HP.ButtonButton
              , HP.disabled ctx.busy
              , HP.title (if shown.favourited then "Unlike" else "Like")
              , HE.onClick \_ -> emit ToggleFavourite
              ]
              [ HH.text (if shown.favourited then "\x2665" else "\x2661"), count shown.favouritesCount ]
          , HH.button
              [ cls "action"
              , HP.type_ HP.ButtonButton
              , HP.title "Comments"
              , HE.onClick \_ -> emit (OpenStatus shown.id)
              ]
              [ HH.text "\x1F4AC", count shown.replyCount ]
          , HH.button
              [ cls ("action" <> if shown.bookmarked then " action-active" else "")
              , HP.type_ HP.ButtonButton
              , HP.disabled ctx.busy
              , HP.title (if shown.bookmarked then "Remove bookmark" else "Bookmark")
              , HE.onClick \_ -> emit ToggleBookmark
              ]
              [ HH.text (if shown.bookmarked then "\x1F516" else "\x1F4D1") ]
          ]
      , if null (trim shown.spoilerText) || not (Array.null shown.mediaAttachments) then HH.text ""
        else HH.div [ cls "card-spoiler" ] [ HH.text shown.spoilerText ]
      , if null caption then HH.text ""
        else HH.p [ cls "card-caption" ] [ HH.text caption ]
      , case shown.place of
          Just place -> HH.div [ cls "card-place" ]
            [ HH.text (place.name <> maybe "" (", " <> _) place.country) ]
          Nothing -> HH.text ""
      ]

mediaBlock :: forall w i. Array MediaAttachment -> HH.HTML w i
mediaBlock attachments =
  HH.div [ cls (if Array.length attachments > 1 then "media media-album" else "media") ]
    (map mediaItem attachments)

mediaItem :: forall w i. MediaAttachment -> HH.HTML w i
mediaItem m = case m.mediaType of
  VideoMedia -> video
  Gifv -> video
  _ ->
    HH.img
      [ cls "media-item"
      , HP.src (fromMaybe m.url m.optimizedUrl)
      , HP.alt (fromMaybe "" m.description)
      , lazy
      ]
  where
  video =
    HH.video
      ( [ cls "media-item"
        , HP.src m.url
        , HP.controls true
        , HP.preload HP.PreloadMetadata
        , HP.attr (HH.AttrName "playsinline") ""
        ] <> maybe [] (\p -> [ HP.poster p ]) m.previewUrl
      )
      []

sensitiveCover :: forall w i. (CardAction -> i) -> String -> HH.HTML w i
sensitiveCover emit spoiler =
  HH.div [ cls "media sensitive-cover" ]
    [ HH.p_ [ HH.text (if null (trim spoiler) then "Sensitive content" else spoiler) ]
    , HH.button [ cls "reveal", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit Reveal ] [ HH.text "Show" ]
    ]

count :: forall w i. Int -> HH.HTML w i
count n
  | n <= 0 = HH.text ""
  | otherwise = HH.span [ cls "count" ] [ HH.text (" " <> show n) ]

handle :: String -> String -> String
handle displayName acct = if null (trim displayName) then acct else displayName

ago :: Maybe Instant -> String -> String
ago now createdAt = case now, parseIso createdAt of
  Just n, Just t -> relativeLabel n t
  _, _ -> ""

-- | Pellucid's `content_text` is the plain caption. Without it (an older
-- | server, or an entity that came without `_pe=1`) the HTML `content` is
-- | reduced to text. This is a display fallback, never an HTML renderer.
captionText :: Maybe String -> String -> String
captionText contentText content = trim case contentText of
  Just t -> t
  Nothing -> stripTags content

stripTags :: String -> String
stripTags =
  replace (unsafeRegex "<br\\s*/?>" global) "\n"
    >>> replace (unsafeRegex "</p>\\s*<p>" global) "\n\n"
    >>> replace (unsafeRegex "<[^>]*>" global) ""
    >>> replace (unsafeRegex "&amp;" global) "&"
    >>> replace (unsafeRegex "&lt;" global) "<"
    >>> replace (unsafeRegex "&gt;" global) ">"
    >>> replace (unsafeRegex "&quot;" global) "\""
    >>> replace (unsafeRegex "&#39;" global) "'"

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

lazy :: forall r i. HP.IProp r i
lazy = HP.attr (HH.AttrName "loading") "lazy"
