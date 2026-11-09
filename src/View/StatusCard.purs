-- | Renders one status in a feed. A plain render function rather than a
-- | component: cards have no state of their own, the screen owns it.
module View.StatusCard
  ( CardAction(..)
  , CardContext
  , statusCard
  , mediaBlock
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (null, trim)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Entity (AccountId, MediaAttachment, MediaType(..), Status(..), StatusId, captionOf, displayed)
import View.Common (agoLabel, avatar, cls, handleName, lazy)

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
  -- | False on the post's own detail screen, where the comments are below.
  , commentsOpenDetail :: Boolean
  }

statusCard :: forall w i. CardContext -> (CardAction -> i) -> Status -> HH.HTML w i
statusCard ctx emit outer =
  let
    Status shown = displayed outer
    Status top = outer
    boostedBy = case top.reblog of
      Just _ -> Just top.account
      Nothing -> Nothing
    caption = captionOf shown
    showMedia = not shown.sensitive || ctx.revealed
  in
    HH.article [ cls "card" ]
      [ case boostedBy of
          Just booster -> HH.div [ cls "card-boost" ] [ HH.text (handleName booster.displayName booster.acct <> " boosted") ]
          Nothing -> HH.text ""
      , HH.header [ cls "card-header" ]
          [ HH.button
              [ cls "avatar-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenAccount shown.account.id) ]
              [ avatar "avatar" shown.account.avatar ]
          , HH.div [ cls "card-names" ]
              [ HH.button
                  [ cls "name-button", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenAccount shown.account.id) ]
                  [ HH.strong_ [ HH.text (handleName shown.account.displayName shown.account.acct) ] ]
              , HH.span [ cls "acct" ] [ HH.text ("@" <> shown.account.acct) ]
              ]
          , HH.span [ cls "card-time", HP.title shown.createdAt ] [ HH.text (agoLabel ctx.now shown.createdAt) ]
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
              , HP.disabled (not ctx.commentsOpenDetail)
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