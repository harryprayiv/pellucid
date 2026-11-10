-- | Renders one status in a feed. A plain render function rather than a
-- | component: cards have no state of their own, the screen owns it.
module View.StatusCard
  ( CardAction(..)
  , CardContext
  , statusCard
  , mediaBlock
  , captionView
  , handleCardNavigation
  ) where

import Prelude

import App.Route (Route(..), navigate)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (null, trim)
import Effect (Effect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Entity (AccountId, MediaAttachment, MediaType(..), Mention, Status(..), StatusId, captionOf, displayed)
import Pixelfed.StatusOps (canShare)
import Util.Caption (Segment(..), segments)
import View.Common (agoLabel, avatar, cls, handleName, lazy)

data CardAction
  = ToggleFavourite
  | ToggleBookmark
  | ToggleShare
  | Reveal
  | OpenAccount AccountId
  | OpenStatus StatusId
  | OpenTag String
  | OpenLikes StatusId
  | OpenShares StatusId

type CardContext =
  { now :: Maybe Instant
  , revealed :: Boolean
  , busy :: Boolean
  -- | False on the post's own detail screen, where the comments are below.
  , commentsOpenDetail :: Boolean
  }

-- | Where a card action that only navigates leads. `Nothing` for the
-- | actions the screen has to carry out itself.
handleCardNavigation :: CardAction -> Maybe (Effect Unit)
handleCardNavigation = case _ of
  OpenAccount id -> Just (navigate (Profile id))
  OpenStatus id -> Just (navigate (StatusDetail id))
  OpenTag name -> Just (navigate (Tag name))
  OpenLikes id -> Just (navigate (LikedBy id))
  OpenShares id -> Just (navigate (SharedBy id))
  _ -> Nothing

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
          Just booster -> HH.div [ cls "card-boost" ] [ HH.text (handleName booster.displayName booster.acct <> " shared") ]
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
          , HH.span [ cls "card-time", HP.title shown.createdAt ]
              [ HH.text (agoLabel ctx.now shown.createdAt <> if shown.editedAt == Nothing then "" else " \x00B7 edited") ]
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
          , if canShare (Status shown) then
              HH.button
                [ cls ("action" <> if shown.reblogged then " action-shared" else "")
                , HP.type_ HP.ButtonButton
                , HP.disabled ctx.busy
                , HP.title (if shown.reblogged then "Undo share" else "Share")
                , HE.onClick \_ -> emit ToggleShare
                ]
                [ HH.text "\x1F501", count shown.reblogsCount ]
            else HH.text ""
          , HH.button
              [ cls ("action action-end" <> if shown.bookmarked then " action-active" else "")
              , HP.type_ HP.ButtonButton
              , HP.disabled ctx.busy
              , HP.title (if shown.bookmarked then "Remove bookmark" else "Bookmark")
              , HE.onClick \_ -> emit ToggleBookmark
              ]
              [ HH.text (if shown.bookmarked then "\x1F516" else "\x1F4D1") ]
          ]
      , engagement emit shown.id shown.favouritesCount shown.reblogsCount
      , if null (trim shown.spoilerText) || not (Array.null shown.mediaAttachments) then HH.text ""
        else HH.div [ cls "card-spoiler" ] [ HH.text shown.spoilerText ]
      , if null caption then HH.text ""
        else HH.p [ cls "card-caption" ] (captionView emit shown.mentions caption)
      , case shown.place of
          Just place -> HH.div [ cls "card-place" ]
            [ HH.text (place.name <> maybe "" (", " <> _) place.country) ]
          Nothing -> HH.text ""
      ]

-- | The line under the actions that opens who liked and who shared. Drawn
-- | only when there is something to open.
engagement :: forall w i. (CardAction -> i) -> StatusId -> Int -> Int -> HH.HTML w i
engagement emit id likes shares
  | likes <= 0 && shares <= 0 = HH.text ""
  | otherwise =
      HH.div [ cls "card-engagement" ]
        [ if likes > 0 then
            HH.button [ cls "link", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenLikes id) ]
              [ HH.text (plural likes "like") ]
          else HH.text ""
        , if shares > 0 then
            HH.button [ cls "link", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenShares id) ]
              [ HH.text (plural shares "share") ]
          else HH.text ""
        ]
      where
      plural n word = show n <> " " <> word <> if n == 1 then "" else "s"

-- | A caption as text nodes and buttons. Hashtags open their feed; a mention
-- | the server listed opens that account. Nothing is parsed as HTML.
captionView :: forall w i. (CardAction -> i) -> Array Mention -> String -> Array (HH.HTML w i)
captionView emit mentions caption = map piece (segments mentions caption)
  where
  piece = case _ of
    Plain text -> HH.text text
    TagLink name ->
      HH.button [ cls "link", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenTag name) ] [ HH.text ("#" <> name) ]
    MentionLink id text ->
      HH.button [ cls "link", HP.type_ HP.ButtonButton, HE.onClick \_ -> emit (OpenAccount id) ] [ HH.text text ]

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
