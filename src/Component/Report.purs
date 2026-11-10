-- | Reporting a post or an account to the instance's moderators: one of the
-- | server's nine reasons and an optional note. The report goes to the
-- | viewer's own instance, which is where its moderators read it.
module Component.Report (component) where

import Prelude

import App.Backend (printAppError)
import App.Monad (Env)
import App.Route (goBack)
import App.Screen as Screen
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.String as String
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pixelfed.Client (report)
import Pixelfed.Entity (ReportReason, ReportTarget(..), reportReasonLabel, reportReasonName, reportReasons)
import View.Common (cls, screenHeader)
import Web.Event.Event (Event, preventDefault)

-- | The server refuses a note longer than 255 bytes after cleaning.
maxNoteLength :: Int
maxNoteLength = 255

data Phase = Editing | Sending | Sent | Failed String

type State =
  { target :: ReportTarget
  , reason :: Maybe ReportReason
  , note :: String
  , phase :: Phase
  }

data Action
  = Receive ReportTarget
  | Back
  | Pick ReportReason
  | SetNote String
  | Submit Event

component :: forall m. MonadAff m => MonadAsk Env m => H.Component Screen.Query ReportTarget Screen.Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, receive = Just <<< Receive }
  }

initialState :: ReportTarget -> State
initialState target = { target, reason: Nothing, note: "", phase: Editing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section [ cls "report" ]
    [ screenHeader Back title []
    , case state.phase of
        Sent ->
          HH.div [ cls "stack-form" ]
            [ HH.p_ [ HH.text "Report sent. This server's moderators will review it." ]
            , HH.button [ HP.type_ HP.ButtonButton, HE.onClick \_ -> Back ] [ HH.text "Done" ]
            ]
        _ -> form state
    ]
  where
  title = case state.target of
    AboutPost _ -> "Report post"
    AboutAccount _ -> "Report account"

form :: forall m. State -> H.ComponentHTML Action () m
form state =
  HH.form [ cls "stack-form", HE.onSubmit Submit ]
    [ HH.p [ cls "muted" ] [ HH.text "What is wrong with it?" ]
    , HH.div [ cls "reasons" ] (map reason reportReasons)
    , HH.label [ cls "field" ]
        [ HH.span [ cls "field-label" ]
            [ HH.text "Note for the moderators (optional)"
            , HH.span [ cls "counter-text" ] [ HH.text (show (String.length state.note) <> "/" <> show maxNoteLength) ]
            ]
        , HH.textarea
            [ HP.value state.note
            , HP.rows 3
            , HP.attr (HH.AttrName "maxlength") (show maxNoteLength)
            , HP.disabled busy
            , HE.onValueInput SetNote
            ]
        ]
    , HH.button [ HP.type_ HP.ButtonSubmit, HP.disabled (busy || isNothing state.reason) ]
        [ HH.text (if busy then "Sending\x2026" else "Send report") ]
    , case state.phase of
        Failed message -> HH.p [ cls "error" ] [ HH.text message ]
        _ -> HH.text ""
    ]
  where
  busy = case state.phase of
    Sending -> true
    _ -> false

  reason r =
    HH.label [ cls "check" ]
      [ HH.input
          [ HP.type_ HP.InputRadio
          , HP.name "reason"
          , HP.value (reportReasonName r)
          , HP.checked (state.reason == Just r)
          , HP.disabled busy
          , HE.onChecked \_ -> Pick r
          ]
      , HH.span_ [ HH.text (reportReasonLabel r) ]
      ]

handleAction :: forall m. MonadAff m => MonadAsk Env m => Action -> H.HalogenM State Action () Screen.Output m Unit
handleAction = case _ of
  Receive target -> do
    current <- H.gets _.target
    when (target /= current) (H.put (initialState target))

  Back -> liftEffect goBack

  Pick r -> H.modify_ _ { reason = Just r, phase = Editing }

  SetNote text -> H.modify_ _ { note = String.take maxNoteLength text, phase = Editing }

  Submit event -> do
    liftEffect (preventDefault event)
    state <- H.get
    case state.reason, state.phase of
      Just _, Sending -> pure unit
      Just _, Sent -> pure unit
      Just r, _ -> do
        backend <- asks _.backend
        H.modify_ _ { phase = Sending }
        result <- liftAff (report backend state.target r state.note)
        H.modify_ _
          { phase = case result of
              Left err -> Failed (printAppError err)
              Right _ -> Sent
          }
      Nothing, _ -> pure unit
