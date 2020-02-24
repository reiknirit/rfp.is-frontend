module Page.Submission where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Effect.Class.Console         (logShow)
import Effect.Aff.Class             (class MonadAff)
import Formless                     as F
import Halogen                      as H
import Halogen.HTML                 as HH

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css)
import Data.Submission              (Submission)
import Form.Submission              as SubmissionForm
import Resource.Attachment          (class ManageAttachment)
import Resource.Submission          (class ManageSubmission
                                    ,createSubmission)

type State = 
  { statusMessage :: Maybe String
  , complete :: Boolean
  }

data Action 
  = Initialize
  | HandleSubmission Submission

type ChildSlots = (
  formless :: H.Slot (F.Query SubmissionForm.SubmissionForm (Const Void) SubmissionForm.ChildSlots) Submission Unit
)
type Query = Const Void

initialState :: State
initialState = 
  { statusMessage: Nothing
  , complete: false
  }

component :: forall m
           . Monad m
          => MonadAff m
          => ManageAttachment m
          => ManageSubmission m
          => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
    }
  where
  handleAction = case _ of
    Initialize -> pure unit
    HandleSubmission submission -> do
      newSubmission <- createSubmission submission
      case newSubmission of
        Just submission -> 
          H.modify_ _ 
            { statusMessage = Just "Thank you for your submission. We have sent a confirmation to your email address."
            , complete = true
            }
        Nothing -> 
          H.modify_ _ 
            { statusMessage = Just "An error occured"
            , complete = false
            }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ case state.complete of
        true  -> HH.div_ []
        false -> submissionForm 
      , case state.statusMessage of
          Just msg -> 
            HH.div
              [ css "status-message" ]
              [ HH.p 
                []
                [ HH.text msg ]
              ]
          Nothing -> HH.div_ []
      ]

    where
      submissionForm = 
        HH.slot
        (SProxy :: _ "formless") 
        unit
        SubmissionForm.component
        unit
        (Just <<< HandleSubmission)
