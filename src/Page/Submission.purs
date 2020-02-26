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
        false -> 
          HH.div
            []
            [ HH.h1
              [ css "text-center" ]
              [ HH.text "Software Correctness Iceland. August 18 - 20th 2020" 
              ]
            , HH.p
              [ css "text-center about" ]
              [ HH.text "The Computer Science Society invites software developers, testers and researchers from around the world to submit abstracts for presentation at the conference Software Correctness 2020 in Reykjavík, Iceland. Presentations and lectures will take place on  Tuesday, Wednesday, and Thursday, August 18.–20. 2020. Abstracts will be reviewed by faculty of Computer Science of the University of Iceland. All accepted presenters will be offered travel expense reimbursement, a speaker's networking event at an exotic destination in Iceland and a chance to publish an article in the conference proceedings." 
              ]
            , HH.p
              [ css "text-center about" ]
              [ HH.text "Hosted in cooperation with Reykjavík Functional Programming and the University of Iceland, this new conference will bring together software developers, quality assurance engineers and researchers. Prospective topics include the specification and verification of software and software design, type-driven code generation, test case generation, static analysis and advances in programming language design. Other possible topics are stories of past and present successes and failures of methods for ensuring correctness as well as predictions for future methods." 
              ]
            , submissionForm 
            ]
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
