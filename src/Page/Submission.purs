module Page.Submission where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Effect.Aff.Class             (class MonadAff)
import Formless                     as F
import Halogen                      as H
import Halogen.HTML                 as HH

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css)
import Data.Submission            (Submission)
import Form.Submission            as SubmissionForm

type State = {}

data Action 
  = Initialize
  | HandleSubmission Submission

type ChildSlots = (
  formless :: H.Slot (F.Query SubmissionForm.SubmissionForm (Const Void) SubmissionForm.ChildSlots) Submission Unit
)
type Query = Const Void

initialState :: State
initialState = {}

component :: forall m
           . Monad m
          => MonadAff m
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
    HandleSubmission submission -> pure unit

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div 
      [] 
      [ submissionForm ]

    where
      submissionForm = 
        HH.slot
        (SProxy :: _ "formless") 
        unit
        SubmissionForm.component
        unit
        (Just <<< HandleSubmission)
