module Form.Submission where

import Prelude      
import Data.Array                       (snoc)
import Data.Const                       (Const(..))
import Data.Maybe                       (Maybe(..))
import Data.Newtype                     (class Newtype)
import Data.Symbol                      (SProxy(..))
import Data.Traversable                 (traverse)
import Effect.Aff.Class                 (class MonadAff)
import Formless                         as F
import Halogen                          as H
import Halogen.HTML                     as HH
import Halogen.HTML.Events              as HE
import Halogen.HTML.Properties          as HP
import Halogen.Media.Component.Upload   as Upload
import Halogen.Media.Data.File          (ExtendedFile(..))
import Halogen.Media.Utils              (filesToFormData)
import Timestamp                        (Timestamp, nowTimestamp)

import Component.HTML.Utils             (css, renderField)
import Data.Attachment                  (AttachmentArray)
import Data.Submission                  (SubmissionId(..), Submission(..))
import Form.Error                       (FormError)
import Form.Validation                  (validateStr, validateStrMaybe)
import Resource.Attachment              (class ManageAttachment
                                        ,uploadAttachment)

newtype SubmissionForm r f = SubmissionForm (r
  ( id          :: f Void      SubmissionId    SubmissionId
  , fullName    :: f FormError String          String
  , pronoun     :: f Void      String          (Maybe String)
  , refund      :: f Void      Boolean         Boolean 
  , airport     :: f Void      String          (Maybe String)
  , title       :: f Void      String          (Maybe String)
  , abstract    :: f Void      String          (Maybe String)
  , bio         :: f FormError String          String
  , comment     :: f Void      String          (Maybe String)
  , email       :: f FormError String          String
  , phoneNumber :: f FormError String          String
  , website     :: f Void      String          (Maybe String)
  , attachments :: f Void      AttachmentArray AttachmentArray
  , createdAt   :: f Void      Timestamp       Timestamp
  , updatedAt   :: f Void      (Maybe Timestamp) (Maybe Timestamp)
  ))

derive instance newtypeSubmissionForm :: Newtype (SubmissionForm r f) _

prx :: F.SProxies SubmissionForm
prx = F.mkSProxies (F.FormProxy :: F.FormProxy SubmissionForm)

type AddedState = ()

type Input = Unit
type ChildSlots = (
  upload :: H.Slot Upload.Query Upload.Output Unit
)
type Query = Const Void

data Action = HandleUpload Upload.Output

component :: forall m
           . Monad m 
          => MonadAff m
          => ManageAttachment m
          => F.Component SubmissionForm Query ChildSlots Input Submission m
component = F.component (const input) F.defaultSpec
  { render = render
  , handleAction = handleAction
  , handleEvent = handleEvent
  }
  where
  input :: F.Input' SubmissionForm m
  input = 
    { initialInputs: Nothing
    , validators: SubmissionForm 
      { id: F.noValidation
      , fullName: validateStr
      , pronoun: validateStrMaybe
      , refund: F.noValidation
      , airport: validateStrMaybe
      , title: validateStrMaybe
      , abstract: validateStrMaybe
      , bio: validateStr
      , comment: validateStrMaybe
      , email: validateStr
      , phoneNumber: validateStr
      , website: validateStrMaybe
      , attachments: F.noValidation
      , createdAt: F.noValidation
      , updatedAt: F.noValidation
      }
    }

  handleAction :: Action
               -> F.HalogenM SubmissionForm AddedState Action ChildSlots Submission m Unit
  handleAction = case _ of
    HandleUpload output -> 
      case output of
        Upload.DroppedFiles files -> do
          state <- H.get
          formData <- H.liftEffect $ filesToFormData "file" files
          newAttachment <- uploadAttachment formData
          _ <- traverse (\(ExtendedFile f uuid t) -> do
            H.query (SProxy :: SProxy "upload") unit (H.tell (Upload.SetUploadStatus uuid true))) files
          -- add attachment to attachments
          case newAttachment of
            Just attachment -> do
              let 
                currAttachments = F.getInput prx.attachments state.form
                newAttachments = snoc currAttachments attachment
              eval $ F.setValidate prx.attachments newAttachments
            Nothing -> pure unit
          pure unit
        _ -> pure unit
    where
      eval act = F.handleAction handleAction handleEvent act

  handleEvent :: F.Event SubmissionForm AddedState
              -> F.HalogenM SubmissionForm AddedState Action ChildSlots Submission m Unit
  handleEvent = case _ of
    F.Submitted form -> do
      now <- H.liftEffect nowTimestamp
      let fields = F.unwrapOutputFields form
      H.raise $ Submission fields { createdAt = now
                                  , updatedAt = Nothing }
    _ -> pure unit

  render :: F.PublicState SubmissionForm AddedState
         -> F.ComponentHTML SubmissionForm Action ChildSlots m
  render st = 
    HH.form_
      [ renderField 
        "Full Name*" 
        (HH.input [ HE.onValueInput $ Just <<< F.setValidate prx.fullName ])
        (F.getError prx.fullName st.form)
      , renderField 
        "Pronoun" 
        (HH.input [ HE.onValueInput $ Just <<< F.setValidate prx.pronoun ])
        Nothing
      , renderField 
        "Email*" 
        (HH.input [ HE.onValueInput $ Just <<< F.setValidate prx.email ])
        (F.getError prx.email st.form)
      , renderField 
        "Phone*" 
        (HH.input [ HE.onValueInput $ Just <<< F.setValidate prx.phoneNumber ])
        (F.getError prx.phoneNumber st.form)
      , renderField 
        "Refund for flight" 
        (HH.input 
          [ HP.type_ HP.InputCheckbox
          , HE.onChecked \_ -> Just $ F.modify prx.refund not
          ])
        Nothing
      , case (F.getInput prx.refund st.form) of
          true  -> 
            renderField
            "City of departure"
            (HH.input [ HE.onValueInput $ Just <<< F.setValidate prx.airport ])
            Nothing
          false -> HH.div [] []
      , renderField 
        "Title" 
        (HH.input [ HE.onValueInput $ Just <<< F.setValidate prx.title ])
        Nothing
      , renderField
        "Abstract"
        (HH.textarea
          [ HP.rows 10
          , HP.cols 60
          , HE.onValueInput $ Just <<< F.setValidate prx.abstract
          ])
        Nothing
      , renderField
        "Bio*"
        (HH.textarea
          [ HP.rows 10
          , HP.cols 60
          , HE.onValueInput $ Just <<< F.setValidate prx.airport 
          ])
        (F.getError prx.bio st.form)
      , renderField
        "Comment"
        (HH.textarea
          [ HP.rows 10
          , HP.cols 60
          , HE.onValueInput $ Just <<< F.setValidate prx.comment
          ])
        Nothing
      , HH.div
        [ css "dropbox-container" ]
        [ HH.label
          []
          [ HH.text "Attachments" ]
        , uploadComponent
        ]
      , HH.div
        [ css "form-action" ]
        [ HH.button
          [ HE.onClick \_ -> Just F.submit
          , css "button"
          ]
          [ HH.text "Submit" ]
        ]
      ]

    where
      uploadComponent = 
        HH.slot
        (SProxy :: _ "upload")
        unit
        (Upload.component)
        unit
        (Just <<< F.injAction <<< HandleUpload)
