module Data.Submission where

import Prelude
import Data.Argonaut            (decodeJson
                                ,encodeJson
                                ,(~>), (:=)
                                ,(.:), (.:?))
import Data.Argonaut.Encode     (class EncodeJson)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)
import Data.Maybe               (Maybe(..))
import Data.Newtype             (class Newtype)
import Formless                 as F
import Timestamp                (Timestamp)

import Data.Attachment          (Attachment(..), AttachmentArray)

newtype SubmissionId = SubmissionId Int

derive instance newtypeSubmissionId :: Newtype SubmissionId _
derive instance genericSubmissionId :: Generic SubmissionId _
derive instance eqSubmissionId :: Eq SubmissionId
derive instance ordSubmissionId :: Ord SubmissionId

derive newtype instance encodeJsonSubmissionId :: EncodeJson SubmissionId
derive newtype instance decodeJsonSubmissionId :: DecodeJson SubmissionId

instance showSubmissionId :: Show SubmissionId where
  show = genericShow

instance initialSubmissionId :: F.Initial SubmissionId where
  initial = SubmissionId 0

newtype Submission = Submission 
  { id :: SubmissionId
  , fullName :: String
  , pronoun :: Maybe String
  , refund :: Boolean
  , airport :: Maybe String
  , title :: Maybe String
  , abstract :: Maybe String
  , bio :: String
  , comment :: Maybe String
  , email :: String
  , phoneNumber :: String
  , website :: Maybe String
  , attachments :: AttachmentArray
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance newtypeSubmission :: Newtype Submission _
derive instance genericSubmission :: Generic Submission _
derive instance eqSubmission :: Eq Submission
derive instance ordSubmission :: Ord Submission

instance encodeJsonSubmission :: EncodeJson Submission where
  encodeJson (Submission submission)
    = "fullName"     := submission.fullName
    ~> "pronoun"     := submission.pronoun
    ~> "refund"      := submission.refund
    ~> "airport"     := submission.airport
    ~> "title"       := submission.title
    ~> "abstract"    := submission.abstract 
    ~> "bio"         := submission.bio
    ~> "comment"     := submission.comment
    ~> "email"       := submission.email
    ~> "phoneNumber" := submission.phoneNumber
    ~> "website"     := submission.website
    ~> "attachments" := map (\(Attachment attachment) -> attachment.id) submission.attachments
    ~> "createdAt"   := submission.createdAt
    ~> "updatedAt"   := submission.updatedAt

derive newtype instance decodeJsonSubmission :: DecodeJson Submission

instance showSubmission :: Show Submission where
  show = genericShow
