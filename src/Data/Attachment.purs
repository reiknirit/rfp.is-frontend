module Data.Attachment where

import Prelude
import Data.Argonaut.Encode     (class EncodeJson)
import Data.Argonaut.Decode     (class DecodeJson)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)
import Data.Maybe               (Maybe(..))
import Data.Newtype             (class Newtype)
import Timestamp                (Timestamp)


newtype AttachmentId = AttachmentId Int

derive instance newtypeAttachmentId :: Newtype AttachmentId _
derive instance genericAttachmentId :: Generic AttachmentId _
derive instance eqAttachmentId :: Eq AttachmentId
derive instance ordAttachmentId :: Ord AttachmentId

derive newtype instance encodeJsonAttachmentId :: EncodeJson AttachmentId
derive newtype instance decodeJsonAttachmentId :: DecodeJson AttachmentId

instance showAttachmentId :: Show AttachmentId where
  show = genericShow


newtype Attachment = Attachment 
  { id        :: AttachmentId
  , name      :: String
  , src       :: String
  , thumbnail :: Maybe String
  , createdAt :: Timestamp
  , updatedAt :: Maybe Timestamp
  }

derive instance newtypeAttachment :: Newtype Attachment _
derive instance genericAttachment :: Generic Attachment _
derive instance eqAttachment :: Eq Attachment
derive instance ordAttachment :: Ord Attachment

derive newtype instance encodeJsonAttachment :: EncodeJson Attachment
derive newtype instance decodeJsonAttachment :: DecodeJson Attachment

instance showAttachment :: Show Attachment where
  show = genericShow

type AttachmentArray = Array Attachment
