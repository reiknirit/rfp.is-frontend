module Resource.Attachment where

import Prelude
import Data.Maybe         (Maybe(..))
import Halogen            (HalogenM, lift)
import Web.XHR.FormData   as FD

import Data.Attachment    (Attachment)

class Monad m <= ManageAttachment m where
  uploadAttachment :: FD.FormData -> m (Maybe Attachment)


instance manageAttachmentHalogenM :: ManageAttachment m => ManageAttachment (HalogenM st act slots msg m) where
  uploadAttachment = lift <<< uploadAttachment
