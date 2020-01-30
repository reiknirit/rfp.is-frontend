module Resource.Submission where

import Prelude
import Data.Maybe           (Maybe(..))
import Halogen              (HalogenM, lift)

import Data.Submission      (Submission)

class Monad m <= ManageSubmission m where
  createSubmission :: Submission -> m (Maybe Submission)

instance manageSubmissionHalogenM :: ManageSubmission m => ManageSubmission (HalogenM st act slots msg m) where
  createSubmission = lift <<< createSubmission

