module Api.Endpoint where

import Prelude
import Data.Generic.Rep                 (class Generic)
import Data.Generic.Rep.Show            (genericShow)
import Routing.Duplex                   (RouteDuplex'
                                        ,root)
import Routing.Duplex.Generic           (sum, noArgs)
import Routing.Duplex.Generic.Syntax    ((/))

data Endpoint 
  = AttachmentUpload
  | CreateSubmission

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "AttachmentUpload" : "attachments" / "upload" / noArgs
  , "CreateSubmission" : "submissions" / noArgs
  }

