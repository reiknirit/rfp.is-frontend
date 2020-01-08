module Form.Error where

import Prelude
import Data.Generic.Rep             (class Generic)
import Data.Generic.Rep.Show        (genericShow)

data FormError 
  = Required          -- Field input is required
  | Invalid           -- Field input is invalid
  | InvalidInt String
  | InvalidNumber String
  | InvalidDateTime String

derive instance genericFormError :: Generic FormError _

instance showFormError :: Show FormError where
  show = genericShow

errorToString :: FormError -> String
errorToString error = case error of
  Required            -> "This field is required"
  Invalid             -> "This field is invalid"
  InvalidInt str      -> "Value is not an integer"
  InvalidNumber str   -> "Value is not a number"
  InvalidDateTime str -> "DateTime is not valid"
