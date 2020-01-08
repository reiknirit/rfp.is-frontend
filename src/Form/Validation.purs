module Form.Validation where

import Prelude
import Data.Either              (Either(..))
import Data.Maybe               (Maybe(..))
import Data.String              (null)
import Formless                 as F

import Form.Error               (FormError(..))

validateStr :: forall form m. Monad m
            => F.Validation form m FormError String String
validateStr = F.hoistFnE_ \str ->
  if null str
    then Left Required
    else Right str

-- | Helper validation for strings that
-- never fails but returns Just x | Nothing
-- depending on if the string being validated
-- is empty or not
validateStrMaybe :: forall form m
                  . Monad m
                 => F.Validation form m Void String (Maybe String)
validateStrMaybe = F.hoistFnE_ \str ->
  if null str
    then Right $ Nothing
    else Right $ Just str
