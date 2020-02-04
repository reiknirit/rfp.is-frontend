module Component.HTML.Utils where

import Prelude
import Data.Maybe               (Maybe(..))
import Halogen.HTML             as HH
import Halogen.HTML.Properties  as HP
import Routing.Duplex           (print)

import Data.Route               (Route, routeCodec)
import Form.Error               (FormError(..), errorToString)

css :: forall r i. String 
    -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

classes_ :: forall p i. Array String 
         -> HH.IProp (class :: String | i) p
classes_ = HP.classes 
         <<< map HH.ClassName

whenElem :: forall p i. Boolean 
         -> (Unit -> HH.HTML p i) 
         -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

safeHref :: forall r i. Route 
         -> HH.IProp ( href :: String | r) i
safeHref = HP.href 
         <<< append "#" 
         <<< print routeCodec

renderField :: forall i p
             . String 
            -> String
            -> HH.HTML i p
            -> (Maybe FormError) 
            -> HH.HTML i p
renderField label title control err =
  HH.div
    [ css "field" ]
    [ HH.label
      [ HP.title title ]
      [ HH.text label
      , control ]
    , HH.div
      [ css "error" ]
      [ HH.span
        []
        [ case err of
          Just e -> HH.text $ errorToString e
          Nothing -> HH.text ""
        ]
      ]
    ]
