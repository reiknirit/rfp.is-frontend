module Component.Router where

import Prelude

import Data.Either                      (hush)
import Data.Maybe                       (fromMaybe, Maybe(..))
import Data.Symbol                      (SProxy(..))
import Effect.Aff.Class                 (class MonadAff)
import Halogen                          (liftEffect)
import Halogen                          as H
import Halogen.HTML                     as HH
import Routing.Duplex                   as RD
import Routing.Hash                     (getHash)

import Capability.Navigate              (class Navigate, navigate)
import Component.Utils                  (OpaqueSlot)
import Component.HTML.Utils             (css)
import Data.Route                       (Route(..), routeCodec)
import Page.Home                        as Home
import Page.Submission                  as Submission
import Resource.Attachment              (class ManageAttachment)
import Resource.Submission              (class ManageSubmission)

type State = 
  { route :: Maybe Route }

data Query a
  = Navigate Route a 

data Action 
  = Initialize

type ChildSlots = 
  ( home :: OpaqueSlot Unit 
  , submission :: OpaqueSlot Unit
  )

wrapperContainer :: forall props act
                  . HH.HTML props act
                 -> HH.HTML props act
wrapperContainer slot = 
  HH.div
    [ css "main-wrapper" ]
    [ HH.div
      [ css "transparent-layer" ]
      []
    , HH.div
      [ css "content-wrapper" ]
      [ slot ]
    ]

component
  :: forall m 
   . MonadAff m
  => ManageAttachment m
  => ManageSubmission m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m 
component = H.mkComponent 
  { initialState: \_ -> { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      when (route /= Just dest) do
         H.modify_ _ { route = Just dest }
      pure (Just a)
  
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just Home -> 
      wrapperContainer $ HH.slot (SProxy :: _ "home") unit Home.component unit absurd
    Just Submission ->
      wrapperContainer $ HH.slot (SProxy :: _ "submission") unit Home.component unit absurd
    Nothing ->
      wrapperContainer $ HH.div_ [ HH.text "Oh no! That page wasn't found." ]
