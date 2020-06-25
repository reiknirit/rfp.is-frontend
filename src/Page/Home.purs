module Page.Home where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Effect.Class                 (class MonadEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.Component.RawHTML    as RawHTML

import Component.Utils              (OpaqueSlot)
import Component.HTML.Utils         (css)

type State = Maybe Int

data Action = NoAction

type ChildSlots = 
  ( html :: OpaqueSlot Unit )

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: const Nothing
    , render
    , eval: H.mkEval H.defaultEval
    }
  where

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ ]
      [ HH.h1
        [ css "text-center" ]
        [ HH.text "Reykjavík Functional Programming" ]
      , HH.div
        [ css "stream text-center" ]
        [ HH.slot (SProxy :: _ "html") unit RawHTML.component { html: "<iframe src='https://player.twitch.tv/?channel=rfpis&parent=streamernews.example.com' frameborder='0' allowfullscreen='true' scrolling='no' height='378' width='620'></iframe>", elRef: "testDiv" } absurd 
        ]
      ]
