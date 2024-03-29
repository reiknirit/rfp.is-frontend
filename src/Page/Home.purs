module Page.Home where

import Prelude
import Data.Const                   (Const)
import Data.Maybe                   (Maybe(..))
import Data.Symbol                  (SProxy(..))
import Effect.Class                 (class MonadEffect)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Properties      as HP
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
        [ HH.slot (SProxy :: _ "html") unit RawHTML.component { html: "<iframe src='https://player.twitch.tv/?channel=rfpis&parent=rfp.is' frameborder='0' allowfullscreen='true' scrolling='no' height='378' width='620'></iframe>", elRef: "testDiv" } absurd 
        ]
      , HH.p
        [ css "text-center" ]
        [ HH.text "Our next meetup will be online, June 5th 2021 at 5:30 PM. Join us on Zoom:"]
      , HH.a
        [ css "text-center"
        , HP.href "https://us02web.zoom.us/j/82053521996" ]
        [ HH.text "https://us02web.zoom.us/j/82053521996" ]
      ]
