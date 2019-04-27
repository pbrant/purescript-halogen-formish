module Formlet where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, inj)
import Halogen.HTML (ComponentHTML, input) as HH
import Halogen.HTML.Events (onValueInput) as HE
import Halogen.HTML.Properties (InputType(..), type_, value) as HP

type Actions form v =
  ( modify :: form -> form
  | v
  )

newtype UI action slots m = UI
  { html :: Array (HH.ComponentHTML action slots m)
  }

instance semigroupUI :: Semigroup (UI action slots m) where
  append (UI { html: html1 }) (UI { html: html2}) =
    UI { html: html1 <> html2 }

instance monoidUI :: Monoid (UI action slots m) where
  mempty = UI { html: [] }

newtype Form i action slots m a =
  Form (i -> { ui :: (i -> action) -> UI action slots m, result :: Maybe a })

derive instance functorForm :: Functor (Form i action slots m)

instance applyForm :: Apply (Form i action slots m) where
  apply (Form ff) (Form fa) =
    Form \i ->
      let ffr = ff i
          far = fa i
      in
        { ui: \cb -> ffr.ui cb <> far.ui cb
        , result: ffr.result <*> far.result
        }

instance applicativeForm :: Applicative (Form i action slots m) where
  pure a = Form \i -> { ui: mempty, result: pure a }

textBox
  :: forall v slots m
   . Form String (Variant (Actions String v)) slots m String
textBox = Form \input ->
  { ui: \cb -> UI
          { html:
              [ HH.input
                  [ HP.type_ HP.InputText
                  , HE.onValueInput \s ->
                      Just $ inj (SProxy :: SProxy "modify") (const s)
                  , HP.value input
                  ]
              ]
          }
  , result: Just input
  }
