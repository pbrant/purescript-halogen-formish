module Formlet
  ( Env(..)
  , Form(..)
  , FormAction(..)
  , UI(..)
  , UISpec
  , UIState
  , Validated(..)
  , Wizard
  , focus
  , input
  , labeled
  , mustEqual
  , nonNull
  , passwordBox
  , runUI
  , step
  , textBox
  , validatedField
  , wizard
  , wrapped
  ) where

import Prelude

import Data.Array ((:))
import Data.Array (null, singleton) as Array
import Data.Either (Either(..), either, hush)
import Data.Lens (ALens', APrism', Lens', _Just, cloneLens, clonePrism, review, set, view, (%=), (^?))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Newtype (over, unwrap) as Newtype
import Data.String as String
import Halogen (HalogenM) as H
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, div, input, label_, text) as HH
import Halogen.HTML.Events (onValueInput) as HE
import Halogen.HTML.Properties (InputType(..), class_, type_, value) as HP

-- See
-- https://medium.com/fuzzy-sharp/building-a-type-safe-embedded-dsl-for-form-components-with-validation-e7ffaaf537e4
-- for details

newtype UI action slots m = UI (Array (HH.ComponentHTML action slots m))
derive instance newtypeUI :: Newtype (UI action slots m) _

instance semigroupUI :: Semigroup (UI action slots m) where
  append (UI html1) (UI html2) =
    UI $ html1 <> html2

instance monoidUI :: Monoid (UI action slots m) where
  mempty = UI []

type Env r =
  { formName :: String
  | r
  }

newtype Form i r action slots m a =
  Form (i -> Env r -> Int ->
          { ui :: (i -> Maybe action) -> UI action slots m
          , result :: Maybe a
          , idCounter :: Int
          })

derive instance functorForm :: Functor (Form i env action slots m)

instance applyForm :: Apply (Form i env action slots m) where
  apply (Form ff) (Form fa) =
    Form \i env counter ->
      let ffr = ff i env counter
          far = fa i env ffr.idCounter
      in
        { ui: \onChange -> ffr.ui onChange <> far.ui onChange
        , result: ffr.result <*> far.result
        , idCounter: far.idCounter
        }

instance applicativeForm :: Applicative (Form i env action slots m) where
  pure a = Form \_ _ _ -> { ui: mempty, result: pure a, idCounter: 0 }

newtype Wizard i r action slots m a = Wizard (Form i r action slots m a)

derive newtype instance functorWizard :: Functor (Wizard i r action slots m)

instance applyWizard :: Apply (Wizard i r action slots m) where
  apply = ap

derive newtype instance applicativeWizard :: Applicative (Wizard i r action slots m)

instance bindWizard :: Bind (Wizard i r action slots m) where
  bind (Wizard (Form fa)) f =
    Wizard <<< Form $ \i env counter ->
      let far = fa i env counter
          bind_ r =
            let Wizard (Form ff) = f r
                ffr = ff i env far.idCounter
            in
              { ui: \onChange -> far.ui onChange <> ffr.ui onChange
              , result: ffr.result
              , idCounter: ffr.idCounter
              }
      in far.result # maybe
          { ui: far.ui
          , result: Nothing
          , idCounter: far.idCounter
          }
          bind_

instance monadWizard :: Monad (Wizard i r action slots m)

wizard :: forall i r action slots m. Wizard i r action slots m ~> Form i r action slots m
wizard (Wizard form) = form

step :: forall i r action slots m. Form i r action slots m ~> Wizard i r action slots m
step = Wizard

newtype Validated a = Validated
  { value :: a
  , modified :: Boolean
  , serverErrors :: Array String
  }
derive instance newtypeValidated :: Newtype (Validated a) _

type Validator a b = a -> Either String b

nonNull :: forall a. String -> Validator (Maybe a) a
nonNull name = maybe (Left $ name <> " is required.") Right

mustEqual :: forall a. Eq a => String -> a -> Validator a a
mustEqual error value1 value2 =
  if value1 == value2
    then Right value1
    else Left error

errors :: forall a. Either String a -> Array String
errors = either Array.singleton (const [])

validatedField
  :: forall i r action slots m a b
   . Validator a b
  -> Form (Validated i) r action slots m a
  -> Form (Validated i) r action slots m b
validatedField validator (Form f) =
  Form \v env counter ->
    let
      { ui, result, idCounter } = f v env counter
      validate r =
        let err = validator r
            touch = Newtype.over Validated (_ { modified = true, serverErrors = [] })
            es =
              if (Newtype.unwrap v).modified
                then displayErrors (errors err)
                else []
        in
          { ui: \onChange ->
                  let UI html = ui (onChange <<< touch)
                  in UI $ html <> es
          , result: hush err
          , idCounter
          }
    in result # maybe
        { ui
        , result: Nothing
        , idCounter
        }
        validate

displayErrors :: forall w i. Array String -> Array (HH.HTML w i)
displayErrors es =
  es <#> \e ->
    HH.div
      [ HP.class_ $ HH.ClassName "error" ]
      [ HH.text e ]

textBox
  :: forall action env slots m
   . Form (Validated String) env action slots m (Maybe String)
textBox = input HP.InputText

passwordBox
  :: forall action env slots m
   . Form (Validated String) env action slots m (Maybe String)
passwordBox = input HP.InputPassword

input
  :: forall action env slots m
   . HP.InputType
  -> Form (Validated String) env action slots m (Maybe String)
input inputType = Form \(Validated v) env idCounter ->
  { ui: \onChange -> UI $
                      [ HH.input
                        [ HP.type_ inputType
                          , HE.onValueInput \s ->
                              onChange <<< Validated $ v
                                { value = s
                                , modified = true
                                , serverErrors = []
                                }
                          , HP.value v.value
                          ]
                      ] <> displayErrors v.serverErrors
  , result: Just $
      if not (Array.null v.serverErrors) || String.null (String.trim v.value)
        then Nothing
        else Just <<< String.trim $ v.value
  , idCounter
  }

wrapped
  :: forall i env action slots m a
   . Form i env action slots m a
  -> (UI action slots m -> UI action slots m)
  -> Form i env action slots m a
wrapped (Form form) f =
  Form \i env counter ->
    let
      { ui, result, idCounter } = form i env counter
    in
      { ui: \onChange -> f (ui onChange)
      , result
      , idCounter
      }

labeled
  :: forall i env action slots m a
   . String
  -> Form i env action slots m a
  -> Form i env action slots m a
labeled l form =
  wrapped
    form
    (\(UI html) -> UI
      [ HH.label_ $ HH.text l : html ])

focus
  :: forall i j env action slots m a
   . Lens' i j
  -> Form j env action slots m a
  -> Form i env action slots m a
focus l (Form f) =
  Form \i env counter ->
    let
      { ui, result, idCounter } = f (view l i) env counter
    in
      { ui: \onChange -> ui (onChange <<< flip (set l) i)
      , result
      , idCounter
      }

data FormAction s
  = SetState s

type UIState action slots s m =
  { ui :: UI action slots m
  , state :: s
  }

type UISpec state r action slots s m a =
  { _uiState :: ALens' state (UIState action slots s m)
  , _action :: APrism' action (FormAction s)
  , form :: Form s r action slots m a
  }

runUI
  :: forall state r action slots s o m a
   . UISpec state r action slots s m a
  -> Env r
  -> Int
  -> action
  -> H.HalogenM state action slots o m (Maybe a)
runUI spec env counter action = do
  let actionPrism = clonePrism spec._action
      stateLens = cloneLens spec._uiState
  action ^? actionPrism # maybe
    (pure Nothing)
    case _ of
      SetState v -> do
        let Form g = spec.form
            next = g v env counter
            ui = next.ui (\s -> review (_Just <<< actionPrism) (SetState s))
        stateLens %= (_ { ui = ui, state = v })
        pure next.result
