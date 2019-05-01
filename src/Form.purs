module Form
 ( Env(..)
  , Field(..)
  , Form(..)
  , FormAction(..)
  , UI(..)
  , UIConfig
  , UISpec
  , Wizard
  , array
  , content
  , decorated
  , empty
  , focus
  , input
  , labeled
  , mustEqual
  , nonNull
  , passwordBox
  , radioList
  , required
  , runForm
  , runUI
  , step
  , textBox
  , updateUI
  , validatedField
  , wizard
  , wrapped
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..), either, hush)
import Data.Lens (ALens', APrism', Lens', Traversal', _Just, cloneLens, clonePrism, preview, review, set, view, (.=), (^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Newtype (over, unwrap) as Newtype
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Halogen (HalogenM) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

-- See
-- https://medium.com/fuzzy-sharp/building-a-type-safe-embedded-dsl-for-form-components-with-validation-e7ffaaf537e4
-- for details

type UIConfig =
  { required :: Boolean
  , hasErrors :: Boolean
  }

type UI action slots m = UIConfig -> Array (HH.ComponentHTML action slots m)

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

newtype Field a = Field
  { value :: a
  , touched :: Boolean
  , externalErrors :: Array String
  }
derive instance newtypeField :: Newtype (Field a) _

type Validator a b = a -> Either String b

nonNull :: forall a. String -> Validator (Maybe a) a
nonNull name = maybe (Left $ name <> " is required.") Right

mustEqual :: forall a. Eq a => String -> a -> Validator a a
mustEqual error value1 value2 =
  if value1 == value2
    then Right value1
    else Left error

empty :: forall a. a -> Field a
empty value = Field
  { value
  , touched: false
  , externalErrors: []
  }

validatedField
  :: forall i r action slots m a b
   . Validator a b
  -> Form (Field i) r action slots m a
  -> Form (Field i) r action slots m b
validatedField validator (Form f) =
  Form \v env counter ->
    let
      { ui, result, idCounter } = f v env counter
      validate r =
        let err = validator r
            touch = Newtype.over Field (_ { touched = true, externalErrors = [] })
            errors = either Array.singleton (const [])
            es =
              if (Newtype.unwrap v).touched
                then displayErrors (errors err)
                else []
        in
          { ui: \onChange uiConfig ->
                  let nextUiConfig = uiConfig
                        { hasErrors = uiConfig.hasErrors || (not (Array.null es))
                        }
                      html = ui (onChange <<< touch) nextUiConfig
                  in html <> es
          , result: hush err
          , idCounter
          }
    in result # maybe
        { ui
        , result: Nothing
        , idCounter
        }
        validate

required
  :: forall i r action slots m a
   . String
  -> Form (Field i) r action slots m (Maybe a)
  -> Form (Field i) r action slots m a
required label optional = validatedField (nonNull label) (markRequired optional)
  where
    markRequired (Form f) =
      Form \v env counter ->
        let
          { ui, result, idCounter } = f v env counter
        in
          { ui: \onChange uiConfig -> ui onChange (uiConfig { required = true })
          , result
          , idCounter
          }

displayErrors :: forall w i. Array String -> Array (HH.HTML w i)
displayErrors es =
  es <#> \e ->
    HH.div
      [ HP.class_ $ HH.ClassName "error" ]
      [ HH.text e ]

radioList
  :: forall action env slots m a
   . Eq a
  => String
  -> Array (Tuple a String)
  -> Form (Field (Maybe a)) env action slots m (Maybe a)
radioList listLabel options = Form \(Field v) env idCounter ->
  { ui: \onChange uiConfig ->
      [ HH.text listLabel ]
        <> (flip Array.mapWithIndex options \i (Tuple value label) ->
              HH.label_
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name $ env.formName <> "_" <> show idCounter
                    , HP.checked $ v.value == Just value
                    , HE.onChecked \_ -> onChange <<< Field $ v
                                          { value = Just value
                                          , touched = true
                                          , externalErrors = []
                                          }
                    ]
                , HH.text label
                ])
        <> displayErrors v.externalErrors
  , result: Just v.value
  , idCounter: idCounter + 1
  }

textBox
  :: forall action env slots m
   . Form (Field String) env action slots m (Maybe String)
textBox = input HP.InputText

passwordBox
  :: forall action env slots m
   . Form (Field String) env action slots m (Maybe String)
passwordBox = input HP.InputPassword

input
  :: forall action env slots m
   . HP.InputType
  -> Form (Field String) env action slots m (Maybe String)
input inputType = Form \(Field v) env idCounter ->
  { ui: \onChange uiConfig ->
          [ HH.input
              [ HP.type_ inputType
              , HE.onValueInput \s ->
                  onChange <<< Field $ v
                    { value = s
                    , touched = true
                    , externalErrors = []
                    }
              , HE.onBlur \_ ->
                  onChange <<< Field $ v
                    { touched = true
                    }
              , HP.value v.value
              , ARIA.required (show uiConfig.required)
              , ARIA.invalid (show $ uiConfig.hasErrors
                                      || not (Array.null v.externalErrors))
              , HP.placeholder $
                  if uiConfig.required
                    then "Required"
                    else ""
              ]
          ] <> displayErrors v.externalErrors
  , result: Just $
      if not (Array.null v.externalErrors) || String.null (String.trim v.value)
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

decorated
  :: forall i env action slots m a
   . Form i env action slots m a
  -> (Array (HH.ComponentHTML action slots m) -> Array (HH.ComponentHTML action slots m))
  -> Form i env action slots m a
decorated form f =
  wrapped form \inner uiConfig -> f (inner uiConfig)

content
  :: forall i env action slots m
   . Array (HH.ComponentHTML action slots m)
  -> Form i env action slots m Unit
content html =
  Form \i env idCounter ->
    { ui: \_ _ -> html
    , result: Just unit
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
    (\f config -> [ HH.label_ $ HH.text l : f config ])

focus
  :: forall i j env action slots m a
   . Lens' i j
  -> Form j env action slots m a
  -> Form i env action slots m a
focus l = focus_ l

focus_
  :: forall i j env action slots m a
   . Traversal' i j
  -> Form j env action slots m a
  -> Form i env action slots m a
focus_ l (Form f) =
  Form \i env counter ->
    let
      run ii =
        let
          { ui, result, idCounter } = f ii env counter
        in
          { ui: \onChange -> ui (onChange <<< flip (set l) i)
          , result
          , idCounter
          }
    in
      preview l i # maybe
        { ui: \_ _ -> []
        , result: Nothing
        , idCounter: counter
        }
        run

array
  :: forall i env action slots m a
   . Form i env action slots m a
  -> Form (Array i) env action slots m (Array a)
array form =
  Form \is env counter ->
    -- Return [] if passed [] as input
    if Array.null is
      then
        { ui: \_ _ -> []
        , result: Just []
        , idCounter: counter
        }
      else
        let Form inner =
              for (Array.range 0 (Array.length is - 1)) \idx ->
                focus_ (ix idx) form
        in inner is env counter

data FormAction s
  = SetState s

type UISpec state r action slots s m a =
  { _uiState :: ALens' state s
  , _action :: APrism' action (FormAction s)
  , form :: Form s r action slots m a
  }

updateUI
  :: forall state r action slots s o m a
   . UISpec state r action slots s m a
  -> action
  -> H.HalogenM state action slots o m Unit
updateUI spec action = do
  let actionPrism = clonePrism spec._action
      stateLens = cloneLens spec._uiState
  action ^? actionPrism # maybe
    (pure unit)
    case _ of
      SetState v ->
        stateLens .= v

runUI
  :: forall state r action slots s m a
   . UISpec state r action slots s m a
  -> Env r
  -> state
  -> Array (HH.ComponentHTML action slots m)
runUI spec env state =
  let n = next spec env state
      actionPrism = clonePrism spec._action
  in n.ui
      (\s -> review (_Just <<< actionPrism) (SetState s))
      { hasErrors: false, required: false }

runForm
  :: forall state r action slots s m a
   . UISpec state r action slots s m a
  -> Env r
  -> state
  -> Maybe a
runForm spec env state =
  let n = next spec env state
  in n.result

next
  :: forall state r action slots s m a
   . UISpec state r action slots s m a
  -> Env r
  -> state
  -> { ui :: (s -> Maybe action) -> UI action slots m
     , result :: Maybe a
     , idCounter :: Int
     }
next spec env state =
  let stateLens = cloneLens spec._uiState
      Form g = spec.form
  in g (view stateLens state) env 0
