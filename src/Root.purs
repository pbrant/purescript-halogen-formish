module Root where

import Prelude

import Data.Lens (Prism', prism')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Formlet as F
import Halogen as H
import Halogen.HTML as HH

type Plain a = a

type NameForm =
  { nameF :: F.Validated String
  , nameM :: F.Validated String
  , nameL :: F.Validated String
  , password :: F.Validated String
  , passwordConfirmation :: F.Validated String
  }

type Name =
  { nameF :: String
  , nameM :: Maybe String
  , nameL :: String
  , password :: String
  }

_nameF = SProxy :: SProxy "nameF"
_nameM = SProxy :: SProxy "nameM"
_nameL = SProxy :: SProxy "nameL"
_password = SProxy :: SProxy "password"
_passwordConfirmation = SProxy :: SProxy "passwordConfirmation"

emptyName :: NameForm
emptyName =
  { nameF: empty ""
  , nameM: empty ""
  , nameL: empty ""
  , password: empty ""
  , passwordConfirmation: empty ""
  }

empty :: forall a. a -> F.Validated a
empty value = F.Validated
  { value
  , modified: false
  , serverErrors: []
  }

type State m =
  { ui :: F.UI Action () m
  , state :: NameForm
  }

data Action
  = Init
  | RunForm (F.FormAction NameForm)

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

initialState :: forall i m. i -> State m
initialState _ =
  { ui: F.UI []
  , state: emptyName
  }

render :: forall m. State m -> H.ComponentHTML Action () m
render { ui: F.UI ui } =
  HH.div_ ui

type PasswordForm r =
  { password :: F.Validated String
  , passwordConfirmation :: F.Validated String
  | r
  }

passwordForm :: forall r m. F.Form (PasswordForm r) () Action () m String
passwordForm = F.wizard do
  password <- F.step
    $ F.focus (prop _password)
    $ F.labeled "Password"
    $ F.validatedField (F.nonNull "Password")
    $ F.passwordBox

  passwordConfirmation <- F.step
    $ F.focus (prop _passwordConfirmation)
    $ F.labeled "Confirm password"
    $ F.validatedField
        (F.nonNull "Confirm password"
          >=> F.mustEqual "Passwords must match." password)
    $ F.passwordBox

  pure password

form :: forall m. F.Form NameForm () Action () m Name
form = ado
  nameF <-
    F.focus (prop _nameF)
      $ F.labeled "First name"
      $ F.validatedField (F.nonNull "First name")
      $ F.textBox
  nameM <-
    F.focus (prop _nameM)
      $ F.labeled "Middle name"
      $ F.textBox
  nameL <-
    F.focus (prop _nameL)
      $ F.labeled "Last name"
      $ F.validatedField (F.nonNull "Last name")
      $ F.textBox
  password <- passwordForm
  in { nameF, nameM, nameL, password }

uiSpec :: forall m. F.UISpec (State m) () Action () NameForm m Name
uiSpec =
  { _uiState: identity
  , _action
  , form
  }

_action :: Prism' Action (F.FormAction NameForm)
_action = prism' RunForm case _ of
  RunForm x -> Just x
  _ -> Nothing

env :: F.Env ()
env =
  { formName: "demo"
  }

handleAction ∷ forall o m. Action -> H.HalogenM (State m) Action () o m Unit
handleAction = case _ of
  Init -> void $ F.runUI uiSpec env 0 (RunForm $ F.SetState emptyName)
  action -> do
    result <- F.runUI uiSpec env 0 action
    traceM "Got a"
    traceM result
    pure unit
