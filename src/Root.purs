module Root where

import Prelude

import Data.Lens (Prism', prism', (%=))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect.Aff.Class (class MonadAff)
import Form as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties (href) as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent (toEvent) as MouseEvent

type AddressForm =
  { address :: F.Field String
  , city :: F.Field String
  , state :: F.Field String
  , zip :: F.Field String
  }

type DemoForm =
  { nameF :: F.Field String
  , nameM :: F.Field String
  , nameL :: F.Field String
  , password :: F.Field String
  , passwordConfirmation :: F.Field String
  , favoriteColor :: F.Field (Maybe String)
  , primaryAddress :: AddressForm
  , otherAddresses :: Array AddressForm
  }

type DemoResult =
  { nameF :: String
  , nameM :: Maybe String
  , nameL :: String
  , password :: String
  , favoriteColor :: Maybe String
  , primaryAddress :: Address
  , otherAddresses :: Array Address
  }

type Address =
  { address :: String
  , city :: String
  , state :: String
  , zip :: String
  }

defaultAddress :: AddressForm
defaultAddress =
  { address: F.empty ""
  , city: F.empty ""
  , state: F.empty ""
  , zip: F.empty ""
  }

emptyForm :: DemoForm
emptyForm =
  { nameF: F.empty ""
  , nameM: F.empty ""
  , nameL: F.empty ""
  , password: F.empty ""
  , passwordConfirmation: F.empty ""
  , favoriteColor: F.empty Nothing
  , primaryAddress: defaultAddress
  , otherAddresses: []
  }

type State = DemoForm

data Action
  = RunForm (F.FormAction DemoForm)
  | AddAddress MouseEvent

component
  :: forall q i o m
   . MonadAff m
  => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: forall i. i -> State
initialState = const emptyForm

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_ $ F.runUI uiSpec env state

type PasswordForm r =
  { password :: F.Field String
  , passwordConfirmation :: F.Field String
  | r
  }

passwordForm :: forall r m. F.Form (PasswordForm r) () Action () m String
passwordForm = F.wizard do
  password <- F.step
    $ F.focus (prop (SProxy :: _ "password"))
    $ F.labeled "Password"
    $ F.required "Password"
    $ F.passwordBox

  passwordConfirmation <- F.step
    $ F.focus (prop (SProxy :: _ "passwordConfirmation"))
    $ F.labeled "Confirm password"
    $ F.validatedField
        (F.nonNull "Confirm password"
          >=> F.mustEqual "Passwords must match." password)
    $ F.passwordBox

  pure password

addressForm :: forall m. F.Form AddressForm () Action () m Address
addressForm = ado
  address <-
    F.focus (prop (SProxy :: _ "address"))
      $ F.labeled "Address"
      $ F.required "Address"
      $ F.textBox
  city <-
    F.focus (prop (SProxy :: _ "city"))
      $ F.labeled "City"
      $ F.required "City"
      $ F.textBox
  state <-
    F.focus (prop (SProxy :: _ "state"))
      $ F.labeled "State"
      $ F.required "State"
      $ F.textBox
  zip <-
    F.focus (prop (SProxy :: _ "zip"))
      $ F.labeled "Zip"
      $ F.required "Zip"
      $ F.textBox
  in { address, city, state, zip }

form :: forall m. F.Form DemoForm () Action () m DemoResult
form = ado
  F.content
    [ HH.h4_
        [ HH.text "Name"
        ]
    ]
  nameF <-
    F.focus (prop (SProxy :: _ "nameF"))
      $ F.labeled "First name"
      $ F.required "First name"
      $ F.textBox
  nameM <-
    F.focus (prop (SProxy :: _ "nameM"))
      $ F.labeled "Middle name"
      $ F.textBox
  nameL <-
    F.focus (prop (SProxy :: _ "nameL"))
      $ F.labeled "Last name"
      $ F.required "Last name"
      $ F.textBox
  password <- passwordForm
  F.content
    [ HH.h4_
        [ HH.text "Colors"
        ]
    ]
  favoriteColor <-
    F.focus (prop (SProxy :: _ "favoriteColor"))
      $ F.radioList
          "Favorite color"
          [ Tuple "red" "Red"
          , Tuple "blue" "Blue"
          , Tuple "green" "Green"
          ]
  F.content
    [ HH.h4_
        [ HH.text "Primary address"
        ]
    ]
  primaryAddress <-
    F.focus (prop (SProxy :: _ "primaryAddress")) addressForm
  F.content
    [ HH.h4_
        [ HH.text "Other addresses"
        ]
    ]
  otherAddresses <-
    F.focus (prop (SProxy :: _ "otherAddresses")) (F.array addressForm)
  F.content
    [ HH.a
        [ HE.onClick $ Just <<< AddAddress
        , HP.href "/"
        ]
        [ HH.text "+ Add Address"
        ]
    ]
  in
    { nameF
    , nameM
    , nameL
    , password
    , favoriteColor
    , primaryAddress
    , otherAddresses
    }

uiSpec :: forall m. F.UISpec State () Action () DemoForm m DemoResult
uiSpec =
  { _uiState: identity
  , _action
  , form
  }

_action :: Prism' Action (F.FormAction DemoForm)
_action = prism' RunForm case _ of
  RunForm x -> Just x
  _ -> Nothing

env :: F.Env ()
env =
  { formName: "demo"
  }

handleAction
  ∷ forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  AddAddress e -> do
    H.liftEffect $ preventDefault (MouseEvent.toEvent e)
    let _otherAddresses = prop (SProxy :: _ "otherAddresses")
    _otherAddresses %= (_ <> [ defaultAddress ])
  action -> do
    F.updateUI uiSpec action
    s <- H.get
    let result = F.runForm uiSpec env s
    traceM "Got a"
    traceM result
    pure unit
