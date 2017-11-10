module Trappisto.Model exposing (..)

import Navigation
import Window
import Keyboard
import Time exposing (Time)
import Components.Status as StatusComponent
import Components.Address as AddressComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent


type alias Config =
    { coin : String }


type Template
    = Status
    | Address
    | Block
    | Transaction


type alias Model =
    { config : Config
    , keys : Keys
    , window : Window.Size
    , statusModel : StatusComponent.Model
    , addressModel : AddressComponent.Model
    , blockModel : BlockComponent.Model
    , transactionModel : TransactionComponent.Model
    , query : String
    , template : Template
    , error : Maybe String
    , vimMode : Bool
    , debug : Bool
    , time : Time
    }


initialModel : Model
initialModel =
    { config = Config "BTC"
    , keys = Keys False False False False False False
    , window = Window.Size 0 0
    , statusModel = StatusComponent.initialModel
    , addressModel = AddressComponent.initialModel
    , blockModel = BlockComponent.initialModel
    , transactionModel = TransactionComponent.initialModel
    , query = ""
    , template = Status
    , error = Nothing
    , vimMode = False
    , debug = False
    , time = 0
    }


type Msg
    = NewUrl Navigation.Location
    | StatusMsg StatusComponent.Msg
    | AddressMsg AddressComponent.Msg
    | BlockMsg BlockComponent.Msg
    | TransactionMsg TransactionComponent.Msg
    | JsMsg String
    | Query String
    | QueryForce String
    | KeyChange Bool Keyboard.KeyCode
    | Resize Window.Size
    | Tick Time
    | FetchStatus


type alias Keys =
    { enter : Bool
    , esc : Bool
    , d : Bool
    , i : Bool
    , j : Bool
    , k : Bool
    }
