module Trappisto.Model exposing (..)

import Navigation
import Window
import Keyboard
import Time exposing (Time)
import Components.Status as StatusComponent
import Components.Address as AddressComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent
import Trappisto.Helpers exposing (Coin)


type alias Flags =
    { coin : String }


type alias Config =
    { coin : Coin }


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
    , wsEndpoint : String
    , vimMode : Bool
    , debug : Bool
    , time : Time
    }


initialModel : Coin -> String -> String -> Model
initialModel coin wsEndpoint query =
    { config = Config coin
    , keys = Keys False False False False False False
    , window = Window.Size 0 0
    , statusModel = StatusComponent.initialModel
    , addressModel = AddressComponent.initialModel coin
    , blockModel = BlockComponent.initialModel coin
    , transactionModel = TransactionComponent.initialModel coin
    , query = query
    , template = Status
    , error = Nothing
    , wsEndpoint = wsEndpoint
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
    | JsMsg (List String)
    | Query String
    | KeyChange Bool Keyboard.KeyCode
    | Resize Window.Size
    | Tick Time
    | WSMsg String
    | FetchStatus


type alias Keys =
    { enter : Bool
    , esc : Bool
    , d : Bool
    , i : Bool
    , j : Bool
    , k : Bool
    }
