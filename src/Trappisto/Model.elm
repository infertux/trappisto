module Trappisto.Model exposing (..)

import Navigation
import Window
import Keyboard
import Time exposing (Time)
import Http exposing (Error)
import Components.Address as AddressComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent
import Trappisto.Helpers exposing (Coin)


type alias Flags =
    { coin : String }


type alias Config =
    { coin : Coin }


type Template
    = Home
    | Address
    | Block
    | Transaction


type alias Model =
    { config : Config
    , keys : Keys
    , window : Window.Size
    , addressModel : AddressComponent.Model
    , blockModel : BlockComponent.Model
    , transactionModel : TransactionComponent.Model
    , query : String
    , template : Template
    , error : Maybe String
    , wsEndpoint : String
    , vimMode : Bool
    , debug : Bool
    , now : Time
    , lastWebSocketPong : Time
    , lastBlockHash : String
    , lastBlockHeight : Int
    , fetching : Bool
    , webSocketConnected : Bool
    }


initialModel : Coin -> String -> String -> Model
initialModel coin wsEndpoint query =
    { config = Config coin
    , keys = Keys False False False False False False
    , window = Window.Size 0 0
    , addressModel = AddressComponent.initialModel coin
    , blockModel = BlockComponent.initialModel coin
    , transactionModel = TransactionComponent.initialModel coin
    , query = query
    , template = Home
    , error = Nothing
    , wsEndpoint = wsEndpoint
    , vimMode = False
    , debug = False
    , now = -1
    , lastWebSocketPong = -1
    , lastBlockHash = ""
    , lastBlockHeight = -1
    , fetching = False
    , webSocketConnected = False
    }


type Msg
    = NewUrl Navigation.Location
    | AddressMsg AddressComponent.Msg
    | BlockMsg BlockComponent.Msg
    | TransactionMsg TransactionComponent.Msg
    | JsMsg (List String)
    | Query String
    | KeyChange Bool Keyboard.KeyCode
    | Resize Window.Size
    | Tick Time
    | WSMsg String
    | GetBestBlock
    | GetBestBlockResult (Result Http.Error BestBlock)


type alias BestBlock =
    { hash : String, height : Int }


type alias Keys =
    { enter : Bool
    , esc : Bool
    , d : Bool
    , i : Bool
    , j : Bool
    , k : Bool
    }
