module Trappisto.Model exposing (..)

import Navigation
import Window
import Keyboard
import Time exposing (Time)
import Http exposing (Error)
import Trappisto.Config exposing (..)
import Components.Address as AddressComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent


type alias Flags =
    { coin : String
    }


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
    , vimMode : Bool
    , debug : Bool
    , now : Time
    , fetching : Bool
    , webSocketConnected : Bool
    , lastWebSocketPong : Time
    , lastBlockHash : String
    , lastBlockHeight : Int
    , lastTransactions : List BasicTransaction
    }


initialModel : Config -> String -> Model
initialModel config query =
    { config = config
    , keys = Keys False False False False False False
    , window = Window.Size 0 0
    , addressModel = AddressComponent.initialModel config
    , blockModel = BlockComponent.initialModel config
    , transactionModel = TransactionComponent.initialModel config
    , query = query
    , template = Home
    , error = Nothing
    , vimMode = False
    , debug = False
    , fetching = False
    , webSocketConnected = False
    , now = -1
    , lastWebSocketPong = -1
    , lastBlockHash = ""
    , lastBlockHeight = -1
    , lastTransactions = []
    }


type Msg
    = NewUrl Navigation.Location
    | AddressMsg AddressComponent.Msg
    | BlockMsg BlockComponent.Msg
    | TransactionMsg TransactionComponent.Msg
    | JsMsg (List String)
    | WebSocketMsg String
    | Query String
    | KeyChange Bool Keyboard.KeyCode
    | Resize Window.Size
    | Tick Time
    | GetBestBlockx
    | GetBestBlockResult (Result Http.Error BestBlock)


type alias BestBlock =
    { hash : String
    , height : Int
    }


type alias BasicTransaction =
    { hash : String
    , amount : Float
    }


type alias Keys =
    { enter : Bool
    , esc : Bool
    , d : Bool
    , i : Bool
    , j : Bool
    , k : Bool
    }
