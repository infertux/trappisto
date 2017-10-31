module Trappisto.Model exposing (..)

import Navigation
import Mouse
import Window
import Keyboard
import Time exposing (Time)
import Components.Status as StatusComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent


type Template
    = Status
    | Address
    | Block
    | Transaction


type alias Model =
    { keys : Keys
    , mouse : Mouse.Position
    , window : Window.Size
    , statusModel : StatusComponent.Model
    , blockModel : BlockComponent.Model
    , transactionModel : TransactionComponent.Model
    , query : String
    , template : Template
    , vimMode : Bool
    , time : Time
    }


initialModel : Model
initialModel =
    { keys = Keys False False False False
    , mouse = Mouse.Position 0 0
    , window = Window.Size 0 0
    , statusModel = StatusComponent.initialModel
    , blockModel = BlockComponent.initialModel
    , transactionModel = TransactionComponent.initialModel
    , query = ""
    , template = Status
    , vimMode = False
    , time = 0
    }


type Msg
    = NewUrl Navigation.Location
    | StatusMsg StatusComponent.Msg
    | BlockMsg BlockComponent.Msg
    | TransactionMsg TransactionComponent.Msg
    | JsMsg String
    | Query String
    | KeyChange Bool Keyboard.KeyCode
    | Resize Window.Size
    | MouseMove Mouse.Position
    | Tick Time
    | FetchStatus


type alias Keys =
    { esc : Bool
    , i : Bool
    , j : Bool
    , k : Bool
    }
