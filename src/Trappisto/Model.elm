module Trappisto.Model exposing (..)

import Mouse
import Window
import Keyboard
import Time exposing (Time)
import Components.Status as StatusComponent
import Components.Block as BlockComponent


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
    , query : String
    , template : Template
    , time : Time
    }


initialModel : Model
initialModel =
    { keys = Keys False False
    , mouse = Mouse.Position 0 0
    , window = Window.Size 0 0
    , statusModel = StatusComponent.initialModel
    , blockModel = BlockComponent.initialModel
    , query = ""
    , template = Status
    , time = 0
    }


type Msg
    = StatusMsg StatusComponent.Msg
    | BlockMsg BlockComponent.Msg
    | JsMsg String
    | Query String
    | KeyChange Bool Keyboard.KeyCode
    | Resize Window.Size
    | MouseMove Mouse.Position
    | Tick Time
    | FetchStatus


type alias Keys =
    { j : Bool
    , k : Bool
    }
