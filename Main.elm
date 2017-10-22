module Main exposing (main)

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Mouse
import Task exposing (Task)
import Time exposing (Time)
import Window


type alias Blockchain =
    { test : String }


type alias Model =
    { keys : Keys
    , mouse : Mouse.Position
    , window : Window.Size
    , blockchain : Blockchain
    }


type Msg
    = KeyChange Bool Keyboard.KeyCode
    | Animate Time
    | Resize Window.Size
    | MouseMove Mouse.Position


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , down : Bool
    , space : Bool
    , pgup : Bool
    , pgdown : Bool
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    ( { blockchain = Blockchain 0
      , keys = Keys False False False False False False False
      , mouse = Mouse.Position 0 0
      , window = Window.Size 0 0
      }
    , Cmd.batch
        [ Task.perform Resize Window.size
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Mouse.moves MouseMove
        , Window.resizes Resize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        KeyChange bool code ->
            ( { model | keys = keyHandler bool code model.keys }, Cmd.none )

        MouseMove position ->
            ( { model | mouse = position }, Cmd.none )

        Resize size ->
            ( { model | window = size }, Cmd.none )

        Animate dt ->
            ( { model
                | blockchain =
                    model.blockchain
                        |> move model.keys
              }
            , Cmd.none
            )


keyHandler : Bool -> Keyboard.KeyCode -> Keys -> Keys
keyHandler bool keyCode keys =
    case keyCode of
        32 ->
            { keys | space = bool }

        37 ->
            { keys | left = bool }

        39 ->
            { keys | right = bool }

        38 ->
            { keys | up = bool }

        40 ->
            { keys | down = bool }

        33 ->
            { keys | pgup = bool }

        34 ->
            { keys | pgdown = bool }

        _ ->
            keys


move : Keys -> Blockchain -> Blockchain
move { left, right, up, down, space, pgup, pgdown } blockchain =
    let
        direction a b =
            if a == b then
                0
            else if a then
                5
            else
                -5

        velocity =
            direction left right
    in
        { blockchain | test = velocity }


view : Model -> Html Msg
view model =
    let
        window =
            model.window
    in
        div
            [ style
                [ ( "width", toString window.width ++ "px" )
                , ( "height", toString window.height ++ "px" )
                , ( "background", "#000" )
                ]
            ]
            [ div
                [ style
                    [ ( "position", "absolute" )
                    , ( "font-family", "monospace" )
                    , ( "color", "white" )
                    , ( "text-align", "center" )
                    , ( "left", "1rem" )
                    , ( "right", "1rem" )
                    , ( "top", "1rem" )
                    ]
                ]
                [ text <| toString model ]
            ]
