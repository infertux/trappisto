port module Main exposing (main)

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Mouse
import Task exposing (Task)
import Date exposing (Date)
import Time exposing (Time)
import Window
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Status
import Block


type alias Model =
    { keys : Keys
    , mouse : Mouse.Position
    , window : Window.Size
    , statusModel : Status.Model
    , blockModel : Block.Model
    , query : String
    , time : Time
    }


initialModel : Model
initialModel =
    { keys = Keys False False False False False False False
    , mouse = Mouse.Position 0 0
    , window = Window.Size 0 0
    , statusModel = Status.initialModel
    , blockModel = Block.initialModel
    , query = ""
    , time = 0
    }


type Msg
    = StatusMsg Status.Msg
    | BlockMsg Block.Msg
    | JsMsg String
    | Query String
    | KeyChange Bool Keyboard.KeyCode
    | Animate Time
    | Resize Window.Size
    | MouseMove Mouse.Position
    | Tick Time
    | Foo


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
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        ( model, msg ) =
            update Foo initialModel
    in
        ( model, Cmd.batch [ msg, Task.perform Resize Window.size ] )



-- ( initialModel
--   -- , Cmd.batch
--   --     [ Task.perform Resize Window.size
--   --     , Task.perform Foo Cmd.none
--   --     ]
-- )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Mouse.moves MouseMove
        , Window.resizes Resize
        , Time.every (Time.second * 5) Tick
        , jsEvents JsMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Tick time ->
            let
                ( updatedModel, cmd ) =
                    update Foo model
            in
                ( { updatedModel | time = time }, cmd )

        Foo ->
            let
                ( updatedModel, cmd ) =
                    Status.update Status.Fetch model.statusModel
            in
                ( { model | statusModel = updatedModel }, Cmd.map StatusMsg cmd )

        StatusMsg statusMsg ->
            let
                ( updatedModel, cmd ) =
                    Status.update statusMsg model.statusModel
            in
                ( { model | statusModel = updatedModel }, Cmd.map StatusMsg cmd )

        BlockMsg blockMsg ->
            let
                ( updatedModel, cmd ) =
                    Block.update blockMsg model.blockModel
            in
                ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )

        Query query ->
            ( model, Cmd.none )

        JsMsg _ ->
            ( model, Cmd.none )

        KeyChange bool code ->
            ( { model | keys = keyHandler bool code model.keys }, Cmd.none )

        MouseMove position ->
            ( { model | mouse = position }, Cmd.none )

        Resize size ->
            ( { model | window = size }, Cmd.none )

        Animate dt ->
            ( model |> move model.keys, Cmd.none )


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


move : Keys -> Model -> Model
move { left, right, up, down, space, pgup, pgdown } model =
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
        model


searchBar : Model -> Html Msg
searchBar model =
    div [ class "col" ]
        [ input
            [ name "query"
            , placeholder "Block, transaction, etc."
            , class "search form-control"
            , onInput Query
            , value model.query
            ]
            []
        ]


statusView : Model -> Html Msg
statusView model =
    Html.map StatusMsg (Status.view model.statusModel)


view : Model -> Html Msg
view model =
    let
        window =
            model.window

        header =
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ pre []
                        [ text " ______   _______  _______  _______  _______  ______  \n(  __  \\ (  ____ \\(  ____ \\(  ____ )(  ____ \\(  __  \\ \n| (  \\  )| (    \\/| (    \\/| (    )|| (    \\/| (  \\  )\n| |   ) || (__    | |      | (____)|| (__    | |   ) |\n| |   | ||  __)   | |      |     __)|  __)   | |   | |\n| |   ) || (      | |      | (\\ (   | (      | |   ) |\n| (__/  )| (____/\\| (____/\\| ) \\ \\__| (____/\\| (__/  )\n(______/ (_______/(_______/|/   \\__/(_______/(______/ \n"
                        ]
                    ]
                ]
            ]

        content =
            [ div [ class "row" ]
                [ searchBar model
                ]
            , div [ class "row" ]
                [ div [ class "col" ] [ text <| toString <| Date.fromTime model.time ]
                , statusView model
                ]
            ]

        debug =
            [ div
                [ style
                    [ ( "font-size", "1rem" )
                    , ( "color", "white" )
                    , ( "text-align", "center" )
                    ]
                ]
                [ text <| toString model ]
            ]
    in
        div [ class "row" ]
            [ div [ class "col text-center" ]
                [ div [ class "row" ] [ div [ class "col" ] header ]
                , div [ class "row" ] [ div [ class "col" ] content ]
                , hr [] []
                , div [ class "row" ] [ div [ class "col" ] debug ]
                ]
            ]



-- port for listening for events from JavaScript


port jsEvents : (String -> msg) -> Sub msg
