port module Main exposing (main)

import AnimationFrame
import Html exposing (Html, text, div)
import Html.Attributes exposing (width, height, style)
import Keyboard
import Mouse
import Task exposing (Task)
import Time exposing (Time)
import Lib.TimeExtra as TimeExtra
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Mouse.moves MouseMove
        , Window.resizes Resize
        , Time.every (Time.second * 30) Tick
        , jsEvents JsMsg
        ]


fetchAddress : String -> Model -> ( Model, Cmd Msg )
fetchAddress address model =
    ( model, Cmd.none )


fetchBlockByHash : String -> Model -> ( Model, Cmd Msg )
fetchBlockByHash hash model =
    let
        ( updatedModel, cmd ) =
            Block.update (Block.FetchByHash hash) model.blockModel
    in
        ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )


fetchBlockByHeight : Int -> Model -> ( Model, Cmd Msg )
fetchBlockByHeight height model =
    let
        ( updatedModel, cmd ) =
            Block.update (Block.FetchByHeight height) model.blockModel
    in
        ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )


fetchTransaction : String -> Model -> ( Model, Cmd Msg )
fetchTransaction transaction model =
    ( model, Cmd.none )


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

                -- FIXME: handle JSON decoding failures
            in
                ( { model | statusModel = updatedModel }, Cmd.map StatusMsg cmd )

        BlockMsg blockMsg ->
            let
                ( updatedModel, cmd ) =
                    Block.update blockMsg model.blockModel

                -- FIXME: handle JSON decoding failures
            in
                ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )

        Query query ->
            let
                possibleAddress query =
                    String.length query /= 64 && String.left 2 query == "Ds"

                -- XXX: 00000000
                possibleBlockHash query =
                    String.length query == 64 && String.left 8 query == "00000000"

                possibleBlockHeight query =
                    case String.toInt query of
                        Ok int ->
                            True

                        Err _ ->
                            False

                possibleTransaction query =
                    String.length query == 64

                updatedModel =
                    { model | query = query }
            in
                if query == "" then
                    fetchAddress query updatedModel
                else if possibleTransaction query then
                    fetchTransaction query updatedModel
                else if possibleBlockHash query then
                    fetchBlockByHash query updatedModel
                else if possibleBlockHeight query then
                    fetchBlockByHeight (String.toInt query |> Result.toMaybe |> Maybe.withDefault -1) updatedModel
                else if possibleAddress query then
                    fetchAddress query updatedModel
                else
                    Debug.crash query

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

        -- 38 ->
        --     { keys | j = bool }
        -- 40 ->
        --     { keys | k = bool }
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


isFetching : Model -> Bool
isFetching model =
    model.statusModel.fetching || model.blockModel.fetching


searchBar : Model -> Html Msg
searchBar model =
    div [ class "col-6 offset-3" ]
        [ input
            [ id "query"
            , name "query"
            , placeholder "Search for blocks, transactions, addresses, particles, etc."
            , class "form-control form-control-lg text-center mt-2 mb-4"
            , onInput Query
            , value model.query
            ]
            []
        ]


notification : Model -> Html Msg
notification model =
    div [ class "row" ]
        [ div [ class "col" ]
            [ text (Maybe.withDefault "" model.blockModel.error) ]
        ]


statusView : Model -> Html Msg
statusView model =
    Html.map StatusMsg (Status.view model.statusModel)


blockView : Model -> Html Msg
blockView model =
    Html.map BlockMsg (Block.view model.blockModel)


view : Model -> Html Msg
view model =
    let
        window =
            model.window

        glow =
            if isFetching model then
                "glow"
            else
                ""

        header =
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ pre [ id "logo", class glow ]
                        [ text " ______   _______  _______  _______  _______  ______  \n(  __  \\ (  ____ \\(  ____ \\(  ____ )(  ____ \\(  __  \\ \n| (  \\  )| (    \\/| (    \\/| (    )|| (    \\/| (  \\  )\n| |   ) || (__    | |      | (____)|| (__    | |   ) |\n| |   | ||  __)   | |      |     __)|  __)   | |   | |\n| |   ) || (      | |      | (\\ (   | (      | |   ) |\n| (__/  )| (____/\\| (____/\\| ) \\ \\__| (____/\\| (__/  )\n(______/ (_______/(_______/|/   \\__/(_______/(______/ \n"
                        ]
                    ]
                ]
            ]

        status model =
            div [ class "row" ]
                [ div [ class "col" ] [ text <| TimeExtra.toISOString model.time ]
                , statusView model
                ]

        block model =
            div [ class "row" ]
                [ div
                    [ class "col-6 offset-3" ]
                    [ blockView model ]
                ]

        content =
            case model.query of
                "" ->
                    [ div [ class "row" ] [ searchBar model ]
                    , notification model
                    , status model
                    ]

                "particles" ->
                    [ div [ class "row" ]
                        [ div [ class "col" ] [ text "Reticulating splines..." ]
                        ]
                    ]

                _ ->
                    [ div [ class "row" ] [ searchBar model ]
                    , notification model
                    , block model
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
        div [ class "row text-white" ]
            [ div [ class "col" ]
                [ div [ class "row text-center" ] [ div [ class "col" ] header ]
                , div [ class "row" ] [ div [ class "col" ] content ]
                , hr [] []
                , div [ class "row" ] [ div [ class "col" ] debug ]
                ]
            ]



-- port for listening for events from JavaScript


port jsEvents : (String -> msg) -> Sub msg
