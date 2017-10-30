port module Trappisto.Update exposing (init, update, subscriptions)

import AnimationFrame
import Keyboard
import Mouse
import Task exposing (Task)
import Time exposing (Time)
import Window
import Trappisto.Model exposing (..)
import Components.Status as StatusComponent
import Components.Block as BlockComponent


-- port for listening for events from JavaScript


port jsEvents : (String -> msg) -> Sub msg


init : ( Model, Cmd Msg )
init =
    let
        ( model, msg ) =
            update FetchStatus initialModel
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
        , Time.every (Time.second * 60) Tick
        , jsEvents JsMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Tick time ->
            let
                -- TODO: ideally we should be able to trigger all updates when
                -- we receive a websocket event and not use polling altogether
                ( updatedModel, cmd ) =
                    update (Query model.query) model
            in
                ( { updatedModel | time = time }, cmd )

        FetchStatus ->
            let
                ( updatedModel, cmd ) =
                    StatusComponent.update StatusComponent.Fetch model.statusModel
            in
                ( { model | statusModel = updatedModel }, Cmd.map StatusMsg cmd )

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
                    False

                statusModel =
                    model.statusModel

                updatedStatusModel =
                    { statusModel | error = Nothing }

                updatedModel =
                    { model | query = query, statusModel = updatedStatusModel }
            in
                if query == "" then
                    ( { updatedModel | template = Status }, Cmd.none )
                else if possibleTransaction query then
                    fetchTransaction query updatedModel
                else if possibleBlockHash query then
                    fetchBlockByHash query updatedModel
                else if possibleBlockHeight query then
                    fetchBlockByHeight (String.toInt query |> Result.toMaybe |> Maybe.withDefault -1) updatedModel
                else if possibleAddress query then
                    fetchAddress query updatedModel
                else
                    let
                        statusModel =
                            updatedModel.statusModel

                        updatedStatusModel =
                            { statusModel | error = Just "Not sure what you're looking for :|" }
                    in
                        ( { updatedModel | statusModel = updatedStatusModel }, Cmd.none )

        StatusMsg statusMsg ->
            let
                ( updatedModel, cmd ) =
                    StatusComponent.update statusMsg model.statusModel

                -- FIXME: handle JSON decoding failures
            in
                ( { model | template = Status, statusModel = updatedModel }, Cmd.map StatusMsg cmd )

        BlockMsg blockMsg ->
            let
                ( updatedModel, cmd ) =
                    BlockComponent.update blockMsg model.blockModel

                -- FIXME: handle JSON decoding failures
            in
                ( { model | template = Block, blockModel = updatedModel }, Cmd.map BlockMsg cmd )

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


fetchAddress : String -> Model -> ( Model, Cmd Msg )
fetchAddress address model =
    ( model, Cmd.none )


fetchBlockByHash : String -> Model -> ( Model, Cmd Msg )
fetchBlockByHash hash model =
    let
        ( updatedModel, cmd ) =
            BlockComponent.update (BlockComponent.FetchByHash hash) model.blockModel
    in
        ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )


fetchBlockByHeight : Int -> Model -> ( Model, Cmd Msg )
fetchBlockByHeight height model =
    let
        ( updatedModel, cmd ) =
            BlockComponent.update (BlockComponent.FetchByHeight height) model.blockModel
    in
        ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )


fetchTransaction : String -> Model -> ( Model, Cmd Msg )
fetchTransaction transaction model =
    ( model, Cmd.none )


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
