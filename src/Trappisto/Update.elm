port module Trappisto.Update exposing (init, update, subscriptions)

import Navigation
import Keyboard
import Mouse
import Task exposing (Task)
import Time exposing (Time)
import Window
import Trappisto.Model exposing (..)
import Components.Status as StatusComponent
import Components.Block as BlockComponent


port elmToJs : String -> Cmd msg


port jsToElm : (String -> msg) -> Sub msg


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        query =
            extractQuery location

        ( model, msg ) =
            if String.isEmpty query then
                update FetchStatus { initialModel | query = query }
            else
                update (Query query) { initialModel | query = query }
    in
        ( model
        , Cmd.batch
            [ elmToJs "focus"
            , msg
            , Task.perform Resize Window.size
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Mouse.moves MouseMove
        , Window.resizes Resize
        , Time.every (Time.second * 60) Tick
        , jsToElm JsMsg
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

        NewUrl location ->
            ( { model | query = extractQuery location }, Cmd.none )

        FetchStatus ->
            let
                ( updatedModel, cmd ) =
                    StatusComponent.update StatusComponent.GetInfo model.statusModel
            in
                ( { model | statusModel = updatedModel }, Cmd.map StatusMsg cmd )

        Query query ->
            let
                genesis =
                    "298e5cc3d985bfe7f81dc135f360abe089edd4396b86d2de66b0cef42b21d980"

                possibleAddress query =
                    String.length query /= 64 && String.left 2 query == "Ds"

                -- XXX: Remove a few zeros in the future... 00000000
                possibleBlockHash query =
                    (String.length query == 64 && String.left 8 query == "00000000") || query == genesis

                parseBlockHeight string =
                    String.toInt string |> Result.toMaybe |> Maybe.withDefault -1

                possibleBlockHeight query =
                    parseBlockHeight query /= -1

                possibleTransaction query =
                    String.length query == 64 && String.left 8 query /= "00000000" && query /= genesis

                statusModel =
                    model.statusModel

                updatedStatusModel =
                    { statusModel | error = Nothing }

                updatedModel =
                    { model | query = query, statusModel = updatedStatusModel }

                updateUrl ( model, cmd ) =
                    ( model
                    , Cmd.batch [ newUrl model, cmd ]
                    )
            in
                if query == "" then
                    ( { updatedModel | template = Status }, Cmd.none ) |> updateUrl
                else if possibleTransaction query then
                    fetchTransaction query updatedModel |> updateUrl
                else if possibleBlockHash query then
                    fetchBlockByHash query updatedModel |> updateUrl
                else if possibleBlockHeight query then
                    fetchBlockByHeight (parseBlockHeight query) updatedModel |> updateUrl
                else if possibleAddress query then
                    fetchAddress query updatedModel |> updateUrl
                else
                    let
                        statusModel =
                            updatedModel.statusModel

                        updatedStatusModel =
                            { statusModel | error = Just "Not sure what you're looking for :|" }
                    in
                        ( { updatedModel | statusModel = updatedStatusModel }, Cmd.none ) |> updateUrl

        StatusMsg statusMsg ->
            let
                ( updatedModel, cmd ) =
                    StatusComponent.update statusMsg model.statusModel
            in
                ( { model | template = Status, statusModel = updatedModel }, Cmd.map StatusMsg cmd )

        BlockMsg blockMsg ->
            let
                ( updatedModel, cmd ) =
                    BlockComponent.update blockMsg model.blockModel
            in
                ( { model | template = Block, blockModel = updatedModel }, Cmd.map BlockMsg cmd )

        JsMsg _ ->
            ( model, Cmd.none )

        KeyChange bool code ->
            let
                keys =
                    decodeKeys bool code model.keys

                updatedModel =
                    { model | keys = keys } |> handleKeys keys

                focus =
                    model.vimMode && not updatedModel.vimMode
            in
                if focus then
                    ( { updatedModel
                        -- don't append "i" when switching back to normal mode
                        | query = String.dropRight 1 updatedModel.query
                      }
                    , elmToJs "focus"
                    )
                else
                    update (Query updatedModel.query) updatedModel

        MouseMove position ->
            ( { model | mouse = position }, Cmd.none )

        Resize size ->
            ( { model | window = size }, Cmd.none )


fetchAddress : String -> Model -> ( Model, Cmd Msg )
fetchAddress address model =
    ( model, Cmd.none )


fetchBlockByHash : String -> Model -> ( Model, Cmd Msg )
fetchBlockByHash hash model =
    let
        ( updatedModel, cmd ) =
            BlockComponent.update (BlockComponent.GetBlock hash) model.blockModel
    in
        ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )


fetchBlockByHeight : Int -> Model -> ( Model, Cmd Msg )
fetchBlockByHeight height model =
    let
        ( updatedModel, cmd ) =
            BlockComponent.update (BlockComponent.GetBlockHash height) model.blockModel
    in
        ( { model | blockModel = updatedModel }, Cmd.map BlockMsg cmd )


fetchTransaction : String -> Model -> ( Model, Cmd Msg )
fetchTransaction transaction model =
    fetchBlockByHash transaction model


decodeKeys : Bool -> Keyboard.KeyCode -> Keys -> Keys
decodeKeys bool keyCode keys =
    case keyCode of
        27 ->
            { keys | esc = bool }

        73 ->
            { keys | i = bool }

        74 ->
            { keys | j = bool }

        75 ->
            { keys | k = bool }

        _ ->
            keys


handleKeys : Keys -> Model -> Model
handleKeys { esc, i, j, k } model =
    if esc && not model.vimMode then
        { model | vimMode = True }
    else if i && model.vimMode then
        { model | vimMode = False }
    else if model.vimMode then
        case model.template of
            Block ->
                if j then
                    case model.blockModel.nextBlockHash of
                        Just hash ->
                            { model | query = hash }

                        Nothing ->
                            model
                else if k then
                    case model.blockModel.previousBlockHash of
                        Just hash ->
                            { model | query = hash }

                        Nothing ->
                            model
                else
                    model

            _ ->
                model
    else
        model


extractQuery : Navigation.Location -> String
extractQuery location =
    let
        pathname =
            String.dropLeft 1 location.pathname

        hash =
            String.dropLeft 1 location.hash
    in
        if String.isEmpty pathname then
            hash
        else
            pathname


newUrl : Model -> Cmd Msg
newUrl model =
    Navigation.newUrl <| "/#" ++ model.query
