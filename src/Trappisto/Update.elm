port module Trappisto.Update exposing (init, update, subscriptions)

import Navigation
import Keyboard
import Task exposing (Task)
import Time exposing (Time)
import Window
import WebSocket
import Components.Status as StatusComponent
import Components.Address as AddressComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent
import Lib.WebSocket
import Trappisto.Model exposing (..)
import Trappisto.Helpers as Coin exposing (Coin)


port elmToJs : List String -> Cmd msg


port jsToElm : (List String -> msg) -> Sub msg


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        query =
            extractQuery location

        wsEndpoint =
            extractWSEndpoint location

        coin =
            case flags.coin of
                "BCH" ->
                    Coin.BCH

                "BTC" ->
                    Coin.BTC

                "DCR" ->
                    Coin.DCR

                _ ->
                    Debug.crash <|
                        "Invalid coin: "
                            ++ flags.coin
                            ++ " (valid coins are BCH, BTC and DCR)"

        model =
            initialModel coin wsEndpoint query

        ( updatedModel, msg ) =
            if String.isEmpty query then
                update FetchStatus model
            else
                update (Query query) model

        notifyBlocks =
            Lib.WebSocket.send model.wsEndpoint "notifyblocks" []

        notifyNewTransactions =
            Lib.WebSocket.send model.wsEndpoint "notifynewtransactions" []
    in
        ( updatedModel
        , Cmd.batch
            [ elmToJs [ "focus" ]
            , msg
            , notifyBlocks
            , notifyNewTransactions
            , Task.perform Resize Window.size
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Window.resizes Resize
        , Time.every (Time.second * 60) Tick
        , WebSocket.listen model.wsEndpoint WSMsg
        , jsToElm JsMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Tick time ->
            -- let
            --     -- TODO: ideally we should be able to trigger all updates when
            --     -- we receive a websocket event and not use polling altogether
            --     -- ( updatedModel, cmd ) =
            --     --     update (Query model.query) model
            -- in
            --     ( { model | time = time }, notifyBlocks )
            ( model, Cmd.none )

        WSMsg message ->
            let
                _ =
                    Debug.log "WSMsg" message

                connected =
                    Lib.WebSocket.isSuccess message

                status =
                    model.statusModel

                updatedStatus =
                    { status | webSocketConnected = connected }
            in
                ( { model | statusModel = updatedStatus }, Cmd.none )

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
                --- FIXME: add BTC genesis?
                genesis =
                    "298e5cc3d985bfe7f81dc135f360abe089edd4396b86d2de66b0cef42b21d980"

                possibleAddress query =
                    String.length query >= 26 && String.length query <= 35

                -- XXX: Remove a few zeros in the future... 00000000
                possibleBlockHash query =
                    (String.length query == 64 && String.left 8 query == "00000000")
                        || (query == genesis)

                parseBlockHeight string =
                    String.toInt string |> Result.toMaybe |> Maybe.withDefault -1

                possibleBlockHeight query =
                    parseBlockHeight query /= -1

                possibleTransaction query =
                    (String.length query == 64 && String.left 8 query /= "00000000")
                        && (query /= genesis)

                updatedModel =
                    { model | query = query, error = Nothing }
            in
                if query == "" then
                    ( { updatedModel | template = Status }
                    , Cmd.none
                    )
                        |> updateUrl
                else if possibleTransaction query then
                    fetchTransaction query updatedModel
                else if possibleBlockHash query then
                    fetchBlockByHash query updatedModel
                else if possibleAddress query then
                    fetchAddress query updatedModel
                else if possibleBlockHeight query then
                    fetchBlockByHeight (parseBlockHeight query) updatedModel
                else
                    ( { updatedModel
                        | template = Status
                        , error = Just "Not sure what you're looking for :|"
                      }
                    , Cmd.none
                    )
                        |> updateUrl

        StatusMsg statusMsg ->
            let
                ( updatedModel, cmd ) =
                    StatusComponent.update statusMsg model.statusModel
            in
                ( { model
                    | template = Status
                    , statusModel = updatedModel
                  }
                , Cmd.map StatusMsg cmd
                )
                    |> updateUrl

        AddressMsg addressMsg ->
            let
                ( updatedModel, cmd ) =
                    AddressComponent.update addressMsg model.addressModel
            in
                ( { model
                    | template = Address
                    , addressModel = updatedModel
                  }
                , Cmd.map AddressMsg cmd
                )
                    |> updateUrl

        BlockMsg blockMsg ->
            let
                ( updatedModel, cmd ) =
                    BlockComponent.update blockMsg model.blockModel

                -- query =
                --     case blockMsg of
                --         BlockComponent.GetBlock foo ->
                --             updatedModel.hash
                --         BlockComponent.GetBlockHash foo ->
                --             toString updatedModel.height
                --         _ ->
                --             model.query
            in
                ( { model
                    | template = Block
                    , query = updatedModel.hash
                    , blockModel = updatedModel
                  }
                , Cmd.map BlockMsg cmd
                )
                    |> updateUrl

        TransactionMsg transactionMsg ->
            let
                ( updatedModel, cmd ) =
                    TransactionComponent.update transactionMsg model.transactionModel
            in
                ( { model
                    | template = Transaction
                    , query = updatedModel.hash
                    , transactionModel = updatedModel
                  }
                , Cmd.map TransactionMsg cmd
                )
                    |> updateUrl

        JsMsg params ->
            case params of
                [ "query", query ] ->
                    update (Query query) model

                _ ->
                    Debug.crash <| "WTF? " ++ toString params

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
                    , elmToJs [ "focus" ]
                    )
                else if updatedModel.query /= model.query then
                    update (Query updatedModel.query) updatedModel
                else if keys.enter then
                    update (Query updatedModel.query) updatedModel
                else
                    ( updatedModel, Cmd.none )

        Resize size ->
            ( { model | window = size }, Cmd.none )


fetchAddress : String -> Model -> ( Model, Cmd Msg )
fetchAddress address model =
    update (AddressMsg (AddressComponent.SearchRawTransactions address)) model


fetchBlockByHash : String -> Model -> ( Model, Cmd Msg )
fetchBlockByHash hash model =
    update (BlockMsg (BlockComponent.GetBlock hash)) model


fetchBlockByHeight : Int -> Model -> ( Model, Cmd Msg )
fetchBlockByHeight height model =
    update (BlockMsg (BlockComponent.GetBlockHash height)) model


fetchTransaction : String -> Model -> ( Model, Cmd Msg )
fetchTransaction hash model =
    update (TransactionMsg (TransactionComponent.GetRawTransaction hash)) model


decodeKeys : Bool -> Keyboard.KeyCode -> Keys -> Keys
decodeKeys bool keyCode keys =
    case keyCode of
        13 ->
            { keys | enter = bool }

        27 ->
            { keys | esc = bool }

        68 ->
            { keys | d = bool }

        73 ->
            { keys | i = bool }

        74 ->
            { keys | j = bool }

        75 ->
            { keys | k = bool }

        _ ->
            keys


handleKeys : Keys -> Model -> Model
handleKeys { esc, d, i, j, k } model =
    if d then
        { model | debug = not model.debug }
    else if esc && not model.vimMode then
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


extractWSEndpoint : Navigation.Location -> String
extractWSEndpoint location =
    String.concat
        [ "wss://"
        , location.host
        , location.pathname
        , "ws"
        ]


newUrl : Model -> Cmd Msg
newUrl model =
    Navigation.newUrl <| "/#" ++ model.query


updateUrl : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateUrl ( model, cmd ) =
    let
        commands =
            case model.query of
                "" ->
                    [ newUrl model ]

                _ ->
                    [ newUrl model
                    , elmToJs [ "title", (toString model.template) ++ " " ++ model.query ]
                    ]
    in
        ( model, Cmd.batch <| commands ++ [ cmd ] )
