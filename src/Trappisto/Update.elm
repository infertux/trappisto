port module Trappisto.Update exposing (init, update, subscriptions)

import Navigation
import Keyboard
import Task exposing (Task)
import Time exposing (Time)
import Window
import Lib.JsonRpc as JsonRpc
import Lib.WebSocket as WebSocket
import Components.Address as AddressComponent
import Components.Block as BlockComponent
import Components.Transaction as TransactionComponent
import Trappisto.Config as Config
import Trappisto.Model exposing (..)
import Trappisto.Decoder as Decoder


port elmToJs : List String -> Cmd msg


port jsToElm : (List String -> msg) -> Sub msg


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        config =
            { coin = extractCoin flags
            , rpcEndpoint = extractRPCEndpoint location
            , wsEndpoint = extractWSEndpoint location
            }

        query =
            extractQuery location

        model =
            initialModel config query

        ( updatedModel, msg ) =
            if String.isEmpty query then
                ( model, Cmd.none )
            else
                update (Query query) model

        notifyBlocks =
            WebSocket.send model.config.wsEndpoint "notifyblocks" []

        notifyNewTransactions =
            WebSocket.send model.config.wsEndpoint "notifynewtransactions" []
    in
        ( updatedModel
        , Cmd.batch
            [ elmToJs [ "focus" ]
            , msg
            , getBestBlock updatedModel
            , notifyBlocks
            , notifyNewTransactions
            , Task.perform Tick Time.now
            , Task.perform Resize Window.size
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (KeyChange True)
        , Keyboard.ups (KeyChange False)
        , Window.resizes Resize
        , Time.every webSocketTTL Tick
        , WebSocket.listen model.config.wsEndpoint WebSocketMsg
        , jsToElm JsMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Tick now ->
            let
                ping =
                    WebSocket.send model.config.wsEndpoint "session" []

                timeout =
                    model.now - model.lastWebSocketPong >= webSocketTTL

                updatedModel =
                    { model
                        | now = now
                        , webSocketConnected = not timeout
                    }
            in
                ( updatedModel, ping )

        WebSocketMsg message ->
            let
                -- _ =
                --     Debug.log "WebSocketMsg" message
                connected =
                    WebSocket.isSuccess message

                txAccepted =
                    if WebSocket.isMethod [ "txaccepted" ] message then
                        Decoder.decodeTxAccepted message
                    else
                        Nothing

                newLastTransactions =
                    case txAccepted of
                        Nothing ->
                            model.lastTransactions

                        Just tx ->
                            List.take 15 (tx :: model.lastTransactions)

                updatedModel =
                    { model
                        | webSocketConnected = connected
                        , lastWebSocketPong = model.now
                        , lastTransactions = newLastTransactions
                    }

                cmd =
                    if WebSocket.isMethod [ "blockconnected", "blockdisconnected" ] message then
                        Cmd.batch
                            [ getBestBlock updatedModel
                            , update (Query updatedModel.query) updatedModel
                                |> Tuple.second
                            ]
                    else
                        Cmd.none
            in
                ( updatedModel, cmd )

        NewUrl location ->
            ( { model | query = extractQuery location }, Cmd.none )

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

                easterEgg query =
                    query == "particles"

                updatedModel =
                    { model | query = query, error = Nothing }
            in
                if query == "" then
                    ( { updatedModel | template = Home }
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
                else if easterEgg query then
                    ( updatedModel, elmToJs [ "particles" ] )
                else
                    ( { updatedModel
                        | template = Home
                        , error = Just "Not sure what you're looking for :|"
                      }
                    , Cmd.none
                    )
                        |> updateUrl

        GetBestBlockResult result ->
            case result of
                Ok bestBlock ->
                    ( { model
                        | lastBlockHash = bestBlock.hash
                        , lastBlockHeight = bestBlock.height
                        , fetching = False
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | error = JsonRpc.parseError error
                        , fetching = False
                      }
                    , Cmd.none
                    )

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
                    Decoder.decodeKeys bool code model.keys

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


handleKeys : Keys -> Model -> Model
handleKeys { esc, d, i, j, k } model =
    let
        toggleVimMode model =
            if esc && not model.vimMode then
                { model | vimMode = True }
            else if i && model.vimMode then
                { model | vimMode = False }
            else
                model

        handleVimMode model =
            if not model.vimMode then
                model
            else
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

        handleDebug model =
            if d && model.vimMode then
                { model | debug = not model.debug }
            else
                model
    in
        model |> toggleVimMode |> handleVimMode |> handleDebug


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


extractRPCEndpoint : Navigation.Location -> String
extractRPCEndpoint location =
    String.concat
        [ "https://"
        , location.host
        , location.pathname
        , "rpc"
        ]


extractWSEndpoint : Navigation.Location -> String
extractWSEndpoint location =
    String.concat
        [ "wss://"
        , location.host
        , location.pathname
        , "ws"
        ]


extractCoin : Flags -> Config.Coin
extractCoin flags =
    case flags.coin of
        "BCH" ->
            Config.BCH

        "BTC" ->
            Config.BTC

        "DCR" ->
            Config.DCR

        _ ->
            Debug.crash <|
                "Invalid coin: "
                    ++ flags.coin
                    ++ " (valid coins are BCH, BTC and DCR)"


newUrl : Model -> Cmd Msg
newUrl model =
    Navigation.newUrl <| "/#" ++ model.query


updateUrl : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateUrl ( model, cmd ) =
    let
        commands =
            if model.template == Home then
                [ newUrl model, elmToJs [ "title", "" ] ]
            else if String.isEmpty model.query then
                [ newUrl model ]
            else
                [ newUrl model
                , elmToJs
                    [ "title"
                    , (toString model.template) ++ " " ++ model.query
                    ]
                ]
    in
        ( model, Cmd.batch <| commands ++ [ cmd ] )


getBestBlock : Model -> Cmd Msg
getBestBlock model =
    JsonRpc.sendWithoutParams model.config.rpcEndpoint
        "getbestblock"
        GetBestBlockResult
        Decoder.decodeGetBestBlock


webSocketTTL : Time
webSocketTTL =
    Time.second * 10
