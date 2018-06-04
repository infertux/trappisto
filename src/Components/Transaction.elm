module Components.Transaction exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time exposing (Time)
import Regex
import Lib.TimeExtra as TimeExtra
import Lib.JsonRpc as JsonRpc
import Trappisto.Config exposing (..)
import Trappisto.Helpers exposing (..)


type alias Model =
    { hash : String
    , type_ : String
    , size : Int
    , confirmations : Int
    , blockHash : Maybe String
    , blockHeight : Maybe Int
    , time : Maybe Time
    , vIn : List VIn
    , vOut : List VOut
    , fetching : Bool
    , error : Maybe String
    , config : Config
    }


initialModel : Config -> Model
initialModel config =
    { hash = ""
    , type_ = "?"
    , size = -1
    , confirmations = -1
    , blockHash = Nothing
    , blockHeight = Nothing
    , time = Nothing
    , vIn = []
    , vOut = []
    , fetching = False
    , error = Nothing
    , config = config
    }


modelFromJson : JsonModel -> Config -> Model
modelFromJson jsonModel config =
    { hash = jsonModel.hash
    , type_ = computeType jsonModel
    , size = computeSize jsonModel
    , confirmations = jsonModel.confirmations
    , blockHash = jsonModel.blockHash
    , blockHeight = jsonModel.blockHeight
    , time = Maybe.map TimeExtra.timestampToTime jsonModel.time
    , vIn = List.sortBy .amountIn jsonModel.vIn |> List.reverse
    , vOut = List.sortBy .value jsonModel.vOut |> List.reverse
    , fetching = False
    , error = Nothing
    , config = config
    }


type alias JsonModel =
    { hash : String
    , hex : String
    , confirmations : Int
    , blockHash : Maybe String
    , blockHeight : Maybe Int
    , time : Maybe Int
    , vIn : List VIn
    , vOut : List VOut
    }


type alias VIn =
    { txId : Maybe String
    , coinbase : Maybe String
    , amountIn : Float
    , blockHeight : Maybe Int
    , prevOut : Maybe PrevOut
    }


type alias PrevOut =
    { addresses : List String
    , value : Float
    }


type alias VOut =
    { value : Float
    , scriptPubKey : ScriptPubKey
    }


type alias ScriptPubKey =
    { asm : String
    , type_ : String
    , addresses : List String
    }


type Msg
    = GetRawTransaction String
    | GetRawTransactionResult (Result Http.Error JsonModel)


view : Model -> Time -> Html a
view model now =
    let
        formatType type_ =
            span [ class "badge badge badge-success" ] [ text type_ ]

        formatBlock hash height =
            case ( hash, height ) of
                ( Nothing, Nothing ) ->
                    span [] [ text "N/A (unconfirmed)" ]

                ( Just hash, Nothing ) ->
                    queryLink hash (shortHash hash) []

                ( Nothing, Just height ) ->
                    queryLink (formatNumber height) (toString height) []

                ( Just hash, Just height ) ->
                    queryLink hash (formatNumber height) []

        formatFees model =
            case model.config.coin of
                DCR ->
                    [ ( "total sent", Just <| formatAmount (totalSent model) )
                    , ( "fee"
                      , (if model.confirmations == 0 then
                            --- XXX: actual fee is not returned by RPC for
                            --- unconfirmed transactions
                            Nothing
                         else
                            Just <|
                                span []
                                    (if fee model == 0 then
                                        [ formatAmount (fee model) ]
                                     else
                                        [ formatAmount (fee model)
                                        , span [] [ text " (" ]
                                        , formatAmount (feePerKb model)
                                        , span [] [ text "/kB)" ]
                                        ]
                                    )
                        )
                      )
                    ]

                _ ->
                    []

        formatAddresses maybe =
            case maybe of
                Nothing ->
                    span [] []

                Just wrapper ->
                    div [] <|
                        (List.map
                            (\address ->
                                div []
                                    [ queryLink address
                                        (shortAddress address)
                                        [ class "d-inline d-xs-none" ]
                                    , queryLink address
                                        address
                                        [ class "d-none d-xs-inline" ]
                                    ]
                            )
                            wrapper.addresses
                        )

        formatVIn vIn =
            vIn
                |> List.map
                    (\vIn ->
                        li [ class "list-group-item bg-secondary" ]
                            [ span [ class "badge badge-info mr-2" ]
                                [ text
                                    (if vIn.coinbase /= Nothing then
                                        "coinbase"
                                     else if vIn.txId /= Nothing then
                                        "outpoint"
                                     else
                                        "stakebase"
                                    )
                                ]
                            , span [ class "float-right" ]
                                [ if vIn.amountIn > 0 then
                                    formatAmount vIn.amountIn
                                  else
                                    span [] []
                                ]
                            , div [ class "clearfix" ] []
                            , div []
                                (case vIn.txId of
                                    Just hash ->
                                        [ queryLink hash (shortHash hash) [] ]

                                    Nothing ->
                                        [ span [] [] ]
                                )
                            , formatAddresses vIn.prevOut
                            ]
                    )

        formatVOut vOut =
            vOut
                |> List.map
                    (\vOut ->
                        li [ class "list-group-item bg-secondary" ]
                            [ span [ class "badge badge-info mr-2" ] [ text vOut.scriptPubKey.type_ ]
                            , span [ class "float-right" ] [ formatAmount vOut.value ]
                            , div [ class "clearfix" ] []
                            , formatAddresses (Just vOut.scriptPubKey)
                            , code [ class "mt-2" ] [ text vOut.scriptPubKey.asm ]
                            ]
                    )
    in
        div [ class "row" ]
            [ div [ class "col" ]
                [ div
                    [ class "card bg-dark" ]
                    [ h5 [ class "card-header d-flex justify-content-between" ]
                        [ span [ class "d-inline d-lg-none align-self-center" ] [ text <| "Transaction " ++ (shortHash model.hash) ]
                        , span [ class "d-none d-lg-inline align-self-center" ] [ text <| "Transaction " ++ model.hash ]
                        , dcrDataLink <| "tx/" ++ model.hash
                        ]
                    , div [ class "card-body" ]
                        [ p [ class "card-text" ]
                            [ dlBuilder <|
                                List.concat
                                    [ [ ( "type"
                                        , Just <| formatType model.type_
                                        )
                                      , ( "confirmations"
                                        , Just <| span [] [ text <| toString model.confirmations ]
                                        )
                                      , ( "time"
                                        , Just <| span [] [ formatTime model now ]
                                        )
                                      , ( "block"
                                        , Just <| formatBlock model.blockHash model.blockHeight
                                        )
                                      , ( "size"
                                        , Just <| span [] [ text <| (formatNumber model.size) ++ " bytes" ]
                                        )
                                      ]
                                    , formatFees model
                                    ]
                            ]
                        , hr [] []
                        , div [ class "row" ]
                            [ div [ class "col mb-3" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ]
                                        [ text <| pluralize (List.length model.vIn) "input" ]
                                    ]
                                , ul [ class "list-group list-group-flush" ] <|
                                    formatVIn model.vIn
                                ]
                            , div [ class "col mb-3" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ]
                                        [ text <| pluralize (List.length model.vOut) "output" ]
                                    ]
                                , ul [ class "list-group list-group-flush" ] <|
                                    formatVOut model.vOut
                                ]
                            ]
                        ]
                    ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRawTransaction hash ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, getRawTransaction updatedModel hash )

        GetRawTransactionResult result ->
            case result of
                Ok jsonModel ->
                    ( modelFromJson jsonModel model.config, Cmd.none )

                Err error ->
                    ( { model
                        | error = JsonRpc.parseError error
                        , fetching = False
                      }
                    , Cmd.none
                    )


getRawTransaction : Model -> String -> Cmd Msg
getRawTransaction model hash =
    let
        params =
            Encode.list [ Encode.string hash, Encode.int 1 ]
    in
        JsonRpc.send
            model.config.rpcEndpoint
            "getrawtransaction"
            params
            GetRawTransactionResult
            (decodeGetRawTransaction True)


decodeGetRawTransaction : Bool -> Decode.Decoder JsonModel
decodeGetRawTransaction prefix =
    let
        path key =
            if prefix then
                [ "result", key ]
            else
                [ key ]
    in
        Pipeline.decode JsonModel
            |> Pipeline.requiredAt (path "txid") Decode.string
            |> Pipeline.requiredAt (path "hex") Decode.string
            |> Pipeline.optionalAt (path "confirmations") Decode.int 0
            |> Pipeline.optionalAt (path "blockhash") (Decode.maybe Decode.string) Nothing
            |> Pipeline.optionalAt (path "blockheight")
                (Decode.maybe Decode.int
                    |> Decode.andThen (\height -> zeroToNothing height |> Decode.succeed)
                )
                Nothing
            |> Pipeline.optionalAt (path "time") (Decode.maybe Decode.int) Nothing
            |> Pipeline.requiredAt (path "vin") (Decode.list decodeVIn)
            |> Pipeline.requiredAt (path "vout") (Decode.list decodeVOut)


decodeVIn : Decode.Decoder VIn
decodeVIn =
    Pipeline.decode VIn
        -- no txid for coinbase txs
        |> Pipeline.optionalAt [ "txid" ]
            (Decode.maybe Decode.string
                |> Decode.andThen (\txid -> zeroesToNothing txid |> Decode.succeed)
            )
            Nothing
        |> Pipeline.optionalAt [ "coinbase" ] (Decode.maybe Decode.string) Nothing
        |> Pipeline.optionalAt [ "amountin" ] decodeAmountIn 0
        |> Pipeline.optionalAt [ "blockheight" ] (Decode.maybe Decode.int) Nothing
        |> Pipeline.optionalAt [ "prevOut" ] (Decode.maybe decodePrevOut) Nothing


decodeAmountIn : Decode.Decoder Float
decodeAmountIn =
    Decode.float
        |> Decode.andThen
            (\float ->
                if float >= 0 then
                    Decode.succeed float
                else if float == -1.0e-8 then
                    --- XXX: unconfirmed txs will return -1 atom as placeholder
                    Decode.succeed 0
                else
                    Debug.crash <| "Unknown amountin: " ++ (toString float)
            )


decodePrevOut : Decode.Decoder PrevOut
decodePrevOut =
    Pipeline.decode PrevOut
        |> Pipeline.requiredAt [ "addresses" ] (Decode.list Decode.string)
        |> Pipeline.requiredAt [ "value" ] Decode.float


decodeVOut : Decode.Decoder VOut
decodeVOut =
    Pipeline.decode VOut
        |> Pipeline.requiredAt [ "value" ] Decode.float
        |> Pipeline.requiredAt [ "scriptPubKey" ] decodeScriptPubKey


decodeScriptPubKey : Decode.Decoder ScriptPubKey
decodeScriptPubKey =
    Pipeline.decode ScriptPubKey
        |> Pipeline.requiredAt [ "asm" ] Decode.string
        |> Pipeline.requiredAt [ "type" ] Decode.string
        |> Pipeline.optionalAt [ "addresses" ] (Decode.list Decode.string) []



-- compute functions (infer stuff from JsonModel)


computeSize : JsonModel -> Int
computeSize jsonModel =
    (String.length jsonModel.hex) // 2


computeType : JsonModel -> String
computeType jsonModel =
    let
        hasVOutType type_ jsonModel =
            List.any (\i -> i.scriptPubKey.type_ == type_) jsonModel.vOut
    in
        --- XXX: simplified detection
        --- full rules can be found in dcrd/blockchain/stake/staketx.go
        if hasVOutType "stakesubmission" jsonModel then
            "Ticket"
        else if hasVOutType "stakegen" jsonModel then
            "Vote " ++ (computeVote jsonModel)
        else if hasVOutType "stakerevoke" jsonModel then
            "Revocation"
        else
            "Regular"


computeVote : JsonModel -> String
computeVote jsonModel =
    -- some examples:
    -- "OP_RETURN 010004000000" -- vote v4 and abstain
    -- "OP_RETURN 050005000000" -- vote v5 and yes
    let
        regex =
            Regex.regex "^OP_RETURN [0-9A-F]{12}$"

        vOut =
            List.head <|
                List.filter
                    (\vOut ->
                        vOut.scriptPubKey.type_
                            == "nulldata"
                            && Regex.contains regex (String.toUpper vOut.scriptPubKey.asm)
                    )
                    jsonModel.vOut

        voteVersion =
            Maybe.andThen
                (\vOut ->
                    vOut.scriptPubKey.asm
                        |> String.slice 14 16
                        |> String.toInt
                        |> Result.toMaybe
                )
                vOut
    in
        case voteVersion of
            Nothing ->
                "v0"

            Just version ->
                "v" ++ (toString version)



-- "methods" to get info from Model


formatTime : Model -> Time -> Html msg
formatTime model now =
    case model.time of
        Nothing ->
            span [] [ text "recent (unconfirmed)" ]

        Just time ->
            TimeExtra.timeAgo time now


vInToAddress : String -> Model -> Float
vInToAddress address model =
    model.vIn
        |> List.filter
            (\vIn ->
                case vIn.prevOut of
                    Nothing ->
                        False

                    Just prevOut ->
                        List.head prevOut.addresses == Just address
            )
        |> List.map .amountIn
        |> List.sum


vOutToAddress : String -> Model -> Float
vOutToAddress address model =
    model.vOut
        |> List.filter
            (\vOut -> Just address == List.head vOut.scriptPubKey.addresses)
        |> List.map .value
        |> List.sum


sentToAddress : String -> Model -> Float
sentToAddress address model =
    vOutToAddress address model - vInToAddress address model


totalVIn : Model -> Float
totalVIn model =
    List.map .amountIn model.vIn |> List.sum


totalVOut : Model -> Float
totalVOut model =
    List.map .value model.vOut |> List.sum


totalSent : Model -> Float
totalSent model =
    Basics.max (totalVIn model) (totalVOut model)


fee : Model -> Float
fee model =
    totalSent model - totalVOut model


feePerKb : Model -> Float
feePerKb model =
    fee model * 1.0e3 / toFloat model.size
