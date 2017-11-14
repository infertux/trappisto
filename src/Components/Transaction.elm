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
    , coin : Coin
    }


initialModel : Coin -> Model
initialModel coin =
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
    , coin = coin
    }


modelFromJson : JsonModel -> Coin -> Model
modelFromJson jsonModel coin =
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
    , coin = coin
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


view : Model -> Html a
view model =
    let
        formatType type_ =
            span [ class "badge badge badge-success" ] [ text type_ ]

        formatBlock hash height =
            case ( hash, height ) of
                ( Nothing, Nothing ) ->
                    span [] [ text "N/A (unconfirmed)" ]

                ( Just hash, Nothing ) ->
                    a [ href hash ] [ text <| shortHash hash ]

                ( Nothing, Just height ) ->
                    a [ href <| toString height ] [ text <| toString height ]

                ( Just hash, Just height ) ->
                    a [ href hash ] [ text <| toString height ]

        formatFees model =
            case model.coin of
                DCR ->
                    [ ( "total sent", Just <| formatAmount (totalSent model) )
                    , ( "fee"
                      , Just <|
                            span []
                                [ formatAmount (fee model)
                                , span [] [ text " (" ]
                                , formatAmount (feePerKb model)
                                , span [] [ text "/kB)" ]
                                ]
                      )
                    ]

                _ ->
                    []

        formatAddresses scriptPubKey =
            div [] <|
                (List.map
                    (\address -> div [] [ a [ href address ] [ text address ] ])
                    scriptPubKey.addresses
                )

        formatVIn vIn =
            vIn
                |> List.map
                    (\vIn ->
                        li [ class "list-group-item bg-secondary" ]
                            [ span [ class "badge badge-info" ]
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
                                [ text <|
                                    (if vIn.amountIn > 0 then
                                        toString vIn.amountIn
                                     else
                                        ""
                                    )
                                ]
                            , div []
                                [ (case vIn.txId of
                                    Just hash ->
                                        a
                                            [ href hash ]
                                            [ text <| "Transaction " ++ shortHash hash ]

                                    Nothing ->
                                        span [] []
                                  )
                                ]
                            ]
                    )

        formatVOut vOut =
            vOut
                |> List.map
                    (\vOut ->
                        li [ class "list-group-item bg-secondary" ]
                            [ span [ class "badge badge-info" ] [ text vOut.scriptPubKey.type_ ]
                            , span [ class "float-right" ] [ formatAmount vOut.value ]
                            , formatAddresses vOut.scriptPubKey
                            , code [ class "mt-2" ] [ text vOut.scriptPubKey.asm ]
                            ]
                    )
    in
        div [ class "row" ]
            [ div [ class "col-12 col-xl-10 offset-xl-1" ]
                [ div
                    [ class "card bg-dark" ]
                    [ h5 [ class "card-header" ]
                        [ span [] [ text <| "Transaction " ++ model.hash ]
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
                                        , Just <| span [] [ text <| formatTime model ]
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
                            [ div [ class "col" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ]
                                        [ text <| pluralize (List.length model.vIn) "input" ]
                                    ]
                                , ul [ class "list-group list-group-flush" ] <|
                                    formatVIn model.vIn
                                ]
                            , div [ class "col" ]
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
                    ( modelFromJson jsonModel model.coin, Cmd.none )

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
        JsonRpc.post "getrawtransaction"
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
            |> Pipeline.optionalAt (path "blockheight") (Decode.maybe Decode.int) Nothing
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
        |> Pipeline.optionalAt [ "amountin" ] Decode.float -1
        |> Pipeline.optionalAt [ "blockheight" ] (Decode.maybe Decode.int) Nothing


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
            List.map (\i -> i.scriptPubKey.type_) jsonModel.vOut
                |> List.member type_
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


formatTime : Model -> String
formatTime model =
    case model.time of
        Nothing ->
            "recent (unconfirmed)"

        Just time ->
            TimeExtra.toISOString time


vInToAddress : String -> Model -> Float
vInToAddress address model =
    model.vIn
        |> List.filter (\vIn -> vIn.txId /= Nothing)
        -- FIXME: txId or coinbase?
        |> List.map .amountIn
        |> List.sum
        |> negate


vOutToAddress : String -> Model -> Float
vOutToAddress address model =
    model.vOut
        |> List.filter
            (\vOut -> Just address == List.head vOut.scriptPubKey.addresses)
        |> List.map .value
        |> List.sum


sentToAddress : String -> Model -> Maybe Float
sentToAddress address model =
    let
        outpoint =
            model.vIn |> List.any (\vIn -> vIn.txId /= Nothing)
    in
        if outpoint then
            Nothing
        else
            Just <| vOutToAddress address model


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
