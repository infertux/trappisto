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
import Lib.Decred exposing (..)


type alias Model =
    { hash : String
    , type_ : String
    , size : Int
    , confirmations : Int
    , blockHash : Maybe String
    , time : Maybe Time
    , vIn : List VIn
    , vOut : List VOut
    , fetching : Bool
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { hash = ""
    , type_ = "?"
    , size = -1
    , confirmations = -1
    , blockHash = Nothing
    , time = Nothing
    , vIn = []
    , vOut = []
    , fetching = False
    , error = Nothing
    }


modelFromJson : JsonModel -> Model
modelFromJson jsonModel =
    { hash = jsonModel.hash
    , type_ = computeType jsonModel
    , size = computeSize jsonModel
    , confirmations = jsonModel.confirmations
    , blockHash = jsonModel.blockHash
    , time = Maybe.map TimeExtra.timestampToTime jsonModel.time
    , vIn = List.sortBy .amountIn jsonModel.vIn |> List.reverse
    , vOut = List.sortBy .value jsonModel.vOut |> List.reverse
    , fetching = False
    , error = Nothing
    }


type alias JsonModel =
    { hash : String
    , hex : String
    , confirmations : Int
    , blockHash : Maybe String
    , time : Maybe Int
    , vIn : List VIn
    , vOut : List VOut
    }


type alias VIn =
    { txId : Maybe String
    , coinbase : Maybe String
    , amountIn : Float
    , blockHeight : Int
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

        formatTime time =
            case time of
                Nothing ->
                    span [] [ text "recent (still in mempool)" ]

                Just time ->
                    span [] [ text <| TimeExtra.toISOString time ]

        formatBlock block =
            case block of
                Nothing ->
                    span [] [ text "N/A (still in mempool)" ]

                Just hash ->
                    a [ href hash ] [ text hash ]

        formatAddresses scriptPubKey =
            div [] <|
                (List.map
                    (\address -> div [] [ a [ href address ] [ text address ] ])
                    scriptPubKey.addresses
                )
    in
        div [ class "row" ]
            [ div [ class "col-6 offset-3" ]
                [ div
                    [ class "card bg-dark" ]
                    [ h5 [ class "card-header" ]
                        [ span [] [ text <| "Transaction " ++ model.hash ]
                        , a
                            [ class "float-right"
                            , target "_blank"
                            , title "Open on dcrdata.org"
                            , href <| "https://explorer.dcrdata.org/explorer/tx/" ++ model.hash
                            ]
                            [ span [ class "oi oi-external-link" ] [] ]
                        ]
                    , div [ class "card-body" ]
                        [ p [ class "card-text" ]
                            [ dl [ class "row" ]
                                [ dt [ class "col-3 text-right" ] [ text "type" ]
                                , dd [ class "col-9" ] [ formatType model.type_ ]
                                , dt [ class "col-3 text-right" ] [ text "confirmations" ]
                                , dd [ class "col-9" ] [ text <| toString model.confirmations ]
                                , dt [ class "col-3 text-right" ] [ text "time" ]
                                , dd [ class "col-9" ] [ formatTime model.time ]
                                , dt [ class "col-3 text-right" ] [ text "block" ]
                                , dd [ class "col-9" ] [ formatBlock model.blockHash ]
                                , dt [ class "col-3 text-right" ] [ text "size" ]
                                , dd [ class "col-9" ] [ text <| toString model.size ++ " bytes" ]
                                , dt [ class "col-3 text-right" ] [ text "total sent" ]
                                , dd [ class "col-9" ] [ text <| dcrAmount (totalSent model) ]
                                , dt [ class "col-3 text-right" ] [ text "fee" ]
                                , dd [ class "col-9" ]
                                    [ text <|
                                        dcrAmount (fee model)
                                            ++ (" (" ++ (dcrAmount (feePerKb model)) ++ "/kB)")
                                    ]
                                ]
                            ]
                        , hr [] []
                        , div [ class "row" ]
                            [ div [ class "col" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ] [ text "inputs" ]
                                    ]
                                , ul [ class "list-group list-group-flush" ] <|
                                    List.map
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
                                                , span [ class "float-right" ] [ text <| dcrAmount vIn.amountIn ]
                                                , div []
                                                    [ (case vIn.txId of
                                                        Just hash ->
                                                            a [ href hash ] [ text <| "Transaction " ++ shortHash hash ]

                                                        Nothing ->
                                                            span [] []
                                                      )
                                                    ]
                                                ]
                                        )
                                        model.vIn
                                ]
                            , div [ class "col" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ] [ text "outputs" ]
                                    ]
                                , ul [ class "list-group list-group-flush" ] <|
                                    List.map
                                        (\vOut ->
                                            li [ class "list-group-item bg-secondary" ]
                                                [ span [ class "badge badge-info" ] [ text vOut.scriptPubKey.type_ ]
                                                , span [ class "float-right" ] [ text <| dcrAmount vOut.value ]
                                                , formatAddresses vOut.scriptPubKey
                                                , code [ class "mt-2" ] [ text vOut.scriptPubKey.asm ]
                                                ]
                                        )
                                        model.vOut
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
                    ( modelFromJson jsonModel, Cmd.none )

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
            (decodeGetRawTransaction (Just "result"))


decodeGetRawTransaction : Maybe String -> Decode.Decoder JsonModel
decodeGetRawTransaction prefix =
    let
        path key =
            case prefix of
                Nothing ->
                    [ key ]

                Just string ->
                    [ string, key ]
    in
        Pipeline.decode JsonModel
            |> Pipeline.requiredAt (path "txid") Decode.string
            |> Pipeline.requiredAt (path "hex") Decode.string
            |> Pipeline.optionalAt (path "confirmations") Decode.int 0
            |> Pipeline.optionalAt (path "blockhash") (Decode.maybe Decode.string) Nothing
            |> Pipeline.optionalAt (path "time") (Decode.maybe Decode.int) Nothing
            |> Pipeline.requiredAt (path "vin") (Decode.list decodeVIn)
            |> Pipeline.requiredAt (path "vout") (Decode.list decodeVOut)


zeroToNothing : Maybe String -> Decode.Decoder (Maybe String)
zeroToNothing maybe =
    let
        zero =
            "0000000000000000000000000000000000000000000000000000000000000000"
    in
        Decode.succeed <|
            case maybe of
                Just string ->
                    if string == zero then
                        Nothing
                    else
                        maybe

                Nothing ->
                    maybe


decodeVIn : Decode.Decoder VIn
decodeVIn =
    Pipeline.decode VIn
        -- no txid for coinbase txs
        |> Pipeline.optionalAt [ "txid" ] (Decode.maybe Decode.string |> Decode.andThen zeroToNothing) Nothing
        |> Pipeline.optionalAt [ "coinbase" ] (Decode.maybe Decode.string) Nothing
        |> Pipeline.requiredAt [ "amountin" ] Decode.float
        |> Pipeline.requiredAt [ "blockheight" ] Decode.int


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
        --- XXX: simplified detection (full rules can be found in dcrd/blockchain/stake/staketx.go)
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


totalVIn : Model -> Float
totalVIn model =
    List.map .amountIn model.vIn |> List.foldr (+) 0


totalVOut : Model -> Float
totalVOut model =
    List.map .value model.vOut |> List.foldr (+) 0


totalSent : Model -> Float
totalSent model =
    Basics.max (totalVIn model) (totalVOut model)


fee : Model -> Float
fee model =
    totalSent model - totalVOut model


feePerKb : Model -> Float
feePerKb model =
    fee model * 1.0e3 / toFloat model.size
