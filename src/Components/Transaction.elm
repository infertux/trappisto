module Components.Transaction exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time exposing (Time)
import Lib.TimeExtra as TimeExtra
import Lib.JsonRpc as JsonRpc
import Lib.Decred as Decred


type alias Model =
    { hash : String
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
    , size = -1
    , confirmations = -1
    , blockHash = Nothing
    , time = Nothing
    , vIn = []
    , vOut = []
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


modelFromJson : JsonModel -> Model
modelFromJson jsonModel =
    { hash = jsonModel.hash
    , size = (String.length jsonModel.hex) // 2
    , confirmations = jsonModel.confirmations
    , blockHash = jsonModel.blockHash
    , time = Maybe.map timestampToTime jsonModel.time
    , vIn = List.sortBy .amountIn jsonModel.vIn |> List.reverse
    , vOut = List.sortBy .value jsonModel.vOut |> List.reverse
    , fetching = False
    , error = Nothing
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
    }


timestampToTime : Int -> Time
timestampToTime int =
    Time.second * (toFloat int)


totalSpent : Model -> Float
totalSpent model =
    List.map .amountIn model.vIn |> List.foldr (+) 0


fee : Model -> Float
fee model =
    let
        totalOut =
            List.map .value model.vOut |> List.foldr (+) 0
    in
        totalSpent model - totalOut


feePerKb : Model -> Float
feePerKb model =
    fee model * 1.0e3 / toFloat model.size


dcrAmount : Float -> String
dcrAmount float =
    let
        rounded =
            -- remove any floating point arithmetic errors
            float * 1.0e8 |> round |> toFloat |> (flip (/)) 1.0e8
    in
        (toString rounded) ++ " DCR"


type Msg
    = GetRawTransaction String
    | GetRawTransactionResult (Result Http.Error JsonModel)


view : Model -> Html a
view model =
    let
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
    in
        div
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
                        [ dt [ class "col-3 text-right" ] [ text "confirmations" ]
                        , dd [ class "col-9" ] [ text <| toString model.confirmations ]
                        , dt [ class "col-3 text-right" ] [ text "time" ]
                        , dd [ class "col-9" ] [ formatTime model.time ]
                        , dt [ class "col-3 text-right" ] [ text "block" ]
                        , dd [ class "col-9" ] [ formatBlock model.blockHash ]
                        , dt [ class "col-3 text-right" ] [ text "size" ]
                        , dd [ class "col-9" ] [ text <| toString model.size ++ " bytes" ]
                        , dt [ class "col-3 text-right" ] [ text "total spent" ]
                        , dd [ class "col-9" ] [ text <| dcrAmount (totalSpent model) ]
                        , dt [ class "col-3 text-right" ] [ text "fee" ]
                        , dd [ class "col-9" ]
                            [ text <|
                                dcrAmount (fee model)
                                    ++ (" (" ++ (dcrAmount (feePerKb model)) ++ "/kB)")
                            ]
                        ]
                    ]
                ]
            , hr [] []
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [ class "text-center" ] [ text "inputs" ]
                    , ul [ class "list-group list-group-flush" ] <|
                        List.map
                            (\vIn ->
                                li [ class "list-group-item bg-primary" ]
                                    [ span [ class "badge badge-info" ]
                                        [ text
                                            (if vIn.coinbase /= Nothing then
                                                "coinbase"
                                             else
                                                ""
                                            )
                                        ]
                                    , span [ class "float-right" ] [ text <| dcrAmount vIn.amountIn ]
                                    , (case vIn.txId of
                                        Just hash ->
                                            a [ href hash ] [ text <| Decred.shortHash hash ]

                                        Nothing ->
                                            span [] []
                                      )
                                    ]
                            )
                            model.vIn
                    ]
                , div [ class "col" ]
                    [ h4 [ class "text-center" ] [ text "outputs" ]
                    , ul [ class "list-group list-group-flush" ] <|
                        List.map
                            (\vOut ->
                                li [ class "list-group-item bg-success" ]
                                    [ span [ class "badge badge-info" ] [ text vOut.scriptPubKey.type_ ]
                                    , span [ class "float-right" ] [ text <| dcrAmount vOut.value ]
                                    , code [ class "mt-2" ] [ text vOut.scriptPubKey.asm ]
                                    ]
                            )
                            model.vOut
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
        JsonRpc.post "getrawtransaction" params GetRawTransactionResult decodeGetRawTransaction


decodeGetRawTransaction : Decode.Decoder JsonModel
decodeGetRawTransaction =
    Pipeline.decode JsonModel
        |> Pipeline.requiredAt [ "result", "txid" ] Decode.string
        |> Pipeline.requiredAt [ "result", "hex" ] Decode.string
        |> Pipeline.optionalAt [ "result", "confirmations" ] Decode.int 0
        |> Pipeline.optionalAt [ "result", "blockhash" ] (Decode.maybe Decode.string) Nothing
        |> Pipeline.optionalAt [ "result", "time" ] (Decode.maybe Decode.int) Nothing
        |> Pipeline.requiredAt [ "result", "vin" ] (Decode.list decodeVIn)
        |> Pipeline.requiredAt [ "result", "vout" ] (Decode.list decodeVOut)


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
