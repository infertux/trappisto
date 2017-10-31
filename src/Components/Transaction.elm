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


type alias Model =
    { hash : String
    , size : Int
    , confirmations : Int
    , fetching : Bool
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { hash = ""
    , size = -1
    , confirmations = -1
    , fetching = False
    , error = Nothing
    }


type alias JsonModel =
    { hash : String
    , hex : String
    , confirmations : Int
    }


modelFromJson : JsonModel -> Model
modelFromJson jsonModel =
    { hash = jsonModel.hash
    , size = (String.length jsonModel.hex) // 2
    , confirmations = jsonModel.confirmations
    , fetching = False
    , error = Nothing
    }


type Msg
    = GetRawTransaction String
    | GetRawTransactionResult (Result Http.Error JsonModel)


view : Model -> Html a
view model =
    div
        [ class "card bg-dark" ]
        [ h5 [ class "card-header" ] [ text <| "Transaction " ++ model.hash ]
        , div [ class "card-body" ]
            [ p [ class "card-text" ]
                [ dl [ class "row" ]
                    [ dt [ class "col-3 text-right" ] [ text "confirmations" ]
                    , dd [ class "col-9" ] [ text <| toString model.confirmations ]
                    , dt [ class "col-3 text-right" ] [ text "size" ]
                    , dd [ class "col-9" ] [ text <| toString model.size ++ " bytes" ]
                    ]
                ]
            ]
        , div [ class "card-footer" ]
            [ small [ class "text-muted" ]
                [ a
                    [ target "_blank"
                    , href <| "https://explorer.dcrdata.org/explorer/tx/" ++ model.hash
                    ]
                    [ text "More details at dcrdata.org" ]
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
        |> Pipeline.requiredAt [ "result", "confirmations" ] Decode.int



-- |> Pipeline.optionalAt [ "result", "blockhash" ] Decode.string
-- |> Pipeline.optionalAt [ "result", "time" ] Decode.int
