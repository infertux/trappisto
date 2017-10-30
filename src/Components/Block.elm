module Components.Block exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode
import Json.Decode
import Time exposing (Time)
import Lib.TimeExtra as TimeExtra
import Lib.JsonRpc as JsonRpc


type alias Model =
    { hash : String
    , height : Int
    , time : Time
    , confirmations : Int
    , size : Int
    , fetching : Bool
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { hash = ""
    , height = -1
    , time = -1
    , confirmations = -1
    , size = -1
    , fetching = False
    , error = Nothing
    }


type alias JsonModel =
    { hash : String
    , height : Int
    , time : Int
    , confirmations : Int
    , size : Int
    }


type Msg
    = GetBlockHeader String
    | GetBlockHash Int
    | GetBlockHeaderResult (Result Http.Error JsonModel)
    | GetBlockHashResult (Result Http.Error String)


view : Model -> Html a
view model =
    div
        [ class "card bg-dark" ]
        [ h5 [ class "card-header" ] [ text <| "Block " ++ model.hash ]
        , div [ class "card-body" ]
            [ p [ class "card-text" ]
                [ dl [ class "row" ]
                    [ dt [ class "col-3 text-right" ] [ text "height" ]
                    , dd [ class "col-9" ] [ text <| toString model.height ]
                    , dt [ class "col-3 text-right" ] [ text "time" ]
                    , dd [ class "col-9" ] [ text <| TimeExtra.toISOString model.time ]
                    , dt [ class "col-3 text-right" ] [ text "confirmations" ]
                    , dd [ class "col-9" ] [ text <| toString model.confirmations ]
                    , dt [ class "col-3 text-right" ] [ text "size" ]
                    , dd [ class "col-9" ] [ text <| toString model.size ]
                    ]
                ]
            ]
        , div [ class "card-footer" ]
            [ small [ class "text-muted" ]
                [ a
                    [ target "_blank"
                    , href <| "https://explorer.dcrdata.org/explorer/block/" ++ model.hash
                    ]
                    [ text "More details at dcrdata.org" ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetBlockHeader hash ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, getBlockHeader updatedModel hash )

        GetBlockHash height ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, getBlockHash updatedModel height )

        GetBlockHeaderResult result ->
            case result of
                Ok jsonModel ->
                    ( { model
                        | height = jsonModel.height
                        , hash = jsonModel.hash
                        , time = Time.second * (toFloat jsonModel.time)
                        , confirmations = jsonModel.confirmations
                        , size = jsonModel.size
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

        GetBlockHashResult result ->
            case result of
                Ok hash ->
                    ( { model | error = Nothing }, getBlockHeader model hash )

                Err error ->
                    ( { model
                        | error = JsonRpc.parseError error
                        , fetching = False
                      }
                    , Cmd.none
                    )


getBlockHeader : Model -> String -> Cmd Msg
getBlockHeader model hash =
    let
        params =
            Json.Encode.list
                [ Json.Encode.string hash ]
    in
        JsonRpc.post "getblockheader" params GetBlockHeaderResult decodeGetBlockHeader


getBlockHash : Model -> Int -> Cmd Msg
getBlockHash model height =
    let
        params =
            Json.Encode.list
                [ Json.Encode.int height ]
    in
        JsonRpc.post "getblockhash" params GetBlockHashResult decodeGetBlockHash


decodeGetBlockHeader : Json.Decode.Decoder JsonModel
decodeGetBlockHeader =
    Json.Decode.map5 JsonModel
        (Json.Decode.at [ "result", "hash" ] Json.Decode.string)
        (Json.Decode.at [ "result", "height" ] Json.Decode.int)
        (Json.Decode.at [ "result", "time" ] Json.Decode.int)
        (Json.Decode.at [ "result", "confirmations" ] Json.Decode.int)
        (Json.Decode.at [ "result", "size" ] Json.Decode.int)


decodeGetBlockHash : Json.Decode.Decoder String
decodeGetBlockHash =
    Json.Decode.at [ "result" ] Json.Decode.string
