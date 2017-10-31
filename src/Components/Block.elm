module Components.Block exposing (..)

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
    , height : Int
    , time : Time
    , confirmations : Int
    , size : Int
    , ticketPrice : Float
    , transactions : List String
    , tickets : List String
    , previousBlockHash : Maybe String
    , nextBlockHash : Maybe String
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
    , ticketPrice = -1
    , transactions = []
    , tickets = []
    , previousBlockHash = Nothing
    , nextBlockHash = Nothing
    , fetching = False
    , error = Nothing
    }


type alias JsonModel =
    { hash : String
    , height : Int
    , time : Int
    , confirmations : Int
    , size : Int
    , ticketPrice : Float
    , transactions : List String
    , tickets : List String
    , previousBlockHash : Maybe String
    , nextBlockHash : Maybe String
    }


type Msg
    = GetBlock String
    | GetBlockHash Int
    | GetBlockResult (Result Http.Error JsonModel)
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
                    , dd [ class "col-9" ] [ text <| toString model.size ++ " bytes" ]
                    , dt [ class "col-3 text-right" ] [ text "ticket price" ]
                    , dd [ class "col-9" ] [ text <| toString model.ticketPrice ++ " DCR" ]
                    ]
                ]
            ]
        , ul [ class "list-group list-group-flush" ] <|
            List.map
                (\ticket ->
                    li [ class "list-group-item bg-dark" ]
                        [ a [ href <| "#" ++ ticket ] [ text <| "Stake transaction " ++ ticket ]
                        ]
                )
                model.tickets
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
        GetBlock hash ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, getBlock updatedModel hash )

        GetBlockHash height ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, getBlockHash updatedModel height )

        GetBlockResult result ->
            case result of
                Ok jsonModel ->
                    let
                        previousBlockHash =
                            case jsonModel.previousBlockHash of
                                Just hash ->
                                    if hash /= "0000000000000000000000000000000000000000000000000000000000000000" then
                                        Just hash
                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                    in
                        ( { model
                            | height = jsonModel.height
                            , hash = jsonModel.hash
                            , time = Time.second * (toFloat jsonModel.time)
                            , confirmations = jsonModel.confirmations
                            , size = jsonModel.size
                            , ticketPrice = jsonModel.ticketPrice
                            , transactions = jsonModel.transactions
                            , tickets = jsonModel.tickets
                            , previousBlockHash = previousBlockHash
                            , nextBlockHash = jsonModel.nextBlockHash
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
                    ( { model | error = Nothing }, getBlock model hash )

                Err error ->
                    ( { model
                        | error = JsonRpc.parseError error
                        , fetching = False
                      }
                    , Cmd.none
                    )


getBlock : Model -> String -> Cmd Msg
getBlock model hash =
    let
        params =
            Encode.list [ Encode.string hash ]
    in
        JsonRpc.post "getblock" params GetBlockResult decodeGetBlock


getBlockHash : Model -> Int -> Cmd Msg
getBlockHash model height =
    let
        params =
            Encode.list [ Encode.int height ]
    in
        JsonRpc.post "getblockhash" params GetBlockHashResult decodeGetBlockHash


decodeGetBlock : Decode.Decoder JsonModel
decodeGetBlock =
    Pipeline.decode JsonModel
        |> Pipeline.requiredAt [ "result", "hash" ] Decode.string
        |> Pipeline.requiredAt [ "result", "height" ] Decode.int
        |> Pipeline.requiredAt [ "result", "time" ] Decode.int
        |> Pipeline.requiredAt [ "result", "confirmations" ] Decode.int
        |> Pipeline.requiredAt [ "result", "size" ] Decode.int
        |> Pipeline.requiredAt [ "result", "sbits" ] Decode.float
        |> Pipeline.requiredAt [ "result", "tx" ] (Decode.list Decode.string)
        |> Pipeline.optionalAt [ "result", "stx" ] (Decode.list Decode.string) []
        |> Pipeline.optionalAt [ "result", "previousblockhash" ] (Decode.maybe Decode.string) Nothing
        |> Pipeline.optionalAt [ "result", "nextblockhash" ] (Decode.maybe Decode.string) Nothing


decodeGetBlockHash : Decode.Decoder String
decodeGetBlockHash =
    Decode.at [ "result" ] Decode.string
