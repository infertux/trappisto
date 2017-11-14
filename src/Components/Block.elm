module Components.Block exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time exposing (Time)
import Lib.TimeExtra as TimeExtra
import Lib.JsonRpc as JsonRpc
import Trappisto.Helpers exposing (..)


type alias Model =
    { hash : String
    , height : Int
    , time : Time
    , confirmations : Int
    , size : Int
    , ticketPrice : Maybe Float
    , transactions : List String
    , tickets : List String
    , previousBlockHash : Maybe String
    , nextBlockHash : Maybe String
    , fetching : Bool
    , error : Maybe String
    , coin : Coin
    }


initialModel : Coin -> Model
initialModel coin =
    { hash = ""
    , height = -1
    , time = -1
    , confirmations = -1
    , size = -1
    , ticketPrice = Nothing
    , transactions = []
    , tickets = []
    , previousBlockHash = Nothing
    , nextBlockHash = Nothing
    , fetching = False
    , error = Nothing
    , coin = coin
    }


modelFromJson : JsonModel -> Coin -> Model
modelFromJson jsonModel coin =
    { height = jsonModel.height
    , hash = jsonModel.hash
    , time = Time.second * (toFloat jsonModel.time)
    , confirmations = jsonModel.confirmations
    , size = jsonModel.size
    , ticketPrice = jsonModel.ticketPrice
    , transactions = jsonModel.transactions
    , tickets = jsonModel.tickets
    , previousBlockHash = zeroesToNothing jsonModel.previousBlockHash
    , nextBlockHash = jsonModel.nextBlockHash
    , fetching = False
    , error = Nothing
    , coin = coin
    }


type alias JsonModel =
    { hash : String
    , height : Int
    , time : Int
    , confirmations : Int
    , size : Int
    , ticketPrice : Maybe Float
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


view : Model -> Html Msg
view model =
    let
        sibbling maybeHash html =
            [ case maybeHash of
                Nothing ->
                    span [] []

                Just hash ->
                    a
                        [ class "btn btn-secondary"
                        , href "javascript:void(0)"
                        , onClick (GetBlock hash)
                        ]
                        [ text html ]
            ]

        transactions list color =
            case list of
                [] ->
                    Nothing

                list ->
                    div []
                        ((span [ class "mr-2" ] [ text (toString <| List.length list) ])
                            :: (List.map
                                    (\tx ->
                                        a
                                            [ href tx, class <| "ml-1 badge badge-" ++ color ]
                                            [ text <| shortHash tx ]
                                    )
                                    list
                               )
                        )
                        |> Just

        allTransactions =
            case model.coin of
                DCR ->
                    [ ( "stake transactions", transactions model.tickets "secondary" )
                    , ( "normal transactions", transactions model.transactions "light" )
                    ]

                _ ->
                    [ ( "transactions", transactions model.transactions "light" ) ]
    in
        div [ class "row align-items-center" ]
            [ div [ class "col-1 text-right" ] (sibbling model.previousBlockHash "<")
            , div [ class "col-10" ]
                [ div
                    [ class "card bg-dark" ]
                    [ h5 [ class "card-header" ]
                        [ span [] [ text <| "Block " ++ model.hash ]
                        , dcrDataLink <| "block/" ++ model.hash
                        ]
                    , div [ class "card-body" ]
                        [ p [ class "card-text" ]
                            [ dlBuilder <|
                                List.concat
                                    [ [ ( "height"
                                        , Just <|
                                            span []
                                                [ text <| formatNumber model.height ]
                                        )
                                      , ( "time"
                                        , Just <|
                                            span []
                                                [ text <| TimeExtra.toISOString model.time ]
                                        )
                                      , ( "confirmations"
                                        , Just <|
                                            span []
                                                [ text <| toString model.confirmations ]
                                        )
                                      , ( "size"
                                        , Just <|
                                            span []
                                                [ text <| formatNumber model.size ++ " bytes" ]
                                        )
                                      , ( "ticket price", Maybe.map formatAmount model.ticketPrice )
                                      ]
                                    , allTransactions
                                    ]
                            ]
                        ]
                    ]
                ]
            , div [ class "col-1 text-left" ] (sibbling model.nextBlockHash ">")
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
                    ( modelFromJson jsonModel model.coin, Cmd.none )

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
        |> Pipeline.optionalAt [ "result", "sbits" ] (Decode.maybe Decode.float) Nothing
        |> Pipeline.requiredAt [ "result", "tx" ] (Decode.list Decode.string)
        |> Pipeline.optionalAt [ "result", "stx" ] (Decode.list Decode.string) []
        |> Pipeline.optionalAt [ "result", "previousblockhash" ]
            (Decode.maybe Decode.string)
            Nothing
        |> Pipeline.optionalAt [ "result", "nextblockhash" ]
            (Decode.maybe Decode.string)
            Nothing


decodeGetBlockHash : Decode.Decoder String
decodeGetBlockHash =
    Decode.at [ "result" ] Decode.string
