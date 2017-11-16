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
import Trappisto.Config exposing (..)
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
    , config : Config
    }


initialModel : Config -> Model
initialModel config =
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
    , config = config
    }


modelFromJson : JsonModel -> Config -> Model
modelFromJson jsonModel config =
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
    , config = config
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


view : Model -> Time -> Html Msg
view model now =
    let
        sibbling maybeHash direction =
            case maybeHash of
                Nothing ->
                    span [] []

                Just hash ->
                    let
                        ( label, classes ) =
                            if direction == "right" then
                                ( "Next block", " float-right ml-2" )
                            else
                                ( "Previous block", "" )
                    in
                        queryLink hash
                            ("<span class=\"oi oi-chevron-" ++ direction ++ "\"></span>")
                            [ class <| "btn btn-secondary" ++ classes, title label ]

        transactions list color =
            case list of
                [] ->
                    Nothing

                list ->
                    div []
                        ((span [ class "mr-2" ] [ text (toString <| List.length list) ])
                            :: (List.map
                                    (\tx ->
                                        queryLink tx
                                            (shortHash tx)
                                            [ class <| "ml-1 badge badge-" ++ color ]
                                    )
                                    list
                               )
                        )
                        |> Just

        allTransactions =
            case model.config.coin of
                DCR ->
                    [ ( "stake transactions", transactions model.tickets "secondary" )
                    , ( "regular transactions", transactions model.transactions "light" )
                    ]

                _ ->
                    [ ( "transactions", transactions model.transactions "light" ) ]
    in
        div [ class "row align-items-center" ]
            [ div [ class "col" ]
                [ div
                    [ class "card bg-dark" ]
                    [ h5 [ class "card-header" ]
                        [ sibbling model.previousBlockHash "left"
                        , span [ class "ml-4" ]
                            [ text <| "Block " ++ model.hash ]
                        , sibbling model.nextBlockHash "right"
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
                                        , Just <| TimeExtra.timeAgo model.time now
                                        )
                                      , ( "confirmations"
                                        , Just <|
                                            span []
                                                [ text <| formatNumber model.confirmations ]
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
                    ( modelFromJson jsonModel model.config, Cmd.none )

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
        JsonRpc.send model.config.rpcEndpoint
            "getblock"
            params
            GetBlockResult
            decodeGetBlock


getBlockHash : Model -> Int -> Cmd Msg
getBlockHash model height =
    let
        params =
            Encode.list [ Encode.int height ]
    in
        JsonRpc.send
            model.config.rpcEndpoint
            "getblockhash"
            params
            GetBlockHashResult
            decodeGetBlockHash


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
