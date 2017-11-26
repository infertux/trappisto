module Components.Address exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Time exposing (Time)
import Json.Encode as Encode
import Json.Decode as Decode
import Lib.JsonRpc as JsonRpc
import Trappisto.Config exposing (..)
import Trappisto.Helpers exposing (..)
import Components.Transaction as Transaction


type alias Model =
    { address : String
    , transactions : List Transaction.Model
    , tickets : List String
    , fetching : Bool
    , error : Maybe String
    , config : Config
    }


initialModel : Config -> Model
initialModel config =
    { address = ""
    , transactions = []
    , tickets = []
    , fetching = False
    , error = Nothing
    , config = config
    }


modelFromJson : JsonModel -> Config -> Model
modelFromJson jsonModel config =
    { address = jsonModel.address
    , transactions = List.map (\tx -> Transaction.modelFromJson tx config) jsonModel.transactions
    , tickets = []
    , fetching = False
    , error = Nothing
    , config = config
    }


type alias JsonModel =
    { address : String
    , transactions : List Transaction.JsonModel
    }


type Msg
    = SearchRawTransactions String
    | SearchRawTransactionsResult (Result Http.Error JsonModel)
    | TicketsForAddress String
    | TicketsForAddressResult (Result Http.Error (List String))


view : Model -> Time -> Html a
view model now =
    let
        details model =
            if missingTransactions model then
                [ div [ class "alert alert-warning" ]
                    [ text <|
                        "Only the "
                            ++ toString maxTransactionCount
                            ++ " most recent transactions are shown below."
                    ]
                ]
            else
                [ dlBuilder <|
                    [ ( "total received", Maybe.map formatAmount <| totalReceived model )
                    , ( "total sent", Maybe.map formatAmount <| totalSent model )
                    , ( "balance", Maybe.map formatAmount <| balance model )
                    ]
                ]

        transactions model =
            model.transactions
                |> List.map
                    (\tx ->
                        tr []
                            [ td [] [ queryLink tx.hash (shortHash tx.hash) [] ]
                            , td [ class "d-none d-sm-block" ] [ text tx.type_ ]
                            , td []
                                [ Transaction.sentToAddress model.address tx |> formatAmount ]
                            , td [ class "d-none d-sm-block" ] [ Transaction.formatTime tx now ]
                            , td [] [ text <| toString tx.confirmations ]
                            ]
                    )
    in
        div [ class "row" ]
            [ div [ class "col" ]
                [ div [ class "card bg-dark" ]
                    [ h5 [ class "card-header d-flex justify-content-between" ]
                        [ span [ class "d-inline d-lg-none align-self-center" ] [ text <| "Address " ++ (shortAddress model.address) ]
                        , span [ class "d-none d-lg-inline align-self-center" ] [ text <| "Address " ++ model.address ]
                        , dcrDataLink <| "address/" ++ model.address
                        ]
                    , div [ class "card-body" ]
                        [ p [ class "card-text" ] (details model)
                        , hr [] []
                        , div [ class "row" ]
                            [ div [ class "col" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ]
                                        [ text <|
                                            pluralize (List.length model.transactions)
                                                "transaction"
                                        ]
                                    ]
                                , table [ class "table table-dark table-striped" ]
                                    [ thead []
                                        [ tr []
                                            [ th [] [ text "hash" ]
                                            , th [ class "d-none d-sm-block" ] [ text "type" ]
                                            , th [] [ text "received" ]
                                            , th [ class "d-none d-sm-block" ] [ text "time" ]
                                            , th []
                                                [ abbr [ title "confirmations" ] [ text "conf." ] ]
                                            ]
                                        ]
                                    , tbody [] <| transactions model
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchRawTransactions address ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                case model.config.coin of
                    DCR ->
                        ( updatedModel, searchRawTransactions updatedModel address )

                    _ ->
                        ( { model
                            | error =
                                Just <|
                                    "Trappisto does not support Bitcoin addresses "
                                        ++ "but you can explore this address on "
                                        ++ "<a target=\"_blank\" href=\"https://blockchair.com/search?q="
                                        ++ address
                                        ++ "\">blockchair.com</a>"
                          }
                        , Cmd.none
                        )

        SearchRawTransactionsResult result ->
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

        _ ->
            -- TODO: tickets
            ( model, Cmd.none )


searchRawTransactions : Model -> String -> Cmd Msg
searchRawTransactions model address =
    let
        params =
            Encode.list
                [ Encode.string address

                -- verbose
                , Encode.int 1

                -- skip
                , Encode.int 0

                -- count
                , Encode.int maxTransactionCount

                -- vinextra to get prevOut addresses
                , Encode.int 1

                -- reverse
                , Encode.bool True
                ]
    in
        JsonRpc.send
            model.config.rpcEndpoint
            "searchrawtransactions"
            params
            SearchRawTransactionsResult
            (decodeSearchRawTransactions address)


decodeSearchRawTransactions : String -> Decode.Decoder JsonModel
decodeSearchRawTransactions address =
    Decode.map2 JsonModel
        (Decode.succeed address)
        (Decode.field "result"
            (Decode.list (Transaction.decodeGetRawTransaction False))
        )



-- "methods" to get info from Model


maxTransactionCount : Int
maxTransactionCount =
    250


missingTransactions : Model -> Bool
missingTransactions model =
    List.length model.transactions >= maxTransactionCount


totalReceived : Model -> Maybe Float
totalReceived model =
    if missingTransactions model then
        Nothing
    else
        model.transactions
            |> List.map (\tx -> Transaction.vOutToAddress model.address tx)
            |> List.sum
            |> Just


totalSent : Model -> Maybe Float
totalSent model =
    if missingTransactions model then
        Nothing
    else
        model.transactions
            |> List.map (\tx -> Transaction.vInToAddress model.address tx)
            |> List.sum
            |> Just


balance : Model -> Maybe Float
balance model =
    Maybe.map2 (-) (totalReceived model) (totalSent model)
