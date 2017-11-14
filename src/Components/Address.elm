module Components.Address exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode as Encode
import Json.Decode as Decode
import Lib.JsonRpc as JsonRpc
import Trappisto.Helpers exposing (..)
import Components.Transaction as Transaction


type alias Model =
    { address : String
    , transactions : List Transaction.Model
    , tickets : List String
    , fetching : Bool
    , error : Maybe String
    , coin : Coin
    }


initialModel : Coin -> Model
initialModel coin =
    { address = ""
    , transactions = []
    , tickets = []
    , fetching = False
    , error = Nothing
    , coin = coin
    }


modelFromJson : JsonModel -> Coin -> Model
modelFromJson jsonModel coin =
    { address = jsonModel.address
    , transactions = List.map (\tx -> Transaction.modelFromJson tx coin) jsonModel.transactions
    , tickets = []
    , fetching = False
    , error = Nothing
    , coin = coin
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


view : Model -> Html a
view model =
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
                            [ td [] [ a [ href tx.hash ] [ text <| shortHash tx.hash ] ]
                            , td [] [ text tx.type_ ]
                            , td []
                                [ (Transaction.sentToAddress model.address tx
                                    |> Maybe.map formatAmount
                                    |> Maybe.withDefault
                                        (span [] [ text "?" ])
                                  )
                                ]
                            , td [] [ text <| Transaction.formatTime tx ]
                            , td [] [ text <| toString tx.confirmations ]
                            ]
                    )
    in
        div [ class "row" ]
            [ div [ class "col-8 offset-2" ]
                [ div [ class "card bg-dark" ]
                    [ h5 [ class "card-header" ]
                        [ span [] [ text <| "Address " ++ model.address ]
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
                                            toString (List.length model.transactions)
                                                ++ " transactions"
                                        ]
                                    ]
                                , table [ class "table table-dark table-striped" ]
                                    [ thead []
                                        [ tr []
                                            [ th [] [ text "ID" ]
                                            , th [] [ text "type" ]
                                            , th [] [ text "credit" ]
                                            , th [] [ text "time" ]
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
                case model.coin of
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
                    ( modelFromJson jsonModel model.coin, Cmd.none )

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
        JsonRpc.post "searchrawtransactions"
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
    50


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
    let
        amounts =
            List.map (\tx -> Transaction.sentToAddress model.address tx)
                model.transactions

        unkown =
            amounts |> List.any (\amount -> amount == Nothing)
    in
        if missingTransactions model then
            Nothing
        else if unkown then
            Nothing
        else
            amounts
                |> List.map (\amount -> Maybe.withDefault 0 amount)
                |> List.sum
                |> Just



--- FIXME: should not be a Maybe


balance : Model -> Maybe Float
balance model =
    Maybe.map2 (-) (totalReceived model) (totalSent model)
