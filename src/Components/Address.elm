module Components.Address exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode as Encode
import Json.Decode as Decode
import Lib.JsonRpc as JsonRpc
import Lib.Decred exposing (..)
import Components.Transaction as Transaction


type alias Model =
    { address : String
    , transactions : List Transaction.Model
    , tickets : List String
    , fetching : Bool
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { address = ""
    , transactions = []
    , tickets = []
    , fetching = False
    , error = Nothing
    }


modelFromJson : JsonModel -> Model
modelFromJson jsonModel =
    { address = jsonModel.address
    , transactions = List.map Transaction.modelFromJson jsonModel.transactions
    , tickets = []
    , fetching = False
    , error = Nothing
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
        formatFoo foo =
            span [ class "badge badge badge-success" ] [ text foo ]
    in
        div [ class "row" ]
            [ div [ class "col-6 offset-3" ]
                [ div [ class "card bg-dark" ]
                    [ h5 [ class "card-header" ]
                        [ span [] [ text <| "Address " ++ model.address ]
                        , a
                            [ class "float-right"
                            , target "_blank"
                            , title "Open on dcrdata.org"
                            , href <| "https://explorer.dcrdata.org/explorer/address/" ++ model.address
                            ]
                            [ span [ class "oi oi-external-link" ] [] ]
                        ]
                    , div [ class "card-body" ]
                        [ p [ class "card-text" ]
                            [ dl [ class "row" ]
                                [ dt [ class "col-3 text-right" ] [ text "foo" ]
                                , dd [ class "col-9" ] [ text "?" ]
                                , dt [ class "col-3 text-right" ] [ text "last time" ]
                                , dd [ class "col-9" ] [ text "?" ]
                                , dt [ class "col-3 text-right" ] [ text "balance" ]
                                , dd [ class "col-9" ] [ text <| dcrAmount (balance model) ]
                                ]
                            ]
                        , hr [] []
                        , div [ class "row" ]
                            [ div [ class "col" ]
                                [ h4 [ class "text-center" ]
                                    [ span [ class "badge badge-pill badge-info" ] [ text "transactions" ]
                                    ]
                                , ul [ class "list-group list-group-flush" ] <|
                                    List.map
                                        (\transaction ->
                                            li [ class "list-group-item bg-secondary" ]
                                                [ span [ class "badge badge-info" ] [ text "foo" ] ]
                                        )
                                        model.transactions
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
                ( updatedModel, searchRawTransactions updatedModel address )

        SearchRawTransactionsResult result ->
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
                , Encode.int 100

                -- vinextra
                , Encode.int 0

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
            (Decode.list (Transaction.decodeGetRawTransaction Nothing))
        )



-- "methods" to get info from Model


balance : Model -> Float
balance model =
    -- TODO:
    0
