module Block exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode
import Json.Decode
import Date
import Time exposing (Time)
import Lib.DateExtra as DateExtra
import Lib.TimeExtra as TimeExtra
import Lib.JsonApiExtra as JsonApiExtra
import Lib.HtmlAttributesExtra exposing (innerHtml)


type alias Model =
    { hash : String
    , height : Int
    , time : Time
    , confirmations : Int
    , size : Int
    , fetching : Bool
    }


initialModel : Model
initialModel =
    { hash = ""
    , height = -1
    , time = -1
    , confirmations = -1
    , size = -1
    , fetching = False
    }


type alias JsonModel =
    { hash : String
    , height : Int
    , time : Int
    , confirmations : Int
    , size : Int
    }


type Msg
    = FetchByHash String
    | FetchByHeight Int
    | FetchResult (Result Http.Error JsonModel)


view : Model -> Html a
view model =
    div
        [ class "card text-white bg-primary" ]
        [ h4 [ class "card-header" ] [ text <| "Block " ++ toString model.height ]
        , div [ class "card-body" ]
            [ p [ class "card-text" ]
                [ dl [ class "row" ]
                    [ dt [ class "col-3 text-right" ] [ text "hash" ]
                    , dd [ class "col-9" ] [ text "too long" ]
                    , dt [ class "col-3 text-right" ] [ text "height" ]
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
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchByHash hash ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, fetchBlockByHash updatedModel hash )

        FetchByHeight height ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, fetchBlockByHeight updatedModel height )

        FetchResult (Ok jsonModel) ->
            ( { model
                | height = jsonModel.height
                , hash = jsonModel.hash
                , time = Time.second * (toFloat jsonModel.time)
                , confirmations = jsonModel.confirmations
                , size = jsonModel.size
                , fetching = False
              }
            , Cmd.none
            )

        FetchResult (Err _) ->
            ( { model | fetching = False }, Cmd.none )


fetchBlockByHash : Model -> String -> Cmd Msg
fetchBlockByHash model hash =
    let
        params =
            Json.Encode.list
                [ Json.Encode.string hash ]
    in
        JsonApiExtra.post "getblockheader" params FetchResult decodeBlockFetch


fetchBlockByHeight : Model -> Int -> Cmd Msg
fetchBlockByHeight model height =
    let
        params =
            Json.Encode.list
                [ Json.Encode.int height ]
    in
        JsonApiExtra.post "getblockhash" params FetchResult decodeBlockFetch


decodeBlockFetch : Json.Decode.Decoder JsonModel
decodeBlockFetch =
    Json.Decode.map5 JsonModel
        (Json.Decode.at [ "result", "hash" ] Json.Decode.string)
        (Json.Decode.at [ "result", "height" ] Json.Decode.int)
        (Json.Decode.at [ "result", "time" ] Json.Decode.int)
        (Json.Decode.at [ "result", "confirmations" ] Json.Decode.int)
        (Json.Decode.at [ "result", "size" ] Json.Decode.int)
