module Block exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode
import Json.Decode
import Lib.JsonApiExtra as JsonApiExtra
import Lib.HtmlAttributesExtra exposing (innerHtml)


type alias Model =
    { hash : String
    , height : Int
    , confirmations : Int
    , fetching : Bool
    }


initialModel : Model
initialModel =
    { hash = ""
    , height = -1
    , confirmations = -1
    , fetching = False
    }


type alias JsonModel =
    { hash : String
    , height : Int
    , confirmations : Int
    }


type Msg
    = Fetch String
    | FetchResult (Result Http.Error JsonModel)


view : Model -> Html a
view model =
    tr []
        [ td [ class "align-middle" ] [ input [ type_ "checkbox" ] [] ]
        , td [ title "Height" ] [ text <| toString model.height ]
        , td [ class "text-right" ] [ text model.hash ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch query ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, fetchBlock updatedModel query )

        FetchResult (Ok jsonModel) ->
            ( { model
                | height = jsonModel.height
                , hash = jsonModel.hash
                , confirmations = jsonModel.confirmations
                , fetching = False
              }
            , Cmd.none
            )

        FetchResult (Err _) ->
            ( { model | fetching = False }, Cmd.none )


fetchBlock : Model -> String -> Cmd Msg
fetchBlock model query =
    let
        params =
            Json.Encode.list
                [ Json.Encode.string query ]
    in
        JsonApiExtra.post "getblockheader" params FetchResult decodeBlockFetch


decodeBlockFetch : Json.Decode.Decoder JsonModel
decodeBlockFetch =
    Json.Decode.map3 JsonModel
        (Json.Decode.field "hash" Json.Decode.string)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "confirmations" Json.Decode.int)
