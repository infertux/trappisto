module Components.Status exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode
import Json.Decode
import Lib.JsonRpc as JsonRpc


type alias Model =
    { blocks : Int
    , connections : Int
    , fetching : Bool
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { blocks = -1
    , connections = -1
    , fetching = False
    , error = Nothing
    }


type alias JsonModel =
    { blocks : Int
    , connections : Int
    }


type Msg
    = GetInfo
    | GetInfoResult (Result Http.Error JsonModel)


view : Model -> Html a
view model =
    div [ class "col text-center" ]
        [ span [] [ text "Last block: " ]
        , a [ href (toString model.blocks) ] [ text <| toString model.blocks ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInfo ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, getInfo updatedModel )

        GetInfoResult result ->
            case result of
                Ok jsonModel ->
                    ( { model
                        | blocks = jsonModel.blocks
                        , connections = jsonModel.connections
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


getInfo : Model -> Cmd Msg
getInfo model =
    let
        params =
            Json.Encode.list []
    in
        JsonRpc.post "getinfo" params GetInfoResult decodeStatusFetch


decodeStatusFetch : Json.Decode.Decoder JsonModel
decodeStatusFetch =
    Json.Decode.map2 JsonModel
        (Json.Decode.at [ "result", "blocks" ] Json.Decode.int)
        (Json.Decode.at [ "result", "connections" ] Json.Decode.int)
