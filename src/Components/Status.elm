module Components.Status exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Json.Encode
import Json.Decode
import Lib.JsonRpc


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
    = Fetch
    | FetchResult (Result Http.Error JsonModel)


view : Model -> Html a
view model =
    div [ class "col" ]
        [ ul [ class "list-unstyled" ]
            [ li [] [ text <| "Blocks: " ++ toString model.blocks ]
            , li [] [ text <| "Connections: " ++ toString model.connections ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            let
                updatedModel =
                    { model | fetching = True }
            in
                ( updatedModel, fetchStatus updatedModel )

        FetchResult (Ok jsonModel) ->
            ( { model
                | blocks = jsonModel.blocks
                , connections = jsonModel.connections
                , fetching = False
                , error = Nothing
              }
            , Cmd.none
            )

        FetchResult (Err _) ->
            -- FIXME: set error
            ( { model | fetching = False }, Cmd.none )


fetchStatus : Model -> Cmd Msg
fetchStatus model =
    let
        params =
            Json.Encode.list []
    in
        Lib.JsonRpc.post "getinfo" params FetchResult decodeStatusFetch


decodeStatusFetch : Json.Decode.Decoder JsonModel
decodeStatusFetch =
    Json.Decode.map2 JsonModel
        (Json.Decode.at [ "result", "blocks" ] Json.Decode.int)
        (Json.Decode.at [ "result", "connections" ] Json.Decode.int)
