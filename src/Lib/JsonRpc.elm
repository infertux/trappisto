module Lib.JsonRpc exposing (post, parseError)

import Http
import Json.Decode
import Json.Encode
import Time exposing (Time)


type alias RequestParams a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    }


decodeError : String -> String
decodeError json =
    let
        decode =
            Json.Decode.at [ "error", "message" ] Json.Decode.string
                |> Json.Decode.decodeString
    in
        case decode json of
            Ok result ->
                result

            Err err ->
                "Cannot decode error: " ++ err ++ " | " ++ json


parseError : Http.Error -> Maybe String
parseError error =
    case error of
        Http.BadStatus response ->
            Just ("Backend error " ++ toString response.status.code ++ response.status.message)

        Http.BadPayload message response ->
            Just <| decodeError response.body

        error ->
            Just (toString error)


post : String -> Json.Encode.Value -> (Result Http.Error a -> msg) -> Json.Decode.Decoder a -> Cmd msg
post method params result decoder =
    Http.send result <|
        Http.request <|
            makeRequest method params decoder


makeRequest : String -> Json.Encode.Value -> Json.Decode.Decoder a -> RequestParams a
makeRequest method params decoder =
    let
        json =
            Json.Encode.object
                [ ( "jsonrpc", Json.Encode.string "2.0" )
                , ( "id", Json.Encode.int 0 )
                , ( "method", Json.Encode.string method )
                , ( "params", params )
                ]
    in
        { method = "POST"
        , url = "/rpc"
        , body = Http.jsonBody json
        , expect = Http.expectJson decoder
        , headers = []
        , timeout = Nothing
        , withCredentials = False
        }
