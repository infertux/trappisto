module Lib.JsonRpc exposing (send, sendWithoutParams, parseError)

{-
   This allows to send JSONRPC requests to the `/rpc` endpoint and decode any errors.
-}

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


send :
    String
    -> String
    -> Json.Encode.Value
    -> (Result Http.Error a -> msg)
    -> Json.Decode.Decoder a
    -> Cmd msg
send endpoint method params result decoder =
    let
        json =
            Json.Encode.object
                [ ( "jsonrpc", Json.Encode.string "2.0" )
                , ( "id", Json.Encode.int 0 )
                , ( "method", Json.Encode.string method )
                , ( "params", params )
                ]

        request =
            { method = "POST"
            , url = endpoint
            , body = Http.jsonBody json
            , expect = Http.expectJson decoder
            , headers = []
            , timeout = Nothing
            , withCredentials = False
            }
    in
        request |> Http.request |> Http.send result


sendWithoutParams :
    String
    -> String
    -> (Result Http.Error a -> msg)
    -> Json.Decode.Decoder a
    -> Cmd msg
sendWithoutParams endpoint method result decoder =
    send endpoint method (Json.Encode.list []) result decoder


parseError : Http.Error -> Maybe String
parseError error =
    case error of
        Http.BadStatus response ->
            Just <|
                "Backend error "
                    ++ toString response.status.code
                    ++ response.status.message

        Http.BadPayload message response ->
            Just <| decodeError response.body

        error ->
            Just (toString error)


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
