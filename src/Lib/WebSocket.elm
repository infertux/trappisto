module Lib.WebSocket exposing (send, isSuccess, isMethod)

import WebSocket
import Json.Decode as Decode
import Json.Encode as Encode


send : String -> String -> List Encode.Value -> Cmd msg
send endpoint method params =
    let
        json =
            Encode.object
                [ ( "jsonrpc", Encode.string "1.0" )
                , ( "id", Encode.int 0 )
                , ( "method", Encode.string method )
                , ( "params", Encode.list params )
                ]
    in
        json
            |> Encode.encode 0
            |> WebSocket.send endpoint


isSuccess : String -> Bool
isSuccess jsonString =
    let
        decoder =
            Decode.maybe <| Decode.field "error" <| Decode.string

        result =
            Decode.decodeString decoder jsonString
    in
        case result of
            Err _ ->
                False

            Ok maybeError ->
                maybeError == Nothing


isMethod : List String -> String -> Bool
isMethod methods jsonString =
    let
        decoder =
            Decode.maybe <| Decode.field "method" <| Decode.string

        result =
            Decode.decodeString decoder jsonString
    in
        case result of
            Err _ ->
                False

            Ok maybeMethod ->
                List.any (\method -> Just method == maybeMethod) methods
