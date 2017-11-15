module Lib.WebSocket exposing (send, isSuccess)

import WebSocket
import Json.Encode


send : String -> String -> List Json.Encode.Value -> Cmd msg
send endpoint method params =
    let
        json =
            Json.Encode.object
                [ ( "jsonrpc", Json.Encode.string "2.0" )
                , ( "id", Json.Encode.int 0 )
                , ( "method", Json.Encode.string method )
                , ( "params", Json.Encode.list params )
                ]
    in
        json
            |> Json.Encode.encode 0
            |> WebSocket.send endpoint


isSuccess : String -> Bool
isSuccess jsonString =
    jsonString == "{\"result\":null,\"error\":null,\"id\":0}"



-- decode : String -> Json.Decode.Decoder Notification
-- decode jsonString =
--     Json.Decode.map2 JsonModel
--         (Json.Decode.at [ "result", "blocks" ] Json.Decode.int)
--         (Json.Decode.at [ "result", "connections" ] Json.Decode.int)
