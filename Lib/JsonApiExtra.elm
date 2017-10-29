module Lib.JsonApiExtra exposing (post)

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
