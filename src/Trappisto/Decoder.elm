module Trappisto.Decoder exposing (..)

import Keyboard
import Json.Decode as Decode
import Trappisto.Model exposing (Keys, BestBlock, BasicTransaction)


decodeGetBestBlock : Decode.Decoder BestBlock
decodeGetBestBlock =
    Decode.map2 BestBlock
        (Decode.at [ "result", "hash" ] Decode.string)
        (Decode.at [ "result", "height" ] Decode.int)


decodeTxAccepted : String -> Maybe BasicTransaction
decodeTxAccepted json =
    let
        decodeParams =
            Decode.map2 BasicTransaction
                (Decode.index 0 Decode.string)
                (Decode.index 1 Decode.float)
    in
        Decode.decodeString (Decode.field "params" decodeParams) json |> Result.toMaybe


decodeKeys : Bool -> Keyboard.KeyCode -> Keys -> Keys
decodeKeys bool keyCode keys =
    case keyCode of
        13 ->
            { keys | enter = bool }

        27 ->
            { keys | esc = bool }

        68 ->
            { keys | d = bool }

        73 ->
            { keys | i = bool }

        74 ->
            { keys | j = bool }

        75 ->
            { keys | k = bool }

        _ ->
            keys
