module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Json.Decode as Decode
import Components.Block as Block


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "decodeGetBlockHash" <|
                \() ->
                    Expect.equal (Ok "deadbeef") <| Decode.decodeString Block.decodeGetBlockHash "{\"result\":\"deadbeef\"}"
            ]
        , describe "Fuzz test examples, using randomly generated input"
            -- XXX: keping them as examples
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]
