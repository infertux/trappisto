module Trappisto.Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type Coin
    = BCH
    | BTC
    | DCR


pluralize : Int -> String -> String
pluralize count singular =
    let
        phrase =
            toString count ++ " " ++ singular
    in
        if count == 1 then
            phrase
        else
            phrase ++ "s"


formatAmount : Float -> Html a
formatAmount float =
    let
        rounded =
            -- remove any floating point arithmetic errors
            float * 1.0e8 |> round |> toFloat |> (flip (/)) 1.0e8
    in
        span [ class "amount" ]
            [ text <| toString rounded ]


formatNumber : Int -> String
formatNumber int =
    let
        split digits =
            if String.length digits > 3 then
                digits
                    |> String.dropRight 3
                    |> split
                    |> (::) (String.right 3 digits)
            else
                [ digits ]
    in
        toString int |> split |> List.reverse |> String.join ","


shortHash : String -> String
shortHash hash =
    String.concat [ String.left 2 hash, "...", String.right 2 hash ]


dlBuilder : List ( String, Maybe (Html a) ) -> Html a
dlBuilder list =
    let
        filter ( label, value ) =
            case value of
                Nothing ->
                    Nothing

                Just value ->
                    Just <|
                        [ dt [ class "col-3 text-right" ] [ text label ]
                        , dd [ class "col-9" ] [ value ]
                        ]
    in
        dl [ class "row" ] (List.concat <| List.filterMap filter list)


dcrDataLink : String -> Html a
dcrDataLink path =
    a
        [ class "float-right"
        , target "_blank"
        , title "Open on dcrdata.org"
        , href <| "https://explorer.dcrdata.org/explorer/" ++ path
        ]
        [ span [ class "oi oi-external-link" ] [] ]


zeroesToNothing : Maybe String -> Maybe String
zeroesToNothing maybe =
    let
        zeroes =
            "0000000000000000000000000000000000000000000000000000000000000000"
    in
        case maybe of
            Just string ->
                if string == zeroes then
                    Nothing
                else
                    maybe

            Nothing ->
                Nothing
