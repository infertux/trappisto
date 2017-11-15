module Trappisto.Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Lib.HtmlAttributesExtra as HtmlAttributesExtra


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


formatAmount : Float -> Html msg
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


dlBuilder : List ( String, Maybe (Html msg) ) -> Html msg
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


queryLink : String -> String -> List (Attribute msg) -> Html msg
queryLink query label attributes =
    let
        hrefAttribute =
            href <| "javascript:query('" ++ query ++ "')"

        innerHtml =
            HtmlAttributesExtra.innerHtml label
    in
        a (List.concat [ attributes, [ hrefAttribute, innerHtml ] ]) []


dcrDataLink : String -> Html msg
dcrDataLink path =
    a
        [ class "btn btn-secondary float-right"
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
