module Trappisto.Helpers exposing (..)

{-
   Generic helpers to help with formatting and creating views. Those should not
   be specific to Trappisto and could be reused in other projects. If multiple
   helpers fit the same "category", they should be extracted into a library and
   moved to src/Lib.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Regex
import Lib.HtmlAttributesExtra as HtmlAttributesExtra


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
        span [ class "amount" ] [ text <| formatNumber rounded ]


formatNumber : number -> String
formatNumber number =
    let
        string =
            toString number

        regex =
            if String.contains "." string then
                "(\\d)(?=(\\d{3})+(?!\\d)\\.)"
            else
                "(\\d)(?=(\\d{3})+(?!\\d))"
    in
        Regex.replace
            Regex.All
            (Regex.regex regex)
            (\{ match } -> match ++ ",")
            string


shortAddress : String -> String
shortAddress address =
    String.concat [ String.left 4 address, "...", String.right 4 address ]


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
                        [ dt [ class "col-6 col-sm-4 col-md-3 text-right" ] [ text label ]
                        , dd [ class "col-6 col-sm-8 col-md-9" ] [ value ]
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
        [ class "btn btn-secondary"
        , target "_blank"
        , title "Open on dcrdata.org"
        , href <| "https://explorer.dcrdata.org/explorer/" ++ path
        ]
        [ span [ class "oi oi-external-link" ] [] ]


zeroToNothing : Maybe Int -> Maybe Int
zeroToNothing maybe =
    case maybe of
        Just int ->
            if int == 0 then
                Nothing
            else
                maybe

        Nothing ->
            Nothing


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
