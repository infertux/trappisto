module Trappisto.Helpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


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


dcrDataLink : String -> Html a
dcrDataLink path =
    a
        [ class "float-right"
        , target "_blank"
        , title "Open on dcrdata.org"
        , href <| "https://explorer.dcrdata.org/explorer/" ++ path
        ]
        [ span [ class "oi oi-external-link" ] [] ]
