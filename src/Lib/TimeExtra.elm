module Lib.TimeExtra exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import Html exposing (..)
import Html.Attributes exposing (..)
import Date.Distance
import Lib.DateExtra


toISOString : Time -> String
toISOString time =
    let
        date =
            Date.fromTime time

        hour =
            Date.hour date

        minute =
            Date.minute date

        second =
            Date.second date
    in
        (Lib.DateExtra.toISOString date)
            ++ " "
            ++ (String.padLeft 2 '0' (toString hour))
            ++ ":"
            ++ (String.padLeft 2 '0' (toString minute))
            ++ ":"
            ++ (String.padLeft 2 '0' (toString second))
            ++ " UTC"


timestampToTime : Int -> Time
timestampToTime int =
    Time.second * (toFloat int)


timeAgo : Time -> Time -> Html msg
timeAgo time now =
    let
        absolute =
            toISOString time

        relative =
            Date.Distance.inWords (Date.fromTime now) (Date.fromTime time) ++ " ago"
    in
        Html.time
            [ datetime absolute, title absolute ]
            [ text relative ]
