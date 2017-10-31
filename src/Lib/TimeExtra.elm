module Lib.TimeExtra exposing (..)

import Date exposing (Date)
import Time exposing (Time)
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
