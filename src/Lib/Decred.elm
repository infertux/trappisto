module Lib.Decred exposing (..)


dcrAmount : Float -> String
dcrAmount float =
    let
        rounded =
            -- remove any floating point arithmetic errors
            float * 1.0e8 |> round |> toFloat |> (flip (/)) 1.0e8
    in
        (toString rounded) ++ " DCR"


shortHash : String -> String
shortHash hash =
    String.concat [ String.left 2 hash, "...", String.right 2 hash ]
