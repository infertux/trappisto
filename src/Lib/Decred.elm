module Lib.Decred exposing (..)


shortHash : String -> String
shortHash hash =
    String.concat [ String.left 2 hash, "...", String.right 2 hash ]
