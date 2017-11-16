module Trappisto.Config exposing (..)


type Coin
    = BCH
    | BTC
    | DCR


type alias Config =
    { coin : Coin
    , rpcEndpoint : String
    , wsEndpoint : String
    }
