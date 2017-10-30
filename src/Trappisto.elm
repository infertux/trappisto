module Trappisto exposing (main)

import Html exposing (program)
import Trappisto.Model exposing (Model, Msg)
import Trappisto.Update exposing (init, update, subscriptions)
import Trappisto.View exposing (view)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
