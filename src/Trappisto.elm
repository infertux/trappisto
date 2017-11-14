module Trappisto exposing (main)

import Navigation
import Trappisto.Model
import Trappisto.Update exposing (init, update, subscriptions)
import Trappisto.View exposing (view)


main : Program Trappisto.Model.Flags Trappisto.Model.Model Trappisto.Model.Msg
main =
    Navigation.programWithFlags Trappisto.Model.NewUrl
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
