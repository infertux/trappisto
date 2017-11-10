module Trappisto exposing (main)

import Navigation
import Trappisto.Model
import Trappisto.Update exposing (init, update, subscriptions)
import Trappisto.View exposing (view)


main : Program Trappisto.Model.Config Trappisto.Model.Model Trappisto.Model.Msg
main =
    Navigation.programWithFlags Trappisto.Model.NewUrl
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
