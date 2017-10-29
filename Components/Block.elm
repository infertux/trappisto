module Components.Block exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Lib.HtmlAttributesExtra exposing (innerHtml)


type alias Model =
    { height : Int
    , hash : String
    }


emptyModel : Model
emptyModel =
    { height = 0
    , hash = ""
    }


view : Model -> Html a
view model =
    tr []
        [ td [ class "align-middle" ] [ input [ type_ "checkbox" ] [] ]
        , td [ title "Height" ] [ text <| toString model.height ]
        , td [ class "text-right" ] [ text model.hash ]
        ]
