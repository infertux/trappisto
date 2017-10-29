port module BlockListApp exposing (main)

import Html exposing (Html, div)
import Components.BlockList as BlockList


type alias Model =
    { blockListModel : BlockList.Model }


init : ( Model, Cmd Msg )
init =
    ( { blockListModel = BlockList.initialModel }, Cmd.none )


type Msg
    = BlockListMsg BlockList.Msg
    | JsMsg (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BlockListMsg blockListMsg ->
            let
                ( updatedModel, cmd ) =
                    BlockList.update blockListMsg model.blockListModel
            in
                ( { model | blockListModel = updatedModel }, Cmd.map BlockListMsg cmd )

        JsMsg [ "BlockList.Fetch" ] ->
            let
                ( updatedModel, cmd ) =
                    BlockList.update BlockList.Fetch model.blockListModel
            in
                ( { model | blockListModel = updatedModel }, Cmd.map BlockListMsg cmd )

        JsMsg [ "BlockList.Search", query ] ->
            let
                ( updatedModel, cmd ) =
                    BlockList.update (BlockList.Search query) model.blockListModel
            in
                ( { model | blockListModel = updatedModel }, Cmd.map BlockListMsg cmd )

        JsMsg _ ->
            ( model, Cmd.none )


blockListView : Model -> Html Msg
blockListView model =
    Html.map BlockListMsg (BlockList.view model.blockListModel)


view : Model -> Html Msg
view model =
    div [] [ blockListView model ]


subscriptions : Model -> Sub Msg
subscriptions model =
    jsEvents JsMsg



-- port for listening for events from JavaScript


port jsEvents : (List String -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
