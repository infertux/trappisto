module Components.BlockList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List
import Http exposing (Error)
import Json.Decode as Json
import Lib.JsonApiExtra as JsonApiExtra
import Components.Block as Block


type alias Model =
    { blocks : List Block.Model
    , filter : String
    , nextCount : Int
    , fetching : Bool
    , filterCount : Int
    , totalCount : Int
    }


type alias JsonModel =
    { data : List Block.Model
    , filterCount : Int
    , totalCount : Int
    }


type Msg
    = Fetch
    | FetchResult (Result Http.Error JsonModel)
    | Search String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( { model | fetching = True }, fetchblocks model )

        FetchResult (Ok jsonModel) ->
            let
                blockList =
                    jsonModel.data
            in
                ( { model
                    | blocks = blockList
                    , nextCount = model.nextCount * 2
                    , fetching = False
                    , filterCount = jsonModel.filterCount
                    , totalCount = jsonModel.totalCount
                  }
                , Cmd.none
                )

        FetchResult (Err _) ->
            ( { model | fetching = False }, Cmd.none )

        Search filter ->
            let
                updatedModel =
                    { model | filter = filter }
            in
                ( updatedModel, fetchblocks updatedModel )


fetchblocks : Model -> Cmd Msg
fetchblocks model =
    let
        url =
            "/api/blocks?count="
                ++ (toString model.nextCount)
                ++ "&filter="
                ++ model.filter
    in
        JsonApiExtra.post "getinfo" "PARAMS" FetchResult decodeblockFetch


decodeblockFetch : Json.Decoder JsonModel
decodeblockFetch =
    Json.map3 JsonModel
        (Json.field "data" (Json.list decodeblockData))
        (Json.at [ "meta", "filter-count" ] Json.int)
        (Json.at [ "meta", "total-count" ] Json.int)


decodeblockData : Json.Decoder Block.Model
decodeblockData =
    Json.map2 Block.Model
        (Json.at [ "attributes", "height" ] Json.int)
        (Json.at [ "attributes", "hash" ] Json.string)


initialCount : Int
initialCount =
    25


initialModel : Model
initialModel =
    { blocks = []
    , filter = ""
    , nextCount = initialCount
    , fetching = True
    , filterCount = 0
    , totalCount = 0
    }


view : Model -> Html Msg
view model =
    div [ class "block-list" ]
        [ searchBar model
        , renderBlocks model
        ]


renderBlocks : Model -> Html Msg
renderBlocks model =
    let
        length =
            List.length (model.blocks)

        moreLinkWrapper =
            if length == 0 || length == model.nextCount then
                -- FIXME
                [ moreLink model.fetching ]
            else
                []

        rows =
            List.map (\block -> Block.view block) model.blocks
    in
        div [ class "row mt-3" ]
            [ div [ class "col-xs" ]
                [ table [ class "table table-striped table-sm" ]
                    [ thead [] [ newLink ]
                    , tbody [] rows
                    , tfoot [] moreLinkWrapper
                    ]
                ]
            ]


searchBar : Model -> Html Msg
searchBar model =
    div [ class "row flex-items-xs-middle" ]
        [ div [ class "col-xs-3" ] []
        , div [ class "col-xs-6" ]
            [ input [ name "filter", placeholder "Search blocks", class "search form-control", onInput Search, value model.filter ] [] ]
        , div [ class "col-xs-3" ]
            [ small [ class "text-muted" ]
                [ text
                    ((toString model.filterCount)
                        ++ " of "
                        ++ (toString model.totalCount)
                        ++ " matches"
                    )
                ]
            ]
        ]


newLink : Html Msg
newLink =
    tr []
        [ td [ colspan 5 ]
            [ a [ href "/blocks/new", class "btn btn-outline-warning btn-block" ] [ text "Create a new block" ] ]
        ]


moreLink : Bool -> Html Msg
moreLink fetching =
    let
        tag =
            if fetching then
                i [ class "fa fa-spinner fa-pulse fa-3x fa-fw" ] []
            else
                a [ href "javascript:void(0)", onClick Fetch ] [ text "more" ]
    in
        tr []
            [ td [ colspan 5, class "text-center" ] [ tag ] ]
