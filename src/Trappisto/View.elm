module Trappisto.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Lib.TimeExtra as TimeExtra
import Trappisto.Model exposing (..)
import Components.Status as StatusComponent exposing (view)
import Components.Block as BlockComponent exposing (view)


isFetching : Model -> Bool
isFetching model =
    model.statusModel.fetching || model.blockModel.fetching


isError : Model -> Bool
isError model =
    getError model /= Nothing


getError : Model -> Maybe String
getError model =
    if model.statusModel.error /= Nothing then
        model.statusModel.error
    else if model.blockModel.error /= Nothing then
        model.blockModel.error
    else
        Nothing


errorView : Model -> Html Msg
errorView model =
    case getError model of
        Nothing ->
            div [] []

        Just error ->
            div [ class "row" ]
                [ div [ class "col-6 offset-3" ]
                    [ div [ class "text-center alert alert-danger" ]
                        [ text error ]
                    ]
                ]


searchView : Model -> Html Msg
searchView model =
    let
        search =
            div [ class "row" ]
                [ div [ class "col-6 offset-3" ]
                    [ input
                        [ id "query"
                        , name "query"
                        , placeholder "Search for blocks, transactions, addresses, particles, etc."
                        , class "form-control form-control-lg text-center mt-2 mb-4"
                        , onInput Query
                        , value model.query
                        ]
                        []
                    ]
                ]

        vim =
            div [ class "row" ]
                [ div [ class "col-6 offset-3" ]
                    [ div [ class "alert alert-info text-center text-uppercase" ]
                        [ text "Vim mode engaged! Press \"i\" to go back to Insert mode." ]
                    ]
                ]

        view =
            if model.vimMode then
                [ search, vim ]
            else
                [ search ]
    in
        div [ class "row" ] [ div [ class "col" ] view ]


statusView : Model -> Html Msg
statusView model =
    Html.map StatusMsg (StatusComponent.view model.statusModel)


blockView : Model -> Html Msg
blockView model =
    Html.map BlockMsg (BlockComponent.view model.blockModel)


view : Model -> Html Msg
view model =
    let
        window =
            model.window

        glow =
            if isFetching model then
                "glow"
            else
                ""

        header =
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ pre [ id "logo", class glow ]
                        [ text <|
                            if model.query /= "" then
                                "Trappisto - Decred Interactive Block Explorer"
                            else
                                " ______   _______  _______  _______  _______  ______  \n(  __  \\ (  ____ \\(  ____ \\(  ____ )(  ____ \\(  __  \\ \n| (  \\  )| (    \\/| (    \\/| (    )|| (    \\/| (  \\  )\n| |   ) || (__    | |      | (____)|| (__    | |   ) |\n| |   | ||  __)   | |      |     __)|  __)   | |   | |\n| |   ) || (      | |      | (\\ (   | (      | |   ) |\n| (__/  )| (____/\\| (____/\\| ) \\ \\__| (____/\\| (__/  )\n(______/ (_______/(_______/|/   \\__/(_______/(______/ \n"
                        ]
                    ]
                ]
            ]

        status model =
            div [ class "row" ]
                [ div [ class "col" ] [ text <| TimeExtra.toISOString model.time ]
                , statusView model
                ]

        block model =
            div [ class "row" ]
                [ div
                    [ class "col-6 offset-3" ]
                    [ blockView model ]
                ]

        content =
            case model.template of
                Status ->
                    if model.query == "particles" then
                        [ div [ class "row" ]
                            [ div [ class "col" ] [ text "Reticulating splines..." ]
                            ]
                        ]
                    else
                        [ searchView model
                        , errorView model
                        , status model
                        ]

                Block ->
                    if isError model then
                        [ searchView model, errorView model ]
                    else
                        [ searchView model, block model ]

                _ ->
                    Debug.crash <| toString model.template

        debug =
            [ div
                [ style
                    [ ( "font-size", "1rem" )
                    , ( "color", "white" )
                    , ( "text-align", "center" )
                    ]
                ]
                [ text <| toString model ]
            ]
    in
        div [ class "row text-white" ]
            [ div [ class "col" ]
                [ div [ class "row text-center" ] [ div [ class "col" ] header ]
                , div [ class "row" ] [ div [ class "col" ] content ]
                , hr [] []
                , div [ class "row" ] [ div [ class "col" ] debug ]
                ]
            ]
