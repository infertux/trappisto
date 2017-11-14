module Trappisto.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lib.HtmlAttributesExtra as HtmlAttributesExtra
import Trappisto.Model exposing (..)
import Components.Status as StatusComponent exposing (view)
import Components.Address as AddressComponent exposing (view)
import Components.Block as BlockComponent exposing (view)
import Components.Transaction as TransactionComponent exposing (view)


isFetching : Model -> Bool
isFetching model =
    model.statusModel.fetching
        || model.addressModel.fetching
        || model.blockModel.fetching
        || model.transactionModel.fetching


isError : Model -> Bool
isError model =
    getError model /= Nothing


getError : Model -> Maybe String
getError model =
    if model.error /= Nothing then
        model.error
    else
        case model.template of
            Transaction ->
                model.transactionModel.error

            Address ->
                model.addressModel.error

            Block ->
                model.blockModel.error

            Status ->
                model.statusModel.error


errorView : Model -> Html Msg
errorView model =
    case getError model of
        Nothing ->
            div [] []

        Just error ->
            div [ class "row" ]
                [ div [ class "col-6 offset-3" ]
                    [ div
                        [ class "text-center alert alert-danger"
                        , HtmlAttributesExtra.innerHtml error
                        ]
                        []
                    ]
                ]


searchView : Model -> Html Msg
searchView model =
    let
        search =
            div [ class "row" ]
                [ div [ class "col-8 offset-2" ]
                    [ input
                        [ id "query"
                        , name "query"
                        , class "form-control form-control-lg text-center mt-2 mb-4"
                        , placeholder
                            "Search for blocks, transactions, addresses, particles, etc."
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


addressView : Model -> Html Msg
addressView model =
    Html.map AddressMsg (AddressComponent.view model.addressModel)


blockView : Model -> Html Msg
blockView model =
    Html.map BlockMsg (BlockComponent.view model.blockModel)


transactionView : Model -> Html Msg
transactionView model =
    Html.map TransactionMsg (TransactionComponent.view model.transactionModel)


view : Model -> Html Msg
view model =
    let
        header =
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ if model.query /= "" then
                        span [] []
                      else
                        pre [ id "logo", class "text-white" ]
                            [ text
                                " ______   _______  _______  _______  _______  ______  \n(  __  \\ (  ____ \\(  ____ \\(  ____ )(  ____ \\(  __  \\ \n| (  \\  )| (    \\/| (    \\/| (    )|| (    \\/| (  \\  )\n| |   ) || (__    | |      | (____)|| (__    | |   ) |\n| |   | ||  __)   | |      |     __)|  __)   | |   | |\n| |   ) || (      | |      | (\\ (   | (      | |   ) |\n| (__/  )| (____/\\| (____/\\| ) \\ \\__| (____/\\| (__/  )\n(______/ (_______/(_______/|/   \\__/(_______/(______/ \n"
                            ]
                    ]
                ]
            ]

        status model =
            div [ class "row" ] [ statusView model ]

        content =
            if model.error /= Nothing then
                [ searchView model, errorView model ]
            else
                case model.template of
                    Status ->
                        if model.query == "particles" then
                            [ div [ class "row" ]
                                [ div [ class "col" ] [ text "Reticulating splines..." ]
                                ]
                            ]
                        else
                            [ searchView model, errorView model, status model ]

                    Address ->
                        if isError model then
                            [ searchView model, errorView model ]
                        else
                            [ searchView model, addressView model ]

                    Block ->
                        if isError model then
                            [ searchView model, errorView model ]
                        else
                            [ searchView model, blockView model ]

                    Transaction ->
                        if isError model then
                            [ searchView model, errorView model ]
                        else
                            [ searchView model, transactionView model ]

        debug =
            [ hr [] []
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ div
                        [ style
                            [ ( "font-size", "1rem" )
                            , ( "color", "white" )
                            , ( "text-align", "center" )
                            ]
                        ]
                        [ text <| toString model ]
                    ]
                ]
            ]

        headerAndContent model =
            [ div [ class "row text-center" ] [ div [ class "col" ] header ]
            , div [ class "row" ]
                [ div
                    [ class <|
                        "col"
                            ++ (if isFetching model then
                                    " fetching glow"
                                else
                                    ""
                               )
                    ]
                    content
                ]
            ]

        sections =
            if model.debug then
                List.concat [ headerAndContent model, debug ]
            else
                headerAndContent model
    in
        div [ class "row text-white" ] [ div [ class "col" ] sections ]
