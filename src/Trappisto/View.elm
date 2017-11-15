module Trappisto.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lib.HtmlAttributesExtra as HtmlAttributesExtra
import Trappisto.Model exposing (..)
import Trappisto.Helpers exposing (..)
import Components.Address as AddressComponent exposing (view)
import Components.Block as BlockComponent exposing (view)
import Components.Transaction as TransactionComponent exposing (view)


isFetching : Model -> Bool
isFetching model =
    model.fetching
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

            Home ->
                model.error


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
        search model =
            div [ class "row align-items-center" ]
                [ div [ class "col-2" ] [ logo model ]
                , div [ class "col-8" ]
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
                , div [ class "col-2" ] [ statusView model ]
                ]

        vim =
            div [ class "row" ]
                [ div [ class "col-6 offset-3" ]
                    [ div [ class "alert alert-info text-center text-uppercase" ]
                        [ text "Vim mode engaged! Press \"i\" to go back to Insert mode." ]
                    ]
                ]

        logo model =
            div [ class "row" ]
                [ div
                    [ class "col text-center" ]
                    [ a [ href "/" ]
                        [ img
                            [ class
                                (if isFetching model then
                                    "rotate"
                                 else
                                    ""
                                )
                            , src "assets/images/decred.png"
                            , alt "logo"
                            ]
                            []
                        ]
                    ]
                ]

        view =
            if model.vimMode then
                [ search model, vim ]
            else
                [ search model ]
    in
        div [ class "row" ] [ div [ class "col" ] view ]


statusView : Model -> Html Msg
statusView model =
    let
        lastBlock =
            if model.lastBlockHeight < 0 then
                span [] [ text "??????" ]
            else
                queryLink model.lastBlockHash (toString model.lastBlockHeight) []

        ( wsClass, wsStatus ) =
            if model.webSocketConnected then
                ( "success", "ON" )
            else
                ( "danger", "OFF" )
    in
        div [ class "text-center" ]
            [ span
                [ class "badge badge-dark" ]
                [ h5 []
                    [ span [] [ text "Last block:" ]
                    , br [] []
                    , lastBlock
                    ]
                ]
            , span
                [ class <| "badge badge-pill badge-" ++ wsClass ]
                [ text <| "Live updating: " ++ wsStatus ]
            ]


addressView : Model -> Html Msg
addressView model =
    Html.map AddressMsg (AddressComponent.view model.addressModel model.now)


blockView : Model -> Html Msg
blockView model =
    Html.map BlockMsg (BlockComponent.view model.blockModel model.now)


transactionView : Model -> Html Msg
transactionView model =
    Html.map TransactionMsg (TransactionComponent.view model.transactionModel model.now)


view : Model -> Html Msg
view model =
    let
        ascii =
            div [ class "row text-center mt-3" ]
                [ div [ class "col" ]
                    [ pre [ id "logo", class "text-white" ]
                        [ text
                            " ______   _______  _______  _______  _______  ______  \n(  __  \\ (  ____ \\(  ____ \\(  ____ )(  ____ \\(  __  \\ \n| (  \\  )| (    \\/| (    \\/| (    )|| (    \\/| (  \\  )\n| |   ) || (__    | |      | (____)|| (__    | |   ) |\n| |   | ||  __)   | |      |     __)|  __)   | |   | |\n| |   ) || (      | |      | (\\ (   | (      | |   ) |\n| (__/  )| (____/\\| (____/\\| ) \\ \\__| (____/\\| (__/  )\n(______/ (_______/(_______/|/   \\__/(_______/(______/ \n"
                        ]
                    ]
                ]

        content =
            if model.error /= Nothing then
                [ searchView model, errorView model ]
            else
                case model.template of
                    Home ->
                        case model.query of
                            "" ->
                                [ searchView model, errorView model, ascii ]

                            "particles" ->
                                [ div [ class "row" ]
                                    [ div [ class "col" ] [ text "Reticulating splines..." ]
                                    ]
                                ]

                            _ ->
                                [ searchView model, errorView model ]

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

        wrapper model =
            [ div [ class "row" ]
                [ div
                    [ class <|
                        "col"
                            ++ (if isFetching model then
                                    " fetching"
                                else
                                    ""
                               )
                    ]
                    content
                ]
            ]

        sections =
            if model.debug then
                List.concat [ wrapper model, debug ]
            else
                wrapper model

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
    in
        div [ class "row text-white" ] [ div [ class "col" ] sections ]
