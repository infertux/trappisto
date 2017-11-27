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
                [ div [ class "col-12 col-lg-6 offset-lg-3" ]
                    [ div
                        [ class "text-center alert alert-danger"
                        , style [ ( "overflow", "hidden" ) ]
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
                [ div [ class "col-4 col-lg-3" ] [ logo model ]
                , div [ class "col-4 col-lg-6" ]
                    [ input
                        [ id "query"
                        , name "query"
                        , class "form-control form-control-lg text-center mt-2 mb-2"
                        , placeholder
                            "Search for blocks, transactions, addresses, etc."
                        , onInput Query
                        , value model.query
                        ]
                        []
                    ]
                , div [ class "col-4 col-lg-3" ] [ statusView model ]
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
                    [ a [ href "javascript:query('')" ]
                        [ img
                            [ class
                                (if isFetching model then
                                    "img-fluid rotate"
                                 else
                                    "img-fluid"
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
                queryLink
                    model.lastBlockHash
                    (formatNumber model.lastBlockHeight)
                    []

        ( wsClass, wsStatus ) =
            if model.webSocketConnected then
                ( "success", "ON" )
            else
                ( "danger", "OFF" )
    in
        div [ class "text-center" ]
            [ span
                [ class "w-100 badge badge-dark" ]
                [ span [ class "oi oi-spreadsheet" ] []
                , span [] [ text " " ]
                , span [ class "d-none d-sm-inline" ] [ text "Last block: " ]
                , lastBlock
                ]
            , span
                [ class <| "w-100 badge badge-" ++ wsClass ]
                [ span [ class "oi oi-cloud-download" ] []
                , span [] [ text " " ]
                , span [ class "d-none d-sm-inline" ] [ text "Live updating: " ]
                , span [] [ text wsStatus ]
                ]
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
            if List.isEmpty model.lastTransactions then
                div [ class "row text-center mt-3 d-none d-sm-block" ]
                    [ div [ class "col" ]
                        [ pre [ id "logo", class "text-white" ]
                            [ text
                                " ______   _______  _______  _______  _______  ______  \n(  __  \\ (  ____ \\(  ____ \\(  ____ )(  ____ \\(  __  \\ \n| (  \\  )| (    \\/| (    \\/| (    )|| (    \\/| (  \\  )\n| |   ) || (__    | |      | (____)|| (__    | |   ) |\n| |   | ||  __)   | |      |     __)|  __)   | |   | |\n| |   ) || (      | |      | (\\ (   | (      | |   ) |\n| (__/  )| (____/\\| (____/\\| ) \\ \\__| (____/\\| (__/  )\n(______/ (_______/(_______/|/   \\__/(_______/(______/ \n"
                            ]
                        ]
                    ]
            else
                span [] []

        lastTransactions =
            if List.isEmpty model.lastTransactions then
                span [] []
            else
                div [ class "row text-center mt-3" ]
                    [ div [ class "col" ] <|
                        List.concat
                            [ [ hr [] []
                              , h3 []
                                    [ span [ class "badge badge-pill badge-info" ]
                                        [ text "Latest transactions" ]
                                    ]
                              ]
                            , List.map formatTransaction model.lastTransactions
                            ]
                    ]

        formatTransaction tx =
            let
                size =
                    toString (Basics.clamp 3 9 (logBase 10 tx.amount * 4)) ++ "rem"

                fontSize =
                    toString (Basics.clamp 1 3 (logBase 10 tx.amount * 1.5)) ++ "rem"

                amount =
                    toString (round tx.amount) ++ "<br>DCR"
            in
                div
                    [ class "badge badge-pill badge-info m-2"
                    , style
                        [ ( "line-height", fontSize )
                        , ( "font-size", fontSize )
                        , ( "height", size )
                        , ( "width", size )
                        ]
                    ]
                    [ queryLink tx.hash amount [] ]

        content =
            if model.error /= Nothing then
                [ searchView model, errorView model ]
            else
                case model.template of
                    Home ->
                        [ searchView model, errorView model, ascii, lastTransactions ]

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
                    [ class "col" ]
                    (if isFetching model then
                        [ searchView model ]
                     else
                        content
                    )
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
