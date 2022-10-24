module Button exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Model exposing (AnimationState(..), Menustate(..), Model, Msg(..), Pagetype(..))


renderGameButton :  Model -> Html Msg
renderGameButton model =
    List.range 1 3
            |> List.map
                (\i ->
                    let
                        animState =
                            Dict.get (String.fromInt i) model.animationState
                                |> Maybe.withDefault None
                    in
                    div
                        []
                        [ p [] []
                        , button
                            [ Attr.style "width" "240px"
                            , Attr.style "border" "3px solid #25c4c9"
                            , Attr.style "border-radius" "6px"
                            , Attr.style "font-size" "32px"
                            , Attr.classList
                                [ ( "animated", animState /= None )
                                , ( "pulse", animState /= None )
                                , ( "infinite", animState /= None )
                                ]
                            , onMouseEnter <| UserHoveredButton <| String.fromInt i
                            , onMouseLeave <| UserUnhoveredButton <| String.fromInt i
                            , case i of
                                    1 ->
                                        onClick (Page Mapall)
                                    2 ->
                                        onClick (Page Levelall)
                                    3 ->
                                        onClick (Page Help)
                                    _ ->
                                        onClick Noop
                            ]
                            [ case i of
                                    1 ->
                                        text "Story Mode"
                                    2 ->
                                        text "Endless Mode"
                                    3 ->
                                        text "Help"
                                    _ ->
                                        text ""
                            ]
                        ]
                )
            |> div
                [ Attr.style "width" "240px"
                , Attr.style "margin" (String.fromFloat (0.65 * (Tuple.second model.size)) ++ "px auto 0 auto")
                , Attr.style "text-align" "center"
                ]


renderMapbutton1 : Model -> Html Msg
renderMapbutton1 model =
    let
        animState =
            Dict.get (String.fromInt 4) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "8%"
        , Attr.style "top" "54%"
        , Attr.style "width" "20%"
        , Attr.style "height" "38%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/earthmb.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "white"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 4
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 4
            , onClick (Page BE1)
            ]
            [ text "Map1" ]
        ]



renderMapbutton2 : Model -> Html Msg
renderMapbutton2 model =
    let
        animState =
            Dict.get (String.fromInt 5) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "24.5%"
        , Attr.style "top" "17%"
        , Attr.style "width" "22%"
        , Attr.style "height" "53%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/hcmb.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "white"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 5
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 5
            , onClick (Page Map2)
            ]
            [ text "Map2" ]
        ]

renderMapbutton3 : Model -> Html Msg
renderMapbutton3 model =
    let
        animState =
            Dict.get (String.fromInt 6) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "53%"
        , Attr.style "top" "42%"
        , Attr.style "width" "19%"
        , Attr.style "height" "40%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/lcmb.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "white"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 6
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 6
            , onClick (Page Map3)
            ]
            [ text "Map3" ]
        ]

renderMapbutton4 : Model -> Html Msg
renderMapbutton4 model =
    let
        animState =
            Dict.get (String.fromInt 7) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "73%"
        , Attr.style "top" "16%"
        , Attr.style "width" "17%"
        , Attr.style "height" "38%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/habmb.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "white"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 7
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 7
            , onClick (Page Map4)
            ]
            [ text "Map4" ]
        ]

renderExitbutton : Model -> Html Msg
renderExitbutton model =
    let
        animState =
            Dict.get (String.fromInt 8) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , if model.menustate == MenuOut || model.page == Ending4p1 || model.page == Ending3p2 || model.page == Ending2p3 || model.page == Ending1p4 then
            Attr.style "left" "20%"
          else
            Attr.style "left" "42.5%"
        , Attr.style "top" "82%"
        , if model.menustate == MenuOut then
            Attr.style "width" "20%"
          {-else if model.page == Ending4p1 || model.page == Ending3p2 || model.page == Ending2p3 || model.page == Ending1p4 then
            Attr.style "width" "10%"-}
          else
            Attr.style "width" "15%"
        , Attr.style "height" "18%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "border" "3px solid #25c4c9"
            , Attr.style "border-radius" "6px"
            , Attr.style "font-size" "30px"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 8
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 8
            , onClick (Reset Cover)
            ]
            [ text "Exit" ]
        ]

renderRestartbutton : Pagetype -> Model -> Html Msg
renderRestartbutton pagetype model =
    let
        animState =
            Dict.get (String.fromInt 9) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "60%"
        , Attr.style "top" "82%"
        , if model.page == Ending3p2 || model.page == Ending2p3 || model.page == Ending1p4 then
            Attr.style "width" "15%"
          else
            Attr.style "width" "20%"
        , Attr.style "height" "18%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "border" "3px solid #25c4c9"
            , Attr.style "border-radius" "6px"
            , Attr.style "font-size" "30px"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 9
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 9
            , if model.page == Ending3p2 then
                onClick (Reset Map2)
              else if model.page == Ending2p3 then
                onClick (Reset Map3)
              else if model.page == Ending1p4 then
                onClick (Reset model.death)
              else
                onClick (Reset model.page)
            ]
            [ text "Restart" ]
        ]

renderInfbutton1 : Model -> Html Msg
renderInfbutton1 model =
    let
        animState =
            Dict.get (String.fromInt 10) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "13%"
        , Attr.style "top" "30%"
        , Attr.style "width" "18%"
        , Attr.style "height" "38%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/planet1.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "black"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 10
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 10
            , onClick (Page Inf1)
            ]
            [ text "Level1" ]
        ]

renderInfbutton2 : Model -> Html Msg
renderInfbutton2 model =
    let
        animState =
            Dict.get (String.fromInt 11) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "41%"
        , Attr.style "top" "30%"
        , Attr.style "width" "18%"
        , Attr.style "height" "38%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/planet2.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "black"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 11
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 11
            , onClick (Page Inf2)
            ]
            [ text "Level2" ]
        ]

renderInfbutton3 : Model -> Html Msg
renderInfbutton3 model =
    let
        animState =
            Dict.get (String.fromInt 12) model.animationState
                |> Maybe.withDefault None
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "69%"
        , Attr.style "top" "30%"
        , Attr.style "width" "18%"
        , Attr.style "height" "38%"
        ]
        [ button
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "border" "0px"
            , Attr.style "background" "url(assets/planet3.png) 0% 0% / 100% 100%"
            , Attr.style "font-size" "32px"
            , Attr.style "color" "black"
            , Attr.classList
                [ ( "animated", animState /= None )
                , ( "pulse", animState /= None )
                , ( "infinite", animState /= None )
                ]
            , onMouseEnter <| UserHoveredButton <| String.fromInt 12
            , onMouseLeave <| UserUnhoveredButton <| String.fromInt 12
            , onClick (Page Inf3)
            ]
            [ text "Level3" ]
        ]

renderPausebutton : Model -> Html Msg
renderPausebutton model =
    let
        animState =
            Dict.get (String.fromInt 13) model.animationState
                |> Maybe.withDefault None
    in
        div [ Attr.style "position" "fixed"
            , Attr.style "right" "365px"
            , Attr.style "top" "22%"
            , Attr.style "width" "3%"
            , Attr.style "height" "6%"
            , Attr.classList
                [ ("animated", True)
                , ("fadeIn delay-3s", True)
                ]
            ]
            [ button
                [ Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "border" "0px"
                , Attr.style "background" "url(assets/resume.png) 0% 0% / 100% 100%"
                , Attr.classList
                    [ ( "animated", animState /= None )
                    , ( "pulse", animState /= None )
                    , ( "infinite", animState /= None )
                    ]
                , onMouseEnter <| UserHoveredButton <| String.fromInt 13
                , onMouseLeave <| UserUnhoveredButton <| String.fromInt 13
                , onClick Pause
                ]
                [text ""]
            ]


renderResumebutton : Model -> Html Msg
renderResumebutton model =
    let
        animState =
            Dict.get (String.fromInt 14) model.animationState
                |> Maybe.withDefault None
    in
        div [ Attr.style "position" "fixed"
            , Attr.style "right" "305px"
            , Attr.style "top" "22%"
            , Attr.style "width" "3%"
            , Attr.style "height" "6%"
            , Attr.classList
                [ ("animated", True)
                , ("fadeIn delay-3s", True)
                ]
            ]
            [ button
                [ Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "border" "0px"
                , Attr.style "background" "url(assets/pause.png) 0% 0% / 100% 100%"
                , Attr.classList
                    [ ( "animated", animState /= None )
                    , ( "pulse", animState /= None )
                    , ( "infinite", animState /= None )
                    ]
                , onMouseEnter <| UserHoveredButton <| String.fromInt 14
                , onMouseLeave <| UserUnhoveredButton <| String.fromInt 14
                , onClick Resume
                ]
                [text ""]
            ]