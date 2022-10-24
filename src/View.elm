module View exposing (..)


import Button exposing (renderExitbutton, renderGameButton, renderInfbutton1, renderInfbutton2, renderInfbutton3, renderMapbutton1, renderMapbutton2, renderMapbutton3, renderMapbutton4, renderPausebutton, renderRestartbutton, renderResumebutton)
import Dict
import Game exposing (renderAttributes, renderBackground, renderBorder, renderCharacters, renderCraft, renderFire, renderGrad, renderLine, renderMapbox, renderPlanet, renderSmallmap, renderSpecial, renderTail, renderWave, rendercorePlanet, renderBulletTail, renderBullets)
import Game exposing (renderAttributes, renderBackground, renderCharacters, renderCraft, renderFire, renderLine, renderMapbox, renderPlanet, renderScore, renderSmallmap, renderSpecial, renderTail, rendercorePlanet)
import HighCivil exposing (checkEndMap2, checkHigh1, checkHigh2)
import Html exposing (Html, br, button, div, hr, p, pre, text)
import Html.Attributes as Attr exposing (attribute)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Level4 exposing (checkHab3)
import LowCivil exposing (checkEndMap3, checkLow1, checkLow2)
import PauseInterface exposing (renderPaused)
import Chatbox exposing (renderChatbox)
import Story exposing (renderBE1, renderBE2, renderBE3, renderEnding1p2, renderEnding1p3, renderEnding1p4, renderEnding2p1, renderEnding2p2, renderEnding2p3, renderEnding3p1, renderEnding3p2, renderEnding4p1, renderHighFirst, renderHighSecond, renderLowFirst, renderLowSecond, renderStory1, renderStory2to3, renderStory3to4)
import Level1 exposing (checkEndMap1)
import Bgm exposing (renderBgm)
import Svg exposing (Svg, circle, ellipse, g, image, line, rect, svg, text_)
import Svg.Attributes exposing (cx, cy, fill, fontSize, fontStyle, height, opacity, preserveAspectRatio, r, rx, ry, stroke, strokeDasharray, strokeWidth, transform, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)
import Model exposing (AnimationState(..), Boxstate(..), Gamestate(..), Menustate(..), Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..))
import Time

renderCover : Model -> Html Msg
renderCover model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "0"
        , Attr.style "top" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background" "black"
        ]
        [ div [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "black"
            , Attr.classList
                [ ("animated", True)
                , ("fadeIn", True)
                ]
                ]
                [ div [ Attr.style "position" "fixed"
                    , Attr.style "left" "0"
                    , Attr.style "top" "0"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "background" "url(assets/title_1.svg) 0% 0% / 100% 100%"
                    ]
                    [ renderGameButton model
                    --, renderBgm model
                    ]
                ]
        ]



renderBegin : Model -> Html Msg
renderBegin model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "0"
        , Attr.style "top" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background" "black"
        ]
        [div [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "black"
            , Attr.classList
                [ ( "animated", True )
                , ( "fadeIn",  True )
                , ( "fadeOut", model.millisclock >= 3 )
                ]
            , on "animationend" <| Decode.succeed <| BGM
            , if model.millisclock > 3 then
                  on "animationend" <| Decode.succeed <| Page Cover

              else
                  onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "42%"
                , Attr.style "top" "38%"
                , Attr.style "width" "260px"
                , Attr.style "height" "260px"
                , Attr.style "background" "url(assets/logo.png) 0% 0% / 100% 100%"
                ]
                [ --renderBgm model
                ]
            ]
        ]


renderHelp : Model -> Html Msg
renderHelp model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "0"
        , Attr.style "top" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background" "black"
        ]
        [ div [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "black"
            , Attr.classList
                [ ("animated", True)
                , ("fadeIn", True)
                ]
                ]
                [ div [ Attr.style "position" "fixed"
                    , Attr.style "left" "0"
                    , Attr.style "top" "0"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                    ]
                    [ renderExitbutton model
                    ,div [ Attr.style "font-size" "35px"
                         , Attr.style "text-align" "center"
                         , Attr.style "color" "white"
                         ]
                         [ br [] []
                         , text "How to Play"
                         ]

                    ,div [ Attr.style "font-size" "26px"
                          , Attr.style "text-align" "center"
                          , Attr.style "color" "white"
                          ]
                          [ br [] []
                          , text "[Press ←,→,↑,↓] to activate the thruster."
                          , br [] []
                          , br [] []
                          , text "[Press S] to hang the spacecraft to the white hole and circle around it."
                          , br [] []
                          , br [] []
                          , text "[Press D] to escape the white hole in the tangent direction with extra speed."
                          , br [] []
                          , br [] []
                          , text "[Press A] to escape the current white hole and hang onto another."
                          , br [] []
                          , br [] []
                          , text " can offer you extra fuel."
                          , br [] []
                          , br [] []
                          , text " can heal the spacecraft."
                          ]
                    ,div [ Attr.style "font-size" "35px"
                         , Attr.style "text-align" "center"
                         , Attr.style "color" "white"
                         ]
                         [ br [] []
                         , text "You will see more in the first level (Tutorial Level)."
                         ]
                    , div [ Attr.style "position" "fixed"
                          , Attr.style "top" "15%"
                          , Attr.style "left" "80%"
                          , Attr.style "width" "300px"
                          , Attr.style "height" "275px"
                          , Attr.style "background" "url(assets/help.png) 0% 0% / 100% 100%"
                          ]
                          []
                    , div [ Attr.style "position" "fixed"
                          , Attr.style "top" "51%"
                          , Attr.style "left" "35%"
                          , Attr.style "width" "52px"
                          , Attr.style "height" "50px"
                          , Attr.style "background" "url(assets/addfuel.png) 0% 0% / 100% 100%"
                          ]
                          []
                    , div [ Attr.style "position" "fixed"
                          , Attr.style "top" "61%"
                          , Attr.style "left" "35%"
                          , Attr.style "width" "52px"
                          , Attr.style "height" "50px"
                          , Attr.style "background" "url(assets/repair.png) 0% 0% / 100% 100%"
                          ]
                          [ --renderBgm model]
                     ]
                ]
            ]
        ]

renderChooseMap : Model -> Html Msg
renderChooseMap model =
        div [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "right" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "white"
            ]
              [ renderLoadingPage model
              , if model.millisclock >= 1 then
                   div []
                      [div [ Attr.style "position" "fixed"
                          , Attr.style "left" "0"
                          , Attr.style "right" "0"
                          , Attr.style "width" "100%"
                          , Attr.style "height" "100%"
                          , Attr.style "background" "url(assets/vg100p2.jpg) 0% 0% / 100% 100%"
                          , Attr.style "opacity" "0.7"
                          ]
                          []
                      , renderMapbutton1 model
                      , renderMapbutton2 model
                      , renderMapbutton3 model
                      , renderMapbutton4 model
                      , renderExitbutton model
                      ]
                else
                    div [] []
              , if model.millisclock <= 3 then
                    renderLoadingPage model
                else
                    div [] []
              --, renderBgm model
              ]




renderChooseLevel : Model -> Html Msg
renderChooseLevel model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background" "white"
        ]
        [ renderLoadingPage model
        , if model.millisclock >= 1 then
               div []
                  [div [ Attr.style "position" "fixed"
                      , Attr.style "left" "0"
                      , Attr.style "right" "0"
                      , Attr.style "width" "100%"
                      , Attr.style "height" "100%"
                      , Attr.style "background" "url(assets/vg100p2.jpg) 0% 0% / 100% 100%"
                      , Attr.style "opacity" "0.7"
                      ]
                      []
                  , renderExitbutton model
                  , renderInfbutton1 model
                  , renderInfbutton2 model
                  , renderInfbutton3 model
                  ]
                else
                    div [] []
              , if model.millisclock <= 3 then
                    renderLoadingPage model
                else
                    div [] []
              --, renderBgm model
              ]

renderLoadingPage : Model -> Html Msg
renderLoadingPage model =
    if model.page == Map1 || model.page == Map2 || model.page == Map3 || model.page == Map4 then
            div [ Attr.style "position" "fixed"
                 , Attr.style "left" "0"
                 , Attr.style "top" "0"
                 , Attr.style "width"  "100%"
                 , Attr.style "height" "100%"
                 , Attr.style "background" "black"
                 , Attr.classList
                    [ ("animated", True)
                    , ("fadeOut delay-3s", True)
                    ]
                 ]
                 [ div [ Attr.style "width" "240px"
                        , Attr.style "margin" (String.fromFloat (0.5 * (Tuple.second model.size)) ++ "px auto 0 auto")
                        , Attr.style "text-align" "center"
                        , Attr.style "font-size" "30px"
                        , Attr.style "color" "white"
                        ]
                        [ text "Loading..."]
                 ]
    else
        div [ Attr.style "position" "fixed"
             , Attr.style "left" "0"
             , Attr.style "top" "0"
             , Attr.style "width" "100%"
             , Attr.style "height" "100%"
             , Attr.style "background" "black"
             , Attr.classList
                [ ("animated", True)
                , ("fadeOut delay-2s", True)
                ]
             ]
             []

{-  if model.page == Map1 then
    div [ Attr.style "position" "fixed"
         , Attr.style "left" "0"
         , Attr.style "top" "0"
         , Attr.style "width" "100%"
         , Attr.style "height" "100%"
         , Attr.style "background" "black"
         , Attr.classList
            [ ("animated", True)
            , ("fadeOut delay-3s", True)
            ]
         ]
         [ div [ Attr.style "width" "240px"
                , Attr.style "margin" (String.fromFloat (0.5 * (Tuple.second model.size)) ++ "px auto 0 auto")
                , Attr.style "text-align" "center"
                , Attr.style "font-size" "30px"
                , Attr.style "color" "white"
                ]
                [ text "Loading..."]
         ]
   else
      div[][]-}

renderCountdownPage : Model -> Html Msg
renderCountdownPage model =
  if (model.countdownLimit - Time.posixToMillis model.time) // 1000 > 0 then
    div [ Attr.style "position" "fixed"
         , Attr.style "left" "0"
         , Attr.style "top" "0"
         , Attr.style "width" "100%"
         , Attr.style "height" "100%"
         ]
         [ div [ Attr.style "width" "240px"
                , Attr.style "margin" (String.fromFloat (0.5 * (Tuple.second model.size)) ++ "px auto 0 auto")
                , Attr.style "text-align" "center"
                , Attr.style "font-size" "120px"
                , Attr.style "color" "white"
                ]
                [ text (String.fromInt((model.countdownLimit - Time.posixToMillis model.time) // 1000))]
         ]
   else
     div[][]



renderGame : Model -> Html Msg
renderGame model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "0"
        , Attr.style "top" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background" "black"
        ]
        [ div [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" (if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then "#1a8caf" else "black")
            , Attr.classList
                [ ("animated", checkEndMap1 model || (checkHigh1 model && model.high1Passed == False) || (checkHigh2 model && model.high2Passed == False) || checkEndMap2 model || (checkLow1 model && model.low1Passed == False) || (checkLow2 model && model.low2Passed == False) || checkEndMap3 model || checkHab3 model)
                , ("fadeOut", checkEndMap1 model || (checkHigh1 model && model.high1Passed == False) || (checkHigh2 model && model.high2Passed == False) || checkEndMap2 model || (checkLow1 model && model.low1Passed == False) || (checkLow2 model && model.low2Passed == False) || checkEndMap3 model || checkHab3 model)
                ]
            , if checkEndMap1 model then
                on "animationend" <| Decode.succeed <| Page Story1
              else if (checkHigh1 model && model.high1Passed == False) then
                on "animationend" <| Decode.succeed <| Page HighFirst
              else if (checkHigh2 model && model.high2Passed == False) then
                on "animationend" <| Decode.succeed <| Page HighSecond
              else if checkEndMap2 model then
                on "animationend" <| Decode.succeed <| Page Story2to3
              else if (checkLow1 model && model.low1Passed == False) then
                on "animationend" <| Decode.succeed <| Page LowFirst
              else if (checkLow2 model && model.low2Passed == False) then
                on "animationend" <| Decode.succeed <| Page LowSecond
              else if checkEndMap3 model then
                on "animationend" <| Decode.succeed <| Page Story3to4
              else if checkHab3 model && model.isRobber then
                on "animationend" <| Decode.succeed <| Page Ending2p1
              else if checkHab3 model && model.isRobber == False then
                on "animationend" <| Decode.succeed <| Page Ending4p1
              else
                onClick Noop
            ]
            [svg
                [ viewBox ((String.fromFloat (Tuple.first model.viewcen - (Tuple.first model.viewbox)/2))
                            ++ " "
                            ++ (String.fromFloat (Tuple.second model.viewcen - (Tuple.second model.viewbox)/2))
                            ++ " "
                            ++ (String.fromFloat (Tuple.first model.viewbox))
                            ++ " "
                            ++ (String.fromFloat (Tuple.second model.viewbox))
                          )
                , preserveAspectRatio "xMidYMid meet"
                ]
                ( renderBackground model
                ++ renderGrad model
                ++ renderWave model
                ++ renderBorder model
                ++ renderSpecial model
                --++ renderMapbox model
                ++ rendercorePlanet model
                ++ renderPlanet model
                ++ renderTail model
                ++ renderBulletTail model
                --++ renderFire model
                ++ renderCraft model
                ++ renderBullets model
                ++ renderLine model

                )
            , renderChatbox model
            , renderTargetbox model
            , renderMessage model
            , renderSmallmap model
            , renderAttributes model
            , renderCountdownPage model
            , renderLoadingPage model
            , renderPausebutton model
            , renderResumebutton model
            , renderPaused model
            , if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then
                renderScore model
              else
                div [] []
            --, renderBgm model
            ]
        ]

renderMessage : Model -> Html Msg
renderMessage model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "2%"
        , Attr.style "top" "2%"
        , Attr.style "font-size" "20px"
        , Attr.style "color" "white"
        , Attr.style "opacity" "0.6"
        ]
        [ text "[ Press Q ] for Menu"
        ]

renderTargetbox : Model -> Html Msg
renderTargetbox model =
    if model.page /= Inf1 && model.page /= Inf2 && model.page /= Inf3 then
        div [ Attr.style "position" "fixed"
            , Attr.style "bottom" "10%"
            , Attr.style "right" "0"
            , Attr.style "width" "20%"
            , Attr.style "height" "20%"
            , Attr.style "background" "url(assets/targetbox1.png) 0% 0% / 100% 100%"
            ]
            [
                   (if model.page == Map1 && model.tutorial /= StepNone then
                     div[ Attr.style "color" "white"
                        , Attr.style "left" "84%"
                        , Attr.style "top" "75%"
                        , Attr.style "width" "100%"
                        , Attr.style "font-size" "26px"
                        , Attr.style "position" "fixed"
                        ]
                        [ text "Target:"
                        , br [] []
                        , text "Finish tutorial"
                        ]
                    else if model.page == Map2 && (model.high1Passed == False || model.high2Passed == False) then
                     div[ Attr.style "color" "white"
                        , Attr.style "left" "82%"
                        , Attr.style "top" "75%"
                        , Attr.style "width" "100%"
                        , Attr.style "font-size" "19px"
                        , Attr.style "position" "fixed"
                        ]
                        [ text "Target:"
                        , br [] []
                        , text "Go to the planet marked"
                        , br [] []
                        , text "in the small map"
                        ]
                    else if model.page == Map3 && (model.low1Passed == False || model.low2Passed == False) then
                     div[ Attr.style "color" "white"
                        , Attr.style "left" "82%"
                        , Attr.style "top" "75%"
                        , Attr.style "width" "100%"
                        , Attr.style "font-size" "19px"
                        , Attr.style "position" "fixed"
                        ]
                        [ text "Target:"
                        , br [] []
                        , text "Go to the planet marked"
                        , br [] []
                        , text "in the small map"
                        ]
                    else if model.page == Map1 && model.tutorial == StepNone then
                     div[ Attr.style "color" "white"
                        , Attr.style "left" "82%"
                        , Attr.style "top" "75%"
                        , Attr.style "width" "100%"
                        , Attr.style "font-size" "18px"
                        , Attr.style "position" "fixed"
                        ]
                        [ text "Target:"
                        , br [] []
                        , text "Go to the green planet "
                        , br [] []
                        , text "in the lower-right corner."
                        ]
                    else
                       div [][ text "" ]
                    )
            ]
    else
        div [] []



view : Model -> Html Msg
view model =
    div []
    [case model.page of
        Begining ->
            renderBegin model
        Cover ->
            renderCover model
        Mapall ->
            renderChooseMap model
        Map1 ->
            renderGame model
        Story1 ->
            renderStory1 model
        HighFirst ->
            renderHighFirst model
        HighSecond ->
            renderHighSecond model
        Story2to3 ->
            renderStory2to3 model
        LowFirst ->
            renderLowFirst model
        LowSecond ->
            renderLowSecond model
        Story3to4 ->
            renderStory3to4 model
        Map2 ->
            renderGame model
        Map3 ->
            renderGame model
        Map4 ->
            renderGame model
        BE1 ->
            renderBE1 model
        BE2 ->
            renderBE2 model
        BE3 ->
            renderBE3 model
        Levelall ->
            renderChooseLevel model
        Help ->
            renderHelp model
        Inf1 ->
            renderGame model
        Inf2 ->
            renderGame model
        Inf3 ->
            renderGame model
        Ending2p1 ->
            renderEnding2p1 model
        Ending2p2 ->
            renderEnding2p2 model
        Ending2p3 ->
            renderEnding2p3 model
        Ending1p2 ->
            renderEnding1p2 model
        Ending1p3 ->
            renderEnding1p3 model
        Ending1p4 ->
            renderEnding1p4 model
        Ending3p1 ->
            renderEnding3p1 model
        Ending3p2 ->
            renderEnding3p2 model
        Ending4p1 ->
            renderEnding4p1 model
        _ ->
            div [] []
    , renderBgm model
    ]