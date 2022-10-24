module Game exposing (..)

import Dict
import CraftTail exposing (bulletTail)
import Html exposing (Html, div)
import Html exposing (..)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Attributes as Attr exposing (attribute)
import InitLevel1Part2 exposing (HoleType(..), Planet)
import Model exposing (AnimationState(..), Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..), CraftTail)
import Svg exposing (Svg, animate, circle, ellipse, g, image, line, polygon, rect, svg)
import Svg.Attributes exposing (attributeName, cx, cy, dur, fill, fillOpacity, height, opacity, points, preserveAspectRatio, r, repeatCount, rx, ry, stroke, strokeDasharray, strokeWidth, transform, values, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)
import Model exposing (Model, Msg, Pagetype(..), Pressed(..), Status(..), Step(..))
import Svg exposing (Svg, animate, circle, defs, ellipse, g, image, line, linearGradient, polygon, radialGradient, rect, stop, svg)
import Svg.Attributes exposing (attributeName, cx, cy, dur, fill, fillOpacity, height, id, offset, opacity, points, preserveAspectRatio, r, repeatCount, rx, ry, stopColor, stroke, strokeDasharray, strokeWidth, style, transform, values, viewBox, width, x, x1, x2, xlinkHref, y, y1, y2)


rendercorePlanet : Model -> List (Svg Msg)
rendercorePlanet { coreplanet} =
    [ circle
        [ cx (String.fromFloat coreplanet.px)
        , cy (String.fromFloat coreplanet.py)
        , r (String.fromInt coreplanet.rplanet)
        , fill "gold"
        ]
        []
    ]

rendercorePlanet_ : Model -> List (Svg Msg)
rendercorePlanet_ model =
    [ circle
        [ cx (String.fromFloat model.coreplanet.px)
        , cy (String.fromFloat model.coreplanet.py)
        , r (String.fromFloat (1.5 * toFloat model.coreplanet.rplanet))
        , fill (if model.page == Map2 && model.high1Passed && model.high2Passed == False then
                    "yellow"
                else
                    "gold"
                )
        ]
        (if model.page == Map2 && model.high1Passed && model.high2Passed == False then
            [ animate
                [ attributeName "r"
                , values (String.fromFloat (1.5 * toFloat model.coreplanet.rplanet) ++ ";" ++ String.fromFloat (2 * toFloat model.coreplanet.rplanet) ++ ";" ++ String.fromFloat (1.5 * toFloat model.coreplanet.rplanet))
                , dur "1s"
                , repeatCount "indefinite"
                ]
                []
            ]
        else
            []
        )
    ]

renderEscapeArea : Model -> List (Svg Msg)
renderEscapeArea model =
    if model.page == Map1 && model.tutorial == StepNone then
        [ circle
            [ cx "14600"
            , cy "12920"
            , r "1200"
            , fill "#4efc03"
            , stroke "#4efc03"
            , strokeWidth "150"
            ]
            [animate
                [ attributeName "opacity"
                , values "1;0.6;1"
                , dur "2s"
                , repeatCount "indefinite"
                ]
                []
            ]
        ]
    else if model.page == Map2 && model.high1Passed && model.high2Passed then
        [ circle
            [ cx "0"
            , cy "7000"
            , r "1200"
            , fill "#4efc03"
            , stroke "#4efc03"
            , strokeWidth "150"
            ]
            [animate
                [ attributeName "opacity"
                , values "1;0.6;1"
                , dur "2s"
                , repeatCount "indefinite"
                ]
                []
            ]
        ]
    else if model.page == Map3 && model.low1Passed && model.low2Passed then
        [ polygon
            [ points "3120,14906 4520,15220 2760,13700"
            , fill "#4efc03"
            , stroke "#4efc03"
            , strokeWidth "150"
            ]
            [animate
                [ attributeName "opacity"
                , values "1;0.6;1"
                , dur "2s"
                , repeatCount "indefinite"
                ]
                []
            ]
        ]
    else
        [rect [] []]

renderMapbox : Model -> List (Svg Msg)
renderMapbox model =
    if model.page == Map4 then
        [ circle
            [ cx "0"
            , cy "0"
            , r "13000"
            , stroke "#2ae7d7"
            , fill "black"
            , strokeWidth "150"
            ]
            []
        ]
    else if model.page == Map3 then
        [ polygon
            [ points "6680,889 849,6863 3120,14906 11228,16967 17029,10957 14780,2918"
            , stroke "#2ae7d7"
            , fill "black"
            , strokeWidth "150"
            ]
            []
        ]
    else
        [rect [] []]

renderGrad : Model -> List (Svg Msg)
renderGrad model =
    let
        mapPlanet : Planet -> Svg Msg
        mapPlanet planet =
            g []
                [ defs
                    []
                    [ radialGradient
                        [ id "orange_red"
                        ]
                        [ stop [ offset "0%", stopColor "#05f549" ] []
                        , stop [ offset "100%", stopColor "#abe8bd"] []
                        ]
                    ]
                , circle [ cx (String.fromFloat planet.px)
                         , cy (String.fromFloat planet.py)
                         , r (String.fromInt planet.range)
                         , opacity "0.6"
                         , style "fill:url(#orange_red)"
                         ]
                         []
                ]
    in
    if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then
        List.map (\ x -> mapPlanet x ) model.planetList
    else
        []

renderPlanet : Model -> List (Svg Msg)
renderPlanet model =
    let
        mapPlanet : Planet -> Svg Msg
        mapPlanet planet =
            image
                [case model.page of
                    Map1 ->
                        case planet.id of
                            1 ->
                                xlinkHref "assets/venus.png"
                            2 ->
                                xlinkHref "assets/earth.png"
                            3 ->
                                xlinkHref "assets/mars.png"
                            4 ->
                                xlinkHref "assets/jupiter.png"
                            5 ->
                                xlinkHref "assets/saturn.png"
                            6 ->
                                xlinkHref "assets/Planet_Z.png"
                            _ ->
                                xlinkHref "assets/planet1.png"
                    Map2 ->
                        case planet.id of
                            1 ->
                                xlinkHref "assets/planet1.png"
                            2 ->
                                xlinkHref "assets/planet2.png"
                            3 ->
                                xlinkHref "assets/planet3.png"
                            4 ->
                                xlinkHref "assets/planet4.png"
                            5 ->
                                xlinkHref "assets/planet5.png"
                            6 ->
                                xlinkHref "assets/planet6.png"
                            7 ->
                                xlinkHref "assets/planet7.png"
                            8 ->
                                xlinkHref "assets/planet8.png"
                            9 ->
                                xlinkHref "assets/planet9.png"
                            10 ->
                                xlinkHref "assets/planet10.png"
                            11 ->
                                xlinkHref "assets/planet11.png"
                            12 ->
                                xlinkHref "assets/planet12.png"
                            13 ->
                                xlinkHref "assets/planet13.png"
                            14 ->
                                xlinkHref "assets/planet14.png"
                            _ ->
                                xlinkHref "assets/planet15.png"

                    Map3 ->
                        case planet.id of
                            1 ->
                                xlinkHref "assets/planet15.png"
                            2 ->
                                xlinkHref "assets/planet16.png"
                            3 ->
                                xlinkHref "assets/planet17.png"
                            4 ->
                                xlinkHref "assets/planet18.png"
                            5 ->
                                xlinkHref "assets/planet19.png"
                            6 ->
                                xlinkHref "assets/planet20.png"
                            7 ->
                                xlinkHref "assets/planet21.png"
                            8 ->
                                xlinkHref "assets/planet22.png"
                            9 ->
                                xlinkHref "assets/planet23.png"
                            10 ->
                                xlinkHref "assets/planet24.png"
                            11 ->
                                xlinkHref "assets/planet25.png"
                            12 ->
                                xlinkHref "assets/planet26.png"
                            13 ->
                                xlinkHref "assets/planet1.png"
                            14 ->
                                xlinkHref "assets/planet2.png"
                            15 ->
                                xlinkHref "assets/planet3.png"
                            16 ->
                                xlinkHref "assets/planet4.png"
                            17->
                                xlinkHref "assets/planet5.png"
                            18 ->
                                xlinkHref "assets/planet6.png"
                            19 ->
                                xlinkHref "assets/planet7.png"
                            20 ->
                                xlinkHref "assets/planet8.png"
                            21 ->
                                xlinkHref "assets/planet9.png"
                            _ ->
                                xlinkHref "assets/planet10.png"

                    Map4 ->
                        case planet.id of
                            1 ->
                                xlinkHref "assets/planet10.png"
                            2 ->
                                xlinkHref "assets/planet11.png"
                            3 ->
                                xlinkHref "assets/planet12.png"
                            4 ->
                                xlinkHref "assets/planet13.png"
                            5 ->
                                xlinkHref "assets/planet14.png"
                            6 ->
                                xlinkHref "assets/planet15.png"
                            7 ->
                                xlinkHref "assets/planet16.png"
                            8 ->
                                xlinkHref "assets/planet17.png"
                            9 ->
                                xlinkHref "assets/planet18.png"
                            10 ->
                                xlinkHref "assets/planet19.png"
                            11 ->
                                xlinkHref "assets/planet20.png"
                            12 ->
                                xlinkHref "assets/planet21.png"
                            13 ->
                                xlinkHref "assets/planet22.png"
                            _ ->
                                xlinkHref "assets/planet23.png"

                    _ ->
                        xlinkHref "assets/earth.png"


                , x (String.fromFloat (planet.px - toFloat planet.rplanet))
                , y (String.fromFloat (planet.py - toFloat planet.rplanet))
                , width (String.fromInt (2 * planet.rplanet))
                , height (String.fromInt (2 * planet.rplanet))
                ]
                []
            {-circle
                [ cx (String.fromFloat planet.px)
                , cy (String.fromFloat planet.py)
                , r (String.fromInt planet.rplanet)
                , fill "#2ae7d7"
                ]
                []-}
    in
        List.map (\ x -> mapPlanet x ) model.planetList

renderPlanet_ : Model -> List (Svg Msg)
renderPlanet_ model =
    let
        mapPlanet : Planet -> Svg Msg
        mapPlanet planet =
            circle
                [ cx (String.fromFloat planet.px)
                , cy (String.fromFloat planet.py)
                , r (String.fromFloat (1.5 * toFloat planet.rplanet))
                , fill ( if model.page == Map2 && model.high1Passed == False && planet.id == 2 then
                            "yellow"
                         else if model.page == Map3 && model.low1Passed == False && planet.id == 19 then
                            "yellow"
                         else if model.page == Map3 && model.low1Passed && model.low2Passed == False && planet.id == 1 then
                            "yellow"
                         else if model.page == Map4 && (planet.id == 5 || planet.id == 8 || planet.id == 6) then
                            "yellow"
                         else
                            "#2ae7d7"
                       )
                ]
                (if (model.page == Map2 && model.high1Passed == False && planet.id == 2)  || (model.page == Map3 && model.low1Passed == False && planet.id == 19) || (model.page == Map3 && model.low1Passed && model.low2Passed == False && planet.id == 1) || (model.page == Map4 && (planet.id == 5 || planet.id == 8 || planet.id == 6)) then
                    [ animate
                        [ attributeName "r"
                        , values (String.fromFloat (1.5 * toFloat planet.rplanet) ++ ";" ++ String.fromFloat (2 * toFloat planet.rplanet) ++ ";" ++ String.fromFloat (1.5 * toFloat planet.rplanet))
                        , dur "1s"
                        , repeatCount "indefinite"
                        ]
                        []
                    ]
                else
                    []
                )
    in
        List.map (\ x -> mapPlanet x ) model.planetList

renderCraft : Model -> List (Svg Msg)
renderCraft { craft } =
    [ image
        [ xlinkHref "assets/craft.png"
        , x (String.fromFloat (craft.cx - 1.5 * toFloat craft.rcraft))
        , y (String.fromFloat (craft.cy - 1.5 * toFloat craft.rcraft))
        , width (String.fromFloat (3 * toFloat craft.rcraft))
        , height (String.fromFloat (3 * toFloat craft.rcraft))
        ]
        []
    {-circle
        [ cx (String.fromFloat craft.cx)
        , cy (String.fromFloat craft.cy)
        , r (String.fromInt craft.rcraft)
        , fill "orange"
        ]
        []-}
    ]

renderCraft_ : Model -> List (Svg Msg)
renderCraft_ { craft} =
    [ circle
        [ cx (String.fromFloat craft.cx)
        , cy (String.fromFloat craft.cy)
        , r (String.fromInt (8 * craft.rcraft))
        , fill "orange"
        ]
        []
    ]


renderSpecial : Model -> List (Svg Msg)
renderSpecial model =
    let
        mapSpecial : ( Float, Float ) -> HoleType -> Svg Msg
        mapSpecial ( n, m ) holetype =
            case holetype of
                Meteorite ->
                     image [ xlinkHref "assets/aerolite.png"
                          , x (String.fromFloat (n - toFloat(model.wsize)))
                          , y (String.fromFloat (m - toFloat(model.wsize)))
                          , width (String.fromInt (2 * model.wsize))
                          , height (String.fromInt (2 * model.wsize))
                          ]
                          []
                WhiteHole ->
                    g []
                       [image [ xlinkHref "assets/wh.png"
                              , x (String.fromFloat (n - 2 * toFloat(model.wsize)))
                              , y (String.fromFloat (m - 2 * toFloat(model.wsize)))
                              , width (String.fromInt (4 * model.wsize))
                              , height (String.fromInt (4 * model.wsize))
                              ]
                              []
                       , circle [ cx (String.fromFloat n)
                                , cy (String.fromFloat m)
                                , r "150"
                                , fill "transparent"
                                , stroke "white"
                                , strokeWidth "2"
                                , opacity "0.8"
                                , strokeDasharray "20 40"
                                ]
                                []
                       ]
                Repairstation ->
                    g []
                       [image [ xlinkHref "assets/repair.png"
                              , x (String.fromFloat (n - 1.5 * toFloat(model.wsize)))
                              , y (String.fromFloat (m - 1.5 * toFloat(model.wsize)))
                              , width (String.fromInt (3 * model.wsize))
                              , height (String.fromInt (3 * model.wsize))
                              ]
                              []
                       , circle [ cx (String.fromFloat n)
                                , cy (String.fromFloat m)
                                , r "150"
                                , fill "transparent"
                                , stroke "#46e72a"
                                , strokeWidth "2"
                                , opacity "0.8"
                                , strokeDasharray "20 40"
                                ]
                                []
                       ]
                Gasstation ->
                    g []
                       [image [ xlinkHref "assets/gas.png"
                              , x (String.fromFloat (n - 1.5 * toFloat(model.wsize)))
                              , y (String.fromFloat (m - 1.5 * toFloat(model.wsize)))
                              , width (String.fromInt (3 * model.wsize))
                              , height (String.fromInt (3 * model.wsize))
                              ]
                              []
                       , circle [ cx (String.fromFloat n)
                                , cy (String.fromFloat m)
                                , r "150"
                                , fill "transparent"
                                , stroke "#e7922a"
                                , strokeWidth "2"
                                , opacity "0.8"
                                , strokeDasharray "20 40"
                                ]
                                []
                       ]
                AttackHole ->
                     image [ xlinkHref "assets/whiteholered.png"
                          , x (String.fromFloat (n - 2 * toFloat(model.wsize)))
                          , y (String.fromFloat (m - 2 * toFloat(model.wsize)))
                          , width (String.fromInt (4 * model.wsize))
                          , height (String.fromInt (4 * model.wsize))
                          ]
                          []
                _ ->
                    rect[][]
        {-adjustedUpdateHoles = List.filter (\ x -> x.wx > toFloat x.selfRange && x.wx < toFloat model.mapWidth 
                                        {-&& x.wy > toFloat x.selfRange-} && x.wy < 20500) model.updateHoles-}
    in
    List.map (\ hole -> mapSpecial ( hole.wx, hole.wy ) hole.types ) model.updateHoles

renderCharacters : Model -> Html Msg
renderCharacters model =
          div [ Attr.style "position" "fixed"
              , Attr.style "left" "24%"
              , Attr.style "top" "14%"
              , Attr.style "width" "400px"
              , Attr.style "height" "580px"
              , Attr.classList
                    [ ( "animated", True )
                    , ( "fadeIn",  True )
                    ]
              , if List.member model.story1text [Step1, Step2, Step3 ] || model.highsecondtext == Step6 || List.member model.story2to3text [Step1, Step2, Step3] || model.lowfirsttext == Step1 || List.member model.lowsecondtext [Step1, Step4] then
                   Attr.style "background" "url(assets/robot.png) 0% 0% / 100% 100%"
                else
                   Attr.style "background" "url(assets/darkrobot.png) 0% 0% / 100% 100%"
              ]
              [ div[ Attr.style "position" "fixed"
                   , Attr.style "left" "42%"
                   , Attr.style "top" "2%"
                   , Attr.style "width" "500px"
                   , Attr.style "height" "640px"
                   , Attr.classList
                            [ ( "animated", True )
                            , ( "fadeIn",  True )
                            ]
                   , if List.member model.highfirsttext [Step2, Step3, Step6] || model.lowfirsttext == Step4 || List.member model.lowsecondtext [Step2, Step6, Step8] then
                         Attr.style "background" "url(assets/scientist.png) 0% 0% / 100% 100%"
                     else
                         Attr.style "background" "url(assets/darkscientist.png) 0% 0% / 100% 100%"
                   ]
                   [ div[ Attr.style "position" "fixed"
                        , Attr.style "left" "72%"
                        , Attr.style "top" "18%"
                        , Attr.style "width" "300px"
                        , Attr.style "height" "400px"
                        , Attr.classList
                            [ ( "animated", True )
                            , ( "fadeIn",  True )
                            ]
                        , if model.highfirsttext == Step7 || model.highsecondtext == Step2 || model.lowfirsttext == Step5 || List.member model.lowsecondtext [Step3, Step7, Step9] then
                              Attr.style "background" "url(assets/engineer.png) 0% 0% / 100% 100%"
                          else
                              Attr.style "background" "url(assets/darkengineer.png) 0% 0% / 100% 100%"
                        ]
                        (if List.member model.highfirsttext [Step1, Step2, Step3, Step4, Step5,Step6] then
                          [ div [ Attr.style "position" "fixed"
                              , Attr.style "top" "18%"
                              , Attr.style "left" "0%"
                              , Attr.style "width" "300px"
                              , Attr.style "height" "400px"
                              , Attr.classList
                                   [ ("animated", True)
                                   , ("slideInLeft", model.highfirsttext == Step1)
                                   , ("slideOutLeft", model.highfirsttext == Step6)
                                   ]
                              , if List.member model.highfirsttext [Step1, Step4, Step5] then
                                    Attr.style "background" "url(assets/Civilian.png) 0% 0% / 100% 100%"
                                else
                                    Attr.style "background" "url(assets/darkcivilian.png) 0% 0% / 100% 100%"
                              ]
                              []
                          ]
                        else if List.member model.highsecondtext [Step1,Step2, Step3, Step4, Step5,Step6] then
                          [ div [ Attr.style "position" "fixed"
                              , Attr.style "top" "18%"
                              , Attr.style "left" "0%"
                              , Attr.style "width" "300px"
                              , Attr.style "height" "400px"
                              , Attr.classList
                                   [ ("animated", True)
                                   , ("slideInLeft", model.highfirsttext == Step1)
                                   , ("slideOutLeft", model.highfirsttext == Step6)
                                   ]
                              , if List.member model.highsecondtext [Step1, Step3, Step4, Step5] then
                                    Attr.style "background" "url(assets/Dominant.png) 0% 0% / 100% 100%"
                                else
                                    Attr.style "background" "url(assets/darkdominant.png) 0% 0% / 100% 100%"
                              ]
                              []
                          ]
                        else if List.member model.lowsecondtext [Step2, Step3, Step4] then
                          [ div [ Attr.style "position" "fixed"
                              , Attr.style "top" "18%"
                              , Attr.style "left" "0%"
                              , Attr.style "width" "300px"
                              , Attr.style "height" "400px"
                              , Attr.classList
                                   [ ("animated", True)
                                   , ("slideInLeft", model.lowsecondtext == Step2)
                                   , ("slideOutLeft", model.lowsecondtext == Step4)
                                   ]
                              , Attr.style "background" "url(assets/barbarian.png) 0% 0% / 100% 100%"
                              ]
                              []
                          ]
                        else
                          [])

                   ]
              ]


--<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.7.2/animate.min.css">


renderWave : Model -> List (Svg Msg)
renderWave model =
    let
        wave = Maybe.withDefault { sy = 0, speed = 0, length = 0 } model.strangeWave
    in
    if model.ifStrangeWave && model.strangeWave /= Nothing then
            [ defs []
                [ linearGradient
                    [ id "lig"
                    , x1 "0%"
                    , x2 "0%"
                    , y1 "0%"
                    , y2 "100%"
                    ]
                    [ stop [ offset "0%", stopColor "#b781f6"] []
                    , stop [ offset "50%", stopColor "#831ef6"] []
                    , stop [ offset "100%", stopColor "#b781f6"] []
                    ]
                ]
            , rect
                [ x "0"
                , y (String.fromFloat wave.sy)
                , width "7200"
                , height (String.fromInt wave.length)
                , opacity "0.5"
                , style "fill:url(#lig)"
                ]
                []
            ]
    else
        []



renderFire : Model -> List (Svg Msg)
renderFire {craft,fire,status,gamepad} =
    let
        speed1 = sqrt((Tuple.first craft.speed)^2 + (Tuple.second craft.speed)^2)

    in
        if status /= Hung && gamepad.up == StartPressed then
               [ellipse
                    [ cx (String.fromFloat craft.cx)
                    , cy (String.fromFloat (craft.cy + 20))
                    , rx (String.fromFloat (speed1*0.05))
                    , ry (String.fromFloat (5000/speed1))
                    , fill "red"
                    , transform ("rotate(90," ++(String.fromFloat craft.cx)++","++(String.fromFloat (craft.cy + 20))++")")
                    ]
                    []
               ]
        else if status /= Hung && gamepad.down == StartPressed then
               [ellipse
                    [ cx (String.fromFloat craft.cx)
                    , cy (String.fromFloat (craft.cy - 20))
                    , rx (String.fromFloat (speed1*0.05))
                    , ry (String.fromFloat (5000/speed1))
                    , fill "red"
                    , transform ("rotate(270," ++(String.fromFloat craft.cx)++","++(String.fromFloat (craft.cy - 20))++")")
                    ]
                    []
               ]
        else if status /= Hung && gamepad.left == StartPressed then
               [ellipse
                    [ cx (String.fromFloat (craft.cx + 20))
                    , cy (String.fromFloat craft.cy)
                    , rx (String.fromFloat (speed1*0.05))
                    , ry (String.fromFloat (5000/speed1))
                    , fill "red"
                    , transform ("rotate(0," ++(String.fromFloat (craft.cx + 20))++","++(String.fromFloat craft.cy)++")")
                    ]
                    []
               ]
        else if status /= Hung && gamepad.right == StartPressed then
               [ellipse
                    [ cx (String.fromFloat (craft.cx - 20))
                    , cy (String.fromFloat craft.cy)
                    , rx (String.fromFloat (speed1*0.05))
                    , ry (String.fromFloat (5000/speed1))
                    , fill "red"
                    , transform ("rotate(180," ++(String.fromFloat (craft.cx - 20))++","++(String.fromFloat craft.cy)++")")
                    ]
                    []
               ]
        else
           [rect [][]]


renderTail : Model -> List (Svg Msg)
renderTail model =
    let
        mapTail : ( Float, Float, Float )-> Svg Msg
        mapTail ( n, m, p) =
                circle [cx (String.fromFloat n)
                      , cy (String.fromFloat m)
                      , r (String.fromFloat (0.8 * p))
                      , fill "white"
                      , fillOpacity (String.fromFloat ((p/20)^8) )
                      ]
                      []

        mapcraftTail : CraftTail-> Svg Msg
        mapcraftTail crafttail =
            let
                side1 = crafttail.sideone
                side2 = crafttail.sidetwo
                point1x = side1.px + side1.radius*Tuple.first side1.direction
                point1y = side1.py + side1.radius*Tuple.second side1.direction
                point2x = side2.px + side2.radius*Tuple.first side2.direction
                point2y = side2.py + side2.radius*Tuple.second side2.direction
                point3x = side1.px - side1.radius*Tuple.first side1.direction
                point3y = side1.py - side1.radius*Tuple.second side1.direction
                point4x = side2.px - side2.radius*Tuple.first side2.direction
                point4y = side2.py - side2.radius*Tuple.second side2.direction
            in
                polygon
                    [ points (String.fromFloat point1x ++ "," ++ String.fromFloat point1y
                            ++ " "
                            ++ String.fromFloat point3x ++ "," ++ String.fromFloat point3y
                            ++ " "
                            ++ String.fromFloat point4x ++ "," ++ String.fromFloat point4y
                            ++ " "
                            ++ String.fromFloat point2x ++ "," ++ String.fromFloat point2y)
                      , fill "white"
                      , fillOpacity (String.fromFloat ((crafttail.sideone.radius/13)^2) )
                    ]
                    []
    in
    if model.ifStrangeWave == False then
        List.map (\ t -> mapcraftTail t) model.craftTail
    else
        List.map (\ t -> mapTail ( t.cx, t.cy, t.r )) model.tail

{-renderTail : Model -> List (Svg Msg)
renderTail {craft,status,gamestate} =
    let
        speed1 = sqrt (Tuple.first craft.speed^2 + Tuple.second craft.speed^2)
        tx = craft.cx - Tuple.first craft.speed / speed1 * 50
        ty = craft.cy - Tuple.second craft.speed / speed1 * 50
        theta = (atan2 (Tuple.second craft.speed)  (Tuple.first craft.speed))/ 3.1416 * 180-}


renderBullets : Model -> List (Svg Msg)
renderBullets model =
    let
        mapBullets : ( Float, Float ) -> Svg Msg
        mapBullets (x, y) =
             circle
                [ cx (String.fromFloat x)
                , cy (String.fromFloat y)
                , r (String.fromInt 8)
                , fill "red"
                ]
                []
    in
        List.map (\ bullet -> mapBullets ( bullet.x, bullet.y ) ) model.bullets

renderBulletTail : Model -> List (Svg Msg)
renderBulletTail model =
        let
            mapTail : ( Float, Float, Float )-> Svg Msg
            mapTail ( n, m, p ) =
                    circle [cx (String.fromFloat n)
                          , cy (String.fromFloat m)
                          , r (String.fromFloat (2*p))
                          , fill "red"
                          , fillOpacity (String.fromFloat (p/6))
                          ]
                          []
        in
    List.map (\ t -> mapTail ( t.cx, t.cy, t.r )) model.bullettail







    {-[ rect
        [ x (String.fromFloat (Tuple.first model.viewcen + 500))
        , y (String.fromFloat (Tuple.second model.viewcen - 200))
        , rx "10"
        , ry "10"
        , width "100"
        , height "20"
        , fill "transparent"
        , strokeWidth "2"
        , stroke "white"]
        []-}

renderAttributes : Model -> Html Msg
renderAttributes model =
    let
        hp = model.craft.hp
        fuel = model.craft.fuel
    in
    div [ Attr.style "position" "fixed"
        , Attr.style "right" "270px"
        , Attr.style "top" "5%"
        , Attr.style "width" "350px"
        , Attr.style "height" "200px"
        ]
         [svg
                [ viewBox "0 0 400 400"
                , preserveAspectRatio "xMidYMid none"
                ]
                 [rect [  x (String.fromFloat 250)
                         , y (String.fromFloat 50)
                         , rx "10"
                         , ry "10"
                         , width (String.fromFloat (1.2*hp))
                         , height "22"
                         , fill "green"
                         ]
                         []
                  , rect [ x (String.fromFloat 250)
                         , y (String.fromFloat 100)
                         , rx "10"
                         , ry "10"
                         , width (String.fromFloat (1.2*fuel))
                         , height "22"
                         , fill "red"
                         ]
                         []
                  , image [ xlinkHref "assets/hpline.png"
                          , x (String.fromFloat 96)
                          , y (String.fromFloat 17)
                          , width "402"
                          , height "90"
                          ]
                         []
                  , image [ xlinkHref "assets/fuelline.png"
                          , x (String.fromFloat 23)
                          , y (String.fromFloat 52)
                          , width "550"
                          , height "115"
                          ]
                         []
                  ]
         ]



renderLine : Model -> List (Svg Msg)
renderLine model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
        px = model.craft.planetin.px
        py = model.craft.planetin.py
        dist = sqrt ( (cx - px) * (cx - px) + (cy - py) * (cy - py) )
        swidth_ = min 10 (2000 / dist)
        swidth = max swidth_ 3
    in
    [ line
        [ x1 (String.fromInt (round cx))
        , y1 (String.fromInt (round cy))
        , x2 (String.fromInt (round px))
        , y2 (String.fromInt (round py))
        , opacity "0.5"
        , stroke "white"
        , strokeWidth (String.fromFloat swidth)
        , strokeDasharray (String.fromFloat (max 10 (dist/60)) ++ " 10")
       ]
        []
    ]

renderLine_ : Model -> List (Svg Msg)
renderLine_ model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
        px = model.craft.planetin.px
        py = model.craft.planetin.py
    in
    [ line
        [ x1 (String.fromInt (round cx))
        , y1 (String.fromInt (round cy))
        , x2 (String.fromInt (round px))
        , y2 (String.fromInt (round py))
        , stroke "white"
        , strokeWidth "20"
       ]
        []
    ]

renderSmallmap : Model -> Html Msg
renderSmallmap model =
    div [ Attr.style "position" "fixed"
        , Attr.style "right" "0"
        , Attr.style "top" "0"
        , Attr.style "width" (if model.page == Map1 then
                                "276px"
                             else if model.page == Map2 then
                                "144px"
                             else if model.page == Map3 then
                                "260px"
                             else
                                "260px")
        , Attr.style "height" (if model.page == Map1 then
                                "242px"
                              else if model.page == Map2 then
                                "410px"
                              else if model.page == Map3 then
                                "260px"
                              else
                                "260px")
        , Attr.style "background" (if model.page == Map3 || model.page == Map4 then
                                    "transparent"
                                   else
                                    "black")
        , Attr.style "border" (if model.page == Map3 || model.page == Map4 then
                                "3px solid transparent"
                               else
                                "3px solid #2ae7d7")
        , Attr.style "border-radius" "2px"
        ]
        [svg
            [ viewBox (if model.page == Map1 then
                        "0 0 14800 13020"
                      else if model.page == Map2 then
                        "0 0 7200 20500"
                      else if model.page == Map3 then
                        "500 500 16629 16567"
                      else if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then
                        "0 " ++ (String.fromFloat (model.craft.cy - 3600)) ++ " 7200 7200"
                      else
                        "-13000 0 13000 13000")
            , preserveAspectRatio "xMidYMid none"
            ]
            ( renderMapbox model
            ++ renderEscapeArea model
            ++ renderPlanet_ model
            ++ renderCraft_ model
            ++ rendercorePlanet_ model
            ++ renderLine_ model
            ++ [rect [ x (String.fromFloat (Tuple.first model.viewcen - 800))
                     , y (String.fromFloat (Tuple.second model.viewcen - 375))
                     , width "1600"
                     , height "750"
                     , fill "transparent"
                     , stroke "white"
                     , strokeWidth "20"
                     ]
                     []
               ]
            )
        ]

--<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.7.2/animate.min.css">
--delta x = 4550
renderBorder : Model -> List (Svg Msg)
renderBorder model =
    case model.page of
        Map1 ->
            [rect [ x "0"
                  , y "0"
                  , width "14800"
                  , height "13020"
                  , fill "transparent"
                  , stroke "white"
                  , strokeWidth "20"
                  ]
                  []
            ]

        Map2 ->
            [rect [ x "0"
                  , y "0"
                  , width "7200"
                  , height "20500"
                  , fill "transparent"
                  , stroke "white"
                  , strokeWidth "20"
                  ]
                  []
            ]
    --6680,889  849,6863   3120,14906  11228,16967   17029,10957  14780,2918
        Map3 ->
            [ line
                 [ x1 "6680"
                 , y1 "889"
                 , x2 "849"
                 , y2 "6863"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            , line
                 [ x1 "849"
                 , y1 "6863"
                 , x2 "3120"
                 , y2 "14906"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            , line
                 [ x1 "3120"
                 , y1 "14906"
                 , x2 "11228"
                 , y2 "16967"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            , line
                 [ x1 "11228"
                 , y1 "16967"
                 , x2 "17029"
                 , y2 "10957"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            , line
                 [ x1 "17029"
                 , y1 "10957"
                 , x2 "14780"
                 , y2 "2918"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            , line
                 [ x1 "14780"
                 , y1 "2918"
                 , x2 "6680"
                 , y2 "889"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            ]
        Map4 ->
            [ line
                 [ x1 "-13000"
                 , y1 "0"
                 , x2 "0"
                 , y2 "0"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            , line
                 [ x1 "0"
                 , y1 "0"
                 , x2 "0"
                 , y2 "13000"
                 , opacity "0.5"
                 , stroke "white"
                 , strokeWidth "5"
                 ]
                 []
            ]



        _ ->
            [rect[][]]


renderBackground : Model -> List (Svg Msg)
renderBackground model =
    case model.page of
        Map1 ->
            [ image
                [ xlinkHref "assets/5.png"
                , x "-1000"
                , y "-200"
                , width "6000"
                , height "13520"
                ]
                []
            , image
                [ xlinkHref "assets/5.png"
                , x "3550"
                , y "-200"
                , width "6000"
                , height "13520"
                ]
                []
            , image
                [ xlinkHref "assets/5.png"
                , x "8100"
                , y "-200"
                , width "6000"
                , height "13520"
                ]
                []
            , image
                [ xlinkHref "assets/5.png"
                , x "12650"
                , y "-200"
                , width "6000"
                , height "13520"
                ]
                []
            ]
        Map2 ->
            [ image
                [ xlinkHref "assets/Map2background.jpg"
                , x "0"
                , y "0"
                , width "7200"
                , height "20500"
                ]
                []
            ]
        Map3 ->
            [ image
                [ xlinkHref "assets/Map3background.png"
                , x "-1400"
                , y "-200"
                , width "6000"
                , height "18000"
                ]
                []
            , image
                [ xlinkHref "assets/Map3background.png"
                , x "3050"
                , y "-200"
                , width "6000"
                , height "18000"
                ]
                []
            , image
                [ xlinkHref "assets/Map3background.png"
                , x "7600"
                , y "-200"
                , width "6000"
                , height "18000"
                ]
                []
            , image
                [ xlinkHref "assets/Map3background.png"
                , x "12150"
                , y "-200"
                , width "6000"
                , height "18000"
                ]
                []
            ]
        Map4 ->
            [ image
                [ xlinkHref "assets/Map4background.jpg"
                , x "-13500"
                , y "-500"
                , width "7200"
                , height "14000"
                ]
                []
            , image
                [ xlinkHref "assets/Map4background.jpg"
                , x "-8950"
                , y "-500"
                , width "6000"
                , height "14000"
                ]
                []
            , image
                [ xlinkHref "assets/Map4background.jpg"
                , x "-4400"
                , y "-500"
                , width "6000"
                , height "14000"
                ]
                []
            , image
                [ xlinkHref "assets/Map4background.jpg"
                , x "150"
                , y "-500"
                , width "6000"
                , height "14000"
                ]
                []
            ]
        _ ->
            []

renderScore : Model -> Html Msg
renderScore model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "6%"
            ]
            [ div [ Attr.style "font-size" "72px"
                  , Attr.style "text-align" "center"
                  , Attr.style "color" "white"
                  , Attr.style "opacity" "0.6"
                  , Attr.style "font-weight" "900"
                  ]
                  [ br [] []
                  , text ( Debug.toString ( 22500 - floor (model.craft.cy ) ))
                  , text "m"
                  ]
            ]