module Story exposing (..)

import Button exposing (renderExitbutton, renderRestartbutton)
import Game exposing (renderCharacters)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Model exposing (Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..))
import Chatbox exposing (renderChatbox)
import PauseInterface exposing (renderPaused)
import Bgm exposing (renderBgm)

renderBE1 : Model -> Html Msg
renderBE1 model =
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
                , ( "fadeIn", True )
                , ( "fadeOut", model.millisclock >= 3 )
                ]
            , if model.millisclock > 3 then
                  on "animationend" <| Decode.succeed <| Page BE2
              else
                  onClick Noop
            ]
             [ div [ Attr.style "position" "fixed"
                 , Attr.style "left" "0"
                 , Attr.style "top" "0"
                 , Attr.style "width" "100%"
                 , Attr.style "height" "100%"
                 , Attr.style "background" "url(assets/BE1svg.png) 0% 0% / 100% 100%"
                 ]
                 []

             ]
        ]

renderBE2 : Model -> Html Msg
renderBE2 model =
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
                , ( "fadeIn", True )
                , ( "fadeOut", model.millisclock >= 3 )
                ]
            , if model.millisclock > 3 then
                  on "animationend" <| Decode.succeed <| Page BE3
              else
                  onClick Noop
            ]
             [ div [ Attr.style "position" "fixed"
                 , Attr.style "left" "0"
                 , Attr.style "top" "0"
                 , Attr.style "width" "100%"
                 , Attr.style "height" "100%"
                 , Attr.style "background" "url(assets/BE2svg.png) 0% 0% / 100% 100%"
                 ]
                 []

             ]
        ]

renderBE3 : Model -> Html Msg
renderBE3 model =
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
                , ( "fadeIn", True )
                , ( "fadeOut", model.millisclock >= 3 )
                ]
            , if model.millisclock > 3 then
                  on "animationend" <| Decode.succeed <| Page Map1
              else
                  onClick Noop
            ]
             [ div [ Attr.style "position" "fixed"
                 , Attr.style "left" "0"
                 , Attr.style "top" "0"
                 , Attr.style "width" "100%"
                 , Attr.style "height" "100%"
                 , Attr.style "background" "url(assets/BE3svg.png) 0% 0% / 100% 100%"
                 ]
                 []

             ]
        ]

renderStory1 : Model -> Html Msg
renderStory1 model =
    div [ Attr.style "position" "fixed"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background" "black"
        ]
        [ div [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "right" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "black"
            , Attr.classList
                [ ("animated", True)
                , ("fadeIn", True)
                , ("fadeOut", model.story1text == Step4)
                ]
            , if model.story1text == Step4 then
                on "animationend" <| Decode.succeed <| Page Map2
              else
                onClick Noop
            ]
            [div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "right" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]

renderStory2to3: Model -> Html Msg     -- fuel warning
renderStory2to3 model =
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
                , ("fadeOut", model.story2to3text == Step4)
                ]
            , if model.story2to3text == Step4 then
                on "animationend" <| Decode.succeed <| Page Map3
              else
                onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "top" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]

renderStory3to4: Model -> Html Msg
renderStory3to4 model =
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
                , ("fadeOut", model.story3to4text == Step4)
                ]
            , if model.story3to4text == Step4 then
                on "animationend" <| Decode.succeed <| Page Map4
              else
                onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "top" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]


renderLowFirst: Model -> Html Msg
renderLowFirst model =
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
                , ("fadeOut", model.lowfirsttext == Step6)
                ]
            , if model.lowfirsttext == Step6 then
                on "animationend" <| Decode.succeed <| Page Map3
              else
                onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "top" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]

renderLowSecond: Model -> Html Msg
renderLowSecond model =
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
                , ("fadeOut", model.lowsecondtext == Step11)
                ]
            , if model.lowsecondtext == Step11 || model.lowsecondtext == Step12 then
                on "animationend" <| Decode.succeed <| Page Map3
              else
                onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "top" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]

renderHighFirst: Model -> Html Msg
renderHighFirst model =
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
                , ("fadeOut", model.highfirsttext == Step8)
                ]
            , if model.highfirsttext == Step8 then
                on "animationend" <| Decode.succeed <| Page Map2
              else
                onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "top" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]

renderHighSecond: Model -> Html Msg
renderHighSecond model =
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
                , ("fadeOut", model.highsecondtext == Step7 || model.highsecondtext == Step8 )
                ]
            , if model.highsecondtext == Step8 then
                on "animationend" <| Decode.succeed <| Page Map2
              else if model.highsecondtext == Step7 then
                on "animationend" <| Decode.succeed <| Page Ending3p1
              else
                onClick Noop
            ]
            [ div [ Attr.style "position" "fixed"
                , Attr.style "left" "0"
                , Attr.style "top" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                , Attr.style "background" "url(assets/spacere.png) 0% 0% / 100% 100%"
                ]
                []
            , renderCharacters model
            , renderChatbox model
            , renderPaused model
            --, renderBgm model
            ]
        ]


renderEnding1p2 : Model -> Html Msg
renderEnding1p2 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ br [] [], br [] [], br [] [], br [] [], br [] [], br [] [], br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.style "top" "50%"
                        , Attr.classList
                             [ ("animated", True)
                             , ("fadeIn", True)
                             ]
                        ]
                        [ text "Robot: Warning!"]
                  , p [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                             [ ("animated", True)
                             , ("fadeIn", model.millisclock > 1 )
                             ]
                        ]
                        [ text (if model.millisclock > 1 then "Thrusters fail to restart. Hanging-escaping system fails to restart..." else "") ]
                  , p [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                             [ ("animated", True)
                             , ("fadeIn", model.millisclock > 2 )
                             ]
                        ]
                        [ text (if model.millisclock > 2 then "Lost control of the craft." else "")]
                  , p [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                             [ ("animated", True)
                             , ("fadeIn", model.millisclock > 3 )
                             ]
                        ]
                        [ text (if model.millisclock > 3 then "Scientist: We are done..." else "") ]
                  , p [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                             [ ("animated", True)
                             , ("fadeIn", model.millisclock > 4 )
                             ]
                        , on "animationend" <| Decode.succeed <| Page Ending1p3
                        ]
                        [ text (if model.millisclock > 4 then "Engineer: What the hell are you doing? Can't you drive the spacecraft? Fuc..." else "")]
                  ]
            ]


renderEnding1p3 : Model -> Html Msg
renderEnding1p3 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ br [] [], br [] [], br [] [], br [] [], br [] [], br [] [], br [] [], br [] [], br [] []
                  , div [ Attr.style "font-size" "48px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                             [ ("animated", True)
                             , ("heartBeat", True)
                             ]
                        ]
                        [ text "Boom!" ]
                  , p [] [], p [] []
                  , div [ Attr.style "font-size" "56px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                             [ ("animated", True)
                             , ("heartBeat", model.millisclock > 1)
                             ]
                        ]
                        [ text (if model.millisclock > 1 then "Boom!" else "") ]
                  , p [] [], p [] []
                  , div [ Attr.style "font-size" "64px"
                         , Attr.style "text-align" "center"
                         , Attr.style "color" "rgb(255,255,255)"
                         , Attr.classList
                              [ ("animated", True)
                              , ("heartBeat", model.millisclock > 2)
                              ]
                         , on "animationend" <| Decode.succeed <| Page Ending1p4
                         ]
                         [ text (if model.millisclock > 2 then "Boom!" else "") ]

                  ]
            ]

renderEnding1p4 : Model -> Html Msg
renderEnding1p4 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div [ Attr.style "font-size" "45px"
                  , Attr.style "text-align" "center"
                  , Attr.style "color" "rgb(255,255,255)"
                  , Attr.classList
                        [ ("animated", True)
                        , ("fadeIn", True)
                        ]
                  ]
                  [ br[][], br[][], br[][], br[][], br[][]
                  , text "END1 DEAD"
                  ]
            , renderRestartbutton model.page model
            , renderExitbutton model

            ]

renderEnding2p1 : Model -> Html Msg
renderEnding2p1 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", True)
                            ]
                        ]
                        [ br [] [], br [] [], br [] [], br [] [], br [] []
                        , text "You successfully find a new habitable planet."
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 3)
                            ]
                        ]
                        [ if model.millisclock > 3 then
                                text "Human beings begin to establish civilization on this planet."
                          else
                                text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 6)
                            ]
                        ]
                        [ if model.millisclock > 6 then
                                text "Everything seems to be going smoothly..."
                          else
                                text ""
                        ]
                  , br [] [], br [] [], br [] [], br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 9)
                            ]
                        , on "animationend" <| Decode.succeed <| Page Ending2p2
                        ]
                        [ if model.millisclock > 9 then
                                text "Until one day..."
                          else
                                text ""
                        ]

                  ]
            ]


renderEnding2p2 : Model -> Html Msg
renderEnding2p2 model =
        div [ Attr.style "position" "fixed"
                    , Attr.style "top" "0"
                    , Attr.style "left" "0"
                    , Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "background" "rgb(0,0,0)"
                    ]
                    [ div []
                          [ div [ Attr.style "font-size" "26px"
                                , Attr.style "text-align" "center"
                                , Attr.style "color" "rgb(255,255,255)"
                                , Attr.classList
                                    [ ("animated", True)
                                    , ("fadeIn", True)
                                    ]
                                ]
                                [ br [] [], br [] [], br [] [], br [] [], br [] []
                                , text "The judgement day befall human beings again."
                                ]
                          , br [] [], br [] []
                          , div [ Attr.style "font-size" "26px"
                                , Attr.style "text-align" "center"
                                , Attr.style "color" "rgb(255,255,255)"
                                , Attr.classList
                                    [ ("animated", True)
                                    , ("fadeIn", model.millisclock > 3)
                                    ]
                                ]
                                [ if model.millisclock > 3 then
                                        text "A sudden outbreak of plague brought by an unknown virus kills life on the planet."
                                  else
                                        text ""
                                ]
                          , br [] [], br [] []
                          , div [ Attr.style "font-size" "26px"
                                , Attr.style "text-align" "center"
                                , Attr.style "color" "rgb(255,255,255)"
                                , Attr.classList
                                    [ ("animated", True)
                                    , ("fadeIn", model.millisclock > 6)
                                    ]
                                ]
                                [ if model.millisclock > 6 then
                                        text "The planet becomes a poisonous hell, which is no longer habitable."
                                  else
                                        text ""
                                ]
                          , br [] [], br [] [], br [] [], br [] []
                          , div [ Attr.style "font-size" "26px"
                                , Attr.style "text-align" "center"
                                , Attr.style "color" "rgb(255,255,255)"
                                , Attr.classList
                                    [ ("animated", True)
                                    , ("fadeIn", model.millisclock > 9)
                                    ]
                                , on "animationend" <| Decode.succeed <| Page Ending2p3
                                ]
                                [ if model.millisclock > 9 then
                                        text "In order to survive, the few remnants must escape the planet and find a new habitable one..."
                                  else
                                        text ""
                                ]

                          ]
                    ]

renderEnding2p3 : Model -> Html Msg
renderEnding2p3 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", True)
                            ]
                        ]
                  [ br [] [], br [] []
                  , text "END2 WHEEL OF DESTINY"
                  ]
                  , br [] [], br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 2 )
                            ]
                        ]
                        [ if model.millisclock > 2 then
                            text "The plague is a revenge from the low civilization for being robbed of food (fuel)."
                          else
                            text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 4)
                            ]
                        ]
                        [ if model.millisclock > 4 then
                            text "You have taken away the finest fuel resource, bringing the low civilization almost to extinct."
                          else
                            text ""
                        ]
                  , br [] [], br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 6)
                            ]
                        ]
                        [ if model.millisclock > 6 then
                            text "The gamma ray was a revenge from the high civilization."
                          else
                            text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 8)
                            ]
                        ]
                        [ if model.millisclock > 8 then
                            text "Millions of years ago, when they were underdeveloped,"
                          else
                            text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 10)
                            ]
                        ]
                        [ if model.millisclock > 10 then
                            text "wicked act of human beings had brought dreadful disaster to them."
                          else
                            text ""
                        ]
                  , renderRestartbutton model.page model
                  , renderExitbutton model

                  ]
            ]

renderEnding3p1 : Model -> Html Msg
renderEnding3p1 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", True)
                            ]
                        ]
                  [ br [] [], br [] [], br [] [], br [] [], br [] []
                  , text "Actually, the gamma ray was a revenge from the high civilization."
                  ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 3)
                            ]
                        ]
                  [ if model.millisclock > 3 then
                        text "Millions of years ago, when they were underdeveloped,"
                    else
                        text ""
                  ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 6)
                            ]
                        ]
                        [ if model.millisclock > 6 then
                            text "wicked act of human beings had brought dreadful disaster to them."
                          else
                            text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 9)
                            ]
                        ]
                        [ if model.millisclock > 9 then
                            text "Now, you human being turn out to be bullying the weak and catering to the strong again,"
                          else
                            text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 12)
                            ]
                        , on "animationend" <| Decode.succeed <| Page Ending3p2
                        ]
                        [ if model.millisclock > 12 then
                            text "which brings about your own destruction."
                          else
                            text ""
                        ]

                  ]

            ]


renderEnding3p2 : Model -> Html Msg
renderEnding3p2 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ div [ Attr.style "font-size" "36px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                                [ ("animated", True)
                                , ("fadeIn", True)
                                ]
                        ]
                  [ br[][], br[][], br[][], br[][], br [] []
                  , text "END3 SLAVED"
                  ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", True)
                            ]
                        ]
                        [ text "Unfortunately, you are enslaved and abused by the high civilization."
                        ]
                  , renderRestartbutton model.page model
                  , renderExitbutton model

                  ]
            ]

renderEnding4p1 : Model -> Html Msg
renderEnding4p1 model =
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgb(0,0,0)"
            ]
            [ div []
                  [ div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", True)
                            ]
                        ]
                        [ br [] [], br [] [], br [] []
                        , text "END4 HAPPY ENDING"
                        ]
                  , br [] [], br [] [], br [] [], br [][]
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 2)
                            ]
                        ]
                        [ if model.millisclock > 2 then
                                text "Congratulations!"
                          else
                                text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 4)
                            ]
                        ]
                        [ if model.millisclock > 4 then
                                text "You've successfully found a new habitable planet."
                          else
                                text ""
                        ]
                  , br [] [], br [] [], br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 6)
                            ]
                        ]
                        [ if model.millisclock > 6 then
                                text "Human beings begin to rebuild civilization on this planet."
                          else
                                text ""
                        ]
                  , br [] [], br [] []
                  , div [ Attr.style "font-size" "26px"
                        , Attr.style "text-align" "center"
                        , Attr.style "color" "rgb(255,255,255)"
                        , Attr.classList
                            [ ("animated", True)
                            , ("fadeIn", model.millisclock > 8)
                            ]
                        , on "animationend" <| Decode.succeed <| Reset Cover
                        ]
                        [ if model.millisclock > 8 then
                                text "Your name is remembered by people forever."
                          else
                                text ""
                        ]

                  ]
            ]
