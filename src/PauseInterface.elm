module PauseInterface exposing (..)

import Button exposing (renderExitbutton, renderRestartbutton)
import HighCivil exposing (checkEndMap2, checkHigh1, checkHigh2)
import Level1 exposing (checkEndMap1)
import LowCivil exposing (checkEndMap3, checkLow1, checkLow2)
import Model exposing (..)
import Html exposing (Html, br, button, div, hr, p, pre, text)
import Html.Attributes as Attr exposing (attribute)


renderPaused : Model -> Html Msg
renderPaused model =
    if model.menustate /= MenuNone then
        div [ Attr.style "position" "fixed"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" "40%"
            , Attr.style "height" "100%"
            , Attr.style "background" "rgba(255,255,255,0.6)"
            , Attr.classList
                [ ("animated", True)
                , ("slideInLeft", model.menustate == MenuOut)
                , ("slideOutLeft", model.menustate == MenuIn)
                ]
            ]
            [ div [ Attr.style "font-size" "26px"
                  , Attr.style "text-align" "center"
                  ]
                  [ br [] []
                  , text (pLeveltext model)
                  , br [] []
                  , br [] []
                  ]
            , div [ Attr.style "font-size" "26px"
                  , Attr.style "position" "fixed"
                  , Attr.style "left" "2.5%"
                  ]
                  [ text (pLevelDb model)
                  , br [] []
                  , br [] []
                  , text "[ Press W ] to resume."
                  , br [] []
                  , text "[ Press Q ] to pause."
                  , br [] []
                  , text "[ Press S ] to hang on the white hole."
                  , br [] []
                  , text "[ Press D ] to drop the white hole."
                  , br [] []
                  , text "[ Press A ] to switch to adjacent white hole."
                  , br [] []
                  , text "[ Press E ] to enable invincible mode."
                  , br [] []
                  , text "----(Only for experiencing the game)"
                  , br [] []
                  , text "[ Press R ] to disable invincible mode."
                  , br [] []
                  , text "----(Challenging but more interesting)"
                  ]
            , renderExitbutton model
            , renderRestartbutton model.page model
            ]
    else
        div [] []

pLeveltext: Model -> String
pLeveltext model =
    if model.page == Map1 then
        if model.tutorial == StepNone then
            "Solar System"
        else
            "Tutorial Level"
    else if model.page == Map2 then
        "High Civilization"
    else if model.page == Map3 then
        "Low Civilization"
    else if model.page == Map4 then
        "Habitable Planet Map"
    else if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then
        "Endless Level"
    else
        ""

pLevelDb: Model -> String
pLevelDb model =
    if model.page == Map1 then
        if model.tutorial == StepNone then
            "    In this stage, your goal is to flee the solar system. Be sure to make good use of the white holes, or you will soon run out of fuel."
        else
            "    In this stage, you will learn how to control the spacecraft and how to make use of the white holes in your way!"
    else if model.page == Map2 then
        "    Now, you come in the galaxy of a high civilization. Be careful about what the NPC tells you. Make your decision thoughtfully."
    else if model.page == Map3 then
        "    You enter another galaxy. In this galaxy, you may encounter some opportunities. Make your choice carefully! It will decide your desitiny and even the fate of the human civilization."
    else if model.page == Map4 then
        "    Chaser Warning! The robot has told you the position of the three possible planets that may be habitable. Find the correct one as soon as possible!"
    else if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then
        "    This is the endless mode. Try to go as high as possible! Good luck!"
    else
        ""

changeMenuState : Model -> Menustate
changeMenuState model =
       if model.page == Map1 && model.tutorial /= Step7 && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == Map1 && checkEndMap1 model then
            MenuNone
       else if model.page == Story1 && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.gamepad.play == StartPressed && model.menustate == MenuOut then
            MenuIn
       else if model.page == Story1 && model.story1text == Step4 then
            MenuNone
       else if model.page == Map2 && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == Map2 && checkHigh1 model then
            MenuNone
       else if model.page == HighFirst && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == HighFirst && model.highfirsttext == Step8 then
            MenuNone
       else if model.page == Map2 && checkHigh2 model then
            MenuNone
       else if model.page == HighSecond && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == HighSecond && (model.highsecondtext == Step7 || model.highsecondtext == Step8) then
            MenuNone
       else if model.page == Map2 && checkEndMap2 model then
            MenuNone
       else if model.page == Story2to3 && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == Map3 && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == Map3 && checkLow1 model then
            MenuNone
       else if model.page == LowFirst && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == LowFirst && model.lowfirsttext == Step6 then
            MenuNone
       else if model.page == Map3 && checkLow2 model then
            MenuNone
       else if model.page == LowSecond && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == LowSecond && model.lowsecondtext == Step11 then
            MenuNone
       else if model.page == Map3 && checkEndMap3 model then
            MenuNone
       else if model.page == Story3to4 && model.gamepad.stop == StartPressed then
            MenuOut
       else if model.page == Map4 && model.gamepad.stop == StartPressed then
            MenuOut
       else if (model.page == Inf1 || model.page == Inf2 || model.page == Inf3) && model.gamepad.stop == StartPressed then
            MenuOut
       else
            model.menustate