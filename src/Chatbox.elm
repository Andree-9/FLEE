module Chatbox exposing (..)

import HighCivil exposing (renderHighFirsttext, renderHighSecondtext)
import Level4 exposing (checkHab1, checkHab2, checkHab3, renderHabtext, renderStory3to4text)
import LowCivil exposing (renderStory2to3text, renderLowFirsttext, renderLowSecondtext)
import Level1 exposing (renderStory1text, renderTutorial)
import Model exposing (Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..), Boxstate(..))
import Html exposing (Html, br, button, div, hr, p, pre, text)
import Html.Attributes as Attr exposing (attribute)
import Html.Events exposing (on, onClick)

renderChatbox : Model -> Html Msg
renderChatbox model =
    div[Attr.style "position" "fixed"
       , Attr.style "left" "15%"
       , Attr.style "top" "65%"
       , Attr.style "width" "70%"
       , Attr.style "height" "30%"
       , Attr.style "background" "url(assets/chatbox.png) 0% 0% / 100% 100%"
       , Attr.classList
             [ ("animated", True)
             , ("fadeInUp", model.boxstate == BoxUp)
             , ("fadeOutDown", model.boxstate == BoxDown)
             ]
       ]
       [div [ Attr.style "position" "fixed"
            , Attr.style "left" "12%"
            , Attr.style "top" "20%"
            , Attr.style "width" "100%"
            , Attr.style "font-size" "23px"
            , Attr.style "color" "white"
            , Attr.classList
                [ ("animated", True)
                , ("fadeInUp", model.boxstate == BoxUp)
                , ("fadeOutDown", model.boxstate == BoxDown)
                ]
            ]
            (if model.page == Map1 then
                renderTutorial model
             else if model.page == Story1 then
                renderStory1text model
             else if model.page == HighFirst then
                renderHighFirsttext model
             else if model.page == HighSecond then
                renderHighSecondtext model
             else if model.page == Story2to3 then
                renderStory2to3text model
             else if model.page == Story3to4 then
                renderStory3to4text model
             else if model.page == LowFirst then
                renderLowFirsttext model
             else if model.page == LowSecond then
                renderLowSecondtext model
             else if model.page == Map4 && (checkHab1 model || checkHab2 model) then
                renderHabtext model
             else if (model.page == Map1 || model.page == Map2 || model.page == Map3 || model.page == Map4) && model.lowHP then
                [ text "Robot: Warning! The spacecraft is greatly damaged."
                , br [] []
                , text "Go to a repair station immediately!"
                ]
             else
                [])]


changeBoxState : Model -> Boxstate
changeBoxState model =
    case model.page of
        Map1 ->
            if model.gamepad.yes == StartPressed && model.tutorial == Step1 then
                BoxUp
            else if model.tutorial == StepNone then
                BoxDown
            else
                model.boxstate
        Story1 ->
            if model.story1text == Step1 then
                BoxUp
            else if model.story1text == Step4 then
                BoxDown
            else
                model.boxstate
        Map2 ->
            if model.lowHP then
                BoxUp
            else
                BoxDown
        HighFirst ->
            BoxUp
        HighSecond ->
            BoxUp
        Story2to3 ->
            BoxUp
        Map3 ->
            if model.lowHP then
                BoxUp
            else
                BoxDown
        LowFirst ->
            BoxUp
        LowSecond ->
            BoxUp
        Story3to4 ->
            BoxUp
        Map4 ->
            if checkHab1 model || checkHab2 model then
                BoxUp
            else if model.lowHP then
                BoxUp
            else
                BoxDown
        Inf1 ->
            BoxDown
        Inf2 ->
            BoxDown
        Inf3 ->
            BoxDown
        _ ->
            model.boxstate