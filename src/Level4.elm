module Level4 exposing (..)


import Model exposing (Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..))
import Html exposing (Html, br, button, div, hr, p, pre, text)
import Html.Attributes as Attr exposing (attribute)
import Html.Events exposing (on, onClick)
import Time


renderStory3to4text : Model -> List (Html Msg)
renderStory3to4text model =
    case model.story3to4text of
        Step1 ->
            [ text "Robot: "
            , br [] []
            , text "Detecting...Strong signal of resources and living creatures!"
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step2 ->
            [ text "Robot: "
            , br [] []
            , text "There may be a Earth-like planet here. I have marked three possible planets for you."
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step3 ->
            [ text "Engineer: "
            , br [] []
            , text "Faster! Those idiots are right behind us!"
            , br [] []
            , text "[ Press W ] to continue."
            ]
        _ ->
            []

changeS3to4text : Model -> (Step,Int)
changeS3to4text model =
    if model.story3to4text == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step2,  Time.posixToMillis model.time)
    else if model.story3to4text == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step3,  Time.posixToMillis model.time)
    else if model.story3to4text == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step4,  Time.posixToMillis model.time)
    else
        (model.story3to4text, model.changeTimeLimit)

renderHabtext : Model -> List (Html Msg)
renderHabtext model =
    [ text "Scientist: "
    , br [] []
    , text "This is not the planet we are looking for..."
    ]

checkHab1 : Model -> Bool
checkHab1 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map4 then
        if sqrt((cx - -9976) * (cx - -9976) + (cy - 2670) * (cy - 2670)) <= 1.5 * 253 then
            True
        else
            False
    else
        False

checkHab2 : Model -> Bool
checkHab2 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map4 then
        if sqrt((cx - -7295) * (cx - -7295) + (cy - 7200) * (cy - 7200)) <= 1.5 * 255 then
            True
        else
            False
    else
        False

checkHab3 : Model -> Bool
checkHab3 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map4 then
        if sqrt((cx - -2635) * (cx - -2635) + (cy - 10060) * (cy - 10060)) <= 1.5 * 257 then
            True
        else
            False
    else
        False

