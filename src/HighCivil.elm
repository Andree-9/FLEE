module HighCivil exposing (..)

import Model exposing (Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..))
import Html exposing (Html, br, button, div, hr, p, pre, text)
import Html.Attributes as Attr exposing (attribute)
import Html.Events exposing (on, onClick)
import Time


renderHighFirsttext : Model -> List (Html Msg)
renderHighFirsttext model =
    case model.highfirsttext of
        Step1 ->
            [ text "Civilian: "
            , br [] []
            , text "I don't think I have seen you before. Where are you from?"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step2 ->
            [text "Scientist: "
            , br [] []
            , text "We escape the ray attack on Earth and are looking for a shelter……"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step3 ->
            [ text "Scientist: "
            , br [] []
            , text "There are so many white holes in this galaxy. Do you know why? "
            , br [] []
            , text "[Press W] to continue."
            ]
        Step4 ->
            [ text "Civilian: "
            , br [] []
            , text "We created them for transportation, sometimes as weapons."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step5 ->
            [ text "Civilian: "
            , br [] []
            , text "The main planet locates deepest in this galaxy where the dominants live."
            , br [] []
            , text "But I don't think they will help you…"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step6 ->
            [ text "The elderly civilian leaves."
            , br [] []
            , text "Scientist: "
            , br [] []
            , text "This alien civilization is more developed than us."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step7 ->
            [ text "Engineer: "
            , br [] []
            , text "Good news, isn't it? I can't wait to live in the big cities on their main planet."
            , br [] []
            , text "<Task : Land on the Main Planet>"
            , br [] []
            , text "[Press W] to continue."
            ]
        _ ->
            []


changeHighFirsttext : Model -> (Step,Int)
changeHighFirsttext model =
    if model.highfirsttext == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step2,  Time.posixToMillis model.time)
    else if model.highfirsttext == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step3 , Time.posixToMillis model.time)
    else if model.highfirsttext == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step4, Time.posixToMillis model.time)
    else if model.highfirsttext == Step4 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step5, Time.posixToMillis model.time)
    else if model.highfirsttext == Step5 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step6, Time.posixToMillis model.time)
    else if model.highfirsttext == Step6 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step7, Time.posixToMillis model.time)
    else if model.highfirsttext == Step7 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step8, Time.posixToMillis model.time)
    else
        (model.highfirsttext, model.changeTimeLimit)

-- continue game -> fly to the main planet

renderHighSecondtext : Model -> List (Html Msg)
renderHighSecondtext model =
    case model.highsecondtext of
        Step1 ->
            [ text "Dominant: "
            , br [] []
            , text "Delighted to welcome you, guests from the Earth!"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step2 ->
            [ text "Engineer: "
            , br [] []
            , text "......Our planet was destroyed and we have to find a new planet to live."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step3 ->
            [ text "Dominant: "
            , br [] []
            , text "Sorry to hear that. Don't worry! I can provide you with a shelter in the planet nearby."
            , br [] []
            , text "Whether you will accept his help?"
            , br [] []
            , text "[Press Y] to accept and [Press N] to refuse"
            ]
        Step4 ->
            [ text "Dominant: "
            , br [] []
            , text "Don't regret when you turn into hedgehogs!"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step5 ->
            [ text "Dominants: "
            , br [] []
            , text "Great! Wish you a happy journey!"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step6 ->
            [ text "Robot: "
            , br [] []
            , text "Look, some white holes turn red..."
            , br [] []
            , text "[Press W] to continue."
            ]
        _ ->
            []

changeHighSecondtext : Model -> (Step,Int)
changeHighSecondtext model =
    if model.highsecondtext == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step2,  Time.posixToMillis model.time)
    else if model.highsecondtext == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step3,  Time.posixToMillis model.time)
    else if model.highsecondtext == Step3 && model.gamepad.yes == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step5,  Time.posixToMillis model.time)
    else if model.highsecondtext == Step3 && model.gamepad.no == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step4,  Time.posixToMillis model.time)
    else if model.highsecondtext == Step4 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step6,  Time.posixToMillis model.time)
    else if model.highsecondtext == Step5 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step7, Time.posixToMillis model.time)
    else if model.highsecondtext == Step6 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step8, Time.posixToMillis model.time)
    else
        (model.highsecondtext, model.changeTimeLimit)


checkHigh1 : Model -> Bool
checkHigh1 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map2 then
        if sqrt((cx - 4230) * (cx - 4230) + (cy - 880) * (cy - 880)) <= 1.5 * 110 then
            True
        else
            False
    else
        False

checkHigh2 : Model -> Bool
checkHigh2 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map2 then
        if sqrt((cx - 3600) * (cx - 3600) + (cy - 20500) * (cy - 20500)) <= 1.5 * 400 then
            True
        else
            False
    else
        False

checkEndMap2 : Model -> Bool
checkEndMap2 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map2 then
        if sqrt(cx * cx + (cy - 7000) * (cy - 7000)) <= 1200 then
            True
        else
            False
    else
        False
