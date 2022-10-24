module LowCivil exposing (..)

import Model exposing (Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..))
import Html exposing (Html, br, button, div, hr, p, pre, text)
import Html.Attributes as Attr exposing (attribute)
import Html.Events exposing (on, onClick)
import Time


renderStory2to3text : Model -> List (Html Msg)
renderStory2to3text model =
    case model.story2to3text of
        Step1 ->
            [ text "Robot: "
            , br [] []
            , text "Detecting...The nearest galaxy is southeast of the current position."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step2 ->
            [ text "Robot: "
            , br [] []
            , text "Analyzing...The fuel left only enables the spacecraft to reach the galaxy."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step3 ->
            [ text "Robot: "
            , br [] []
            , text "Result : There is no choice but to look for opportunities in the nearest galaxy."
            , br [] []
            , text "[Press W] to continue."
            ]
        _ ->
            []

changeS2to3text : Model -> (Step,Int)
changeS2to3text model =
    if model.story2to3text == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step2,  Time.posixToMillis model.time)
    else if model.story2to3text == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step3,  Time.posixToMillis model.time)
    else if model.story2to3text == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step4,  Time.posixToMillis model.time)
    else
        (model.story2to3text, model.changeTimeLimit)


renderLowFirsttext : Model -> List (Html Msg)
renderLowFirsttext model =
    case model.lowfirsttext of
        Step1 ->
            [ text "Robot: "
            , br [] []
            , text "Detecting... Analyzing..."
            , br [] []
            , text "Result : This planet stores a small amount of fuel resource."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step2 ->
            [text "......"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step3 ->
            [ text "With the robot’s prompt, you get some fuel supplies. " -- add fuel
            , br [] []
            , text "However, it cannot hold up long."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step4 ->
            [ text "Scientist: "
            , br [] []
            , text "According to the data collected by robot, I speculate that there may also be fuel in other planets in this galaxy. Maybe we can have a try."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step5 ->
            [ text "Engineer: "
            , br [] []
            , text "Ha! Let’s just say with this little fuel, we have no other choice."
            , br [] []
            , text "[Press W] to continue."
            ]
        _ ->
            []

changeLowFirsttext : Model -> (Step,Int)
changeLowFirsttext model =
    if model.lowfirsttext == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step2,  Time.posixToMillis model.time)
    else if model.lowfirsttext == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step3,  Time.posixToMillis model.time)
    else if model.lowfirsttext == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step4,  Time.posixToMillis model.time)
    else if model.lowfirsttext == Step4 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step5,  Time.posixToMillis model.time)
    else if model.lowfirsttext == Step5 && model.gamepad.play == StartPressed then
        (Step6, Time.posixToMillis model.time)
    else
        (model.lowfirsttext, model.changeTimeLimit)

-- continue game -> fly to specific planet

renderLowSecondtext : Model -> List (Html Msg)
renderLowSecondtext model =
    case model.lowsecondtext of
        Step1 ->
            [ text "Robot: "
            , br [] []
            , text "Detecting... Analyzing..."
            , br [] []
            , text "Result : Fuel resources exist. Living creature exist..."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step2 ->
            [ text "Scientist: "
            , br [] []
            , text "Oh god! What are those barbarians doing to our gas tank?"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step3 ->
            [ text "Engineer: "
            , br [] []
            , text "Jesus! We must leave them! Quickly set off!"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step4 ->
            [ text "Robot: "
            , br [] []
            , text "Fuel resource in the southwest."
            , br [] []
            , text "Suggestion : Go southwest and acquire more fuel."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step5 ->
            [ text "......"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step6 ->
            [ text "Scientist: "
            , br [] []
            , text "That’s horrible... They drink fuel..."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step7 ->
            [ text "Engineer: "
            , br [] []
            , text "We should get some from them!"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step8 ->
            [ text "Scientist: "
            , br [] []
            , text "Don’t you see the bodies along the road? Just skin and bones..."
            , br [] []
            , text "Their fuel must be limited, too. And they live on that..."
            , br [] []
            , text "[Press W] to continue."
            ]
        Step9 ->
            [ text "Engineer: "
            , br [] []
            , text "So what? You don’t get fuel, you want to die in this place?"
            , br [] []
            , text "[Press W] to continue."
            ]
        Step10 ->
            [ text "Do you choose to rob the fuel or not?"
            , br [] []
            , text "[Press Y] to rob or [Press N] to not rob."
            ]
        _ ->
            []

changeLowSecondtext : Model -> (Step,Int)
changeLowSecondtext model =
    if model.lowsecondtext == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step2,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step3,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step4,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step4 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step5,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step5 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step6,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step6 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step7,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step7 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step8,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step8 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step9,  Time.posixToMillis model.time)
    else if model.lowsecondtext == Step9 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step10, Time.posixToMillis model.time)
    else if model.lowsecondtext == Step10 && model.gamepad.yes == StartPressed then
        (Step11, Time.posixToMillis model.time)
    else if model.lowsecondtext == Step10 && model.gamepad.no == StartPressed then
        (Step12, Time.posixToMillis model.time)
    else
        (model.lowsecondtext, model.changeTimeLimit)


checkLow1 : Model -> Bool
checkLow1 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map3 then
        if sqrt((cx - 15234) * (cx - 15234) + (cy - 10503) * (cy - 10503)) <= 1.3 * 163 then
            True
        else
            False
    else
        False

checkLow2 : Model -> Bool
checkLow2 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map3 then
        if sqrt((cx - 8068) * (cx - 8068) + (cy - 5795) * (cy - 5795)) <= 1.5 * 162 then
            True
        else
            False
    else
        False

checkEndMap3 : Model -> Bool
checkEndMap3 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
    in
    if model.page == Map3 then
        if cx > 2760 && cx < 4520 && cy > 14906 && cy < 15220 && cy > (19 / 22) * (cx - 2760) + 13700 then
            True
        else
            False
    else
        False