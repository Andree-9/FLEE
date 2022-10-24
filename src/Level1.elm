module Level1 exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Model exposing (Model, Msg(..), Pagetype(..), Pressed(..), Status(..), Step(..))
import Time

renderTutorial : Model -> List (Html Msg)
renderTutorial model =
    case model.tutorial of
        Step1 ->
             [ text "Hello, Captain."
             , br [] []
             , text "Do you want a guideline to drive this spacecraft?"
             , br [] []
             , p [] []
             , text "Yes [ Press Y ] / No [ Press N ]"
             ]
        Step2 ->
            [ text "Look at the small map at top-right."
            , br [] []
            , text "It shows the position you are in the whole map and gives"
            , br [] []
            , text "hints on where to go."
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step3 ->
            [ text "There are two bars on the right."
            , br [] []
            , text "The red one shows the HP of the craft."
            , br [] []
            , text "The green one shows the fuel of the craft."
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step4 ->
            [ text "The box on the bottom-right shows the object at this stage."
            , br [] []
            , text "Now, your object is to follow the tutorial."
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step5 ->
            [ text "Now, use arrow keys to move around."
            , br [] []
            , text "This costs fuel and since it is limited, you need to use it wisely."
            , br [] []
            , text "[ Press ↑ ↓ ← → ]"
            ]
        Step6 ->
            [ text "See the dashed line around the white hole?"
            , br [] []
            , text "We are in its gravitational field. Make use of it."
            , br [] []
            , text "[ Press S ]"
            ]
        Step7 ->
            [ text "Good job! The craft has gained an extra speed and no fuel is cost."
            , br [] []
            , text "Now choose a proper time to escape."
            , br [] []
            , text "[ Press D ]"
            ]
        Step8 ->
            [ text "Great! You are getting on it!"
            , br [] []
            , text "Try to use the white hole again. And you will find the closer, the faster."
            , br [] []
            , text "Be careful not to crash the meteorite, which decrease the HP of the spacecraft."
            , br [] []
            , text "To get hung, [ Press S ]."
            ]
        Step9 ->
            [ text "If you find you are in the intersection of two fields of white holes, try to [ Press A ]"
            , br [] []
            , text "And you can switch from one white hole to another."
            , br [] []
            , text "To escape, [ Press D ]."
            ]
        Step10 ->
            [ text "Congratulations! You are ready for space travel."
            , br [] []
            , text "One more thing, getting hung in the green and orange circle will recover HP and fuel."
            , br [] []
            , text "Try them by yourself."
            , br [] []
            , text "[ Press Q ] to pause and open the menu page"
            ]
        _ ->
            []

changeTutorStep : Model -> (Step, Int)
changeTutorStep model =
    if model.gamepad.yes == StartPressed && model.tutorial == Step1 then
        (Step2, model.changeTimeLimit)
    else if model.gamepad.no == StartPressed && model.tutorial == Step1 then
        (StepNone, model.changeTimeLimit)
    else if model.tutorial == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step3,  Time.posixToMillis model.time)
    else if model.tutorial == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step4,  Time.posixToMillis model.time)
    else if model.tutorial == Step4 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000  then
        (Step5,  Time.posixToMillis model.time)
    else if model.tutorial == Step5 && model.status == Available then
        (Step6, model.changeTimeLimit)
    else if model.tutorial == Step6 && model.gamepad.hang == StartPressed then
        (Step7, model.changeTimeLimit)
    else if model.tutorial == Step7 && model.gamepad.escape == StartPressed then
        (Step8, model.changeTimeLimit)
    else if model.tutorial == Step8 && model.gamepad.hang == StartPressed then
        (Step9, model.changeTimeLimit)
    else if model.tutorial == Step9 && model.gamepad.escape == StartPressed then
        (Step10, model.changeTimeLimit)
    else if model.tutorial == Step10 && (model.gamepad.play == StartPressed || model.gamepad.stop == StartPressed) then
        (StepNone, model.changeTimeLimit)
    else
        (model.tutorial, model.changeTimeLimit)

checkEndMap1 : Model -> Bool
checkEndMap1 model =
    let
        cx = model.craft.cx
        cy = model.craft.cy
        r = 1200
    in
    if model.page == Map1 then
        if sqrt((cx - 14600) * (cx - 14600) + (cy - 12920) * (cy - 12920)) <= r then
            True
        else
            False
    else
        False


renderStory1text : Model -> List (Html Msg)
renderStory1text model =
    case model.story1text of
        Step1 ->
            [ text "Robot: Good job, captain! We have escaped the solar system!"
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step2 ->
            [ text "Robot: The white holes appear so densely in the galaxy ahead of us."
            , br [] []
            , text "[ Press W ] to continue."
            ]
        Step3 ->
            [ text "Robot: Maybe a civilization lives there and may give us some help."
            , br [] []
            , text "[ Press W ] to continue."
            ]
        _ ->
            []

changeS1text : Model -> (Step,Int)
changeS1text model =
    if model.story1text == Step1 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step2 , Time.posixToMillis model.time)
    else if model.story1text == Step2 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step3 , Time.posixToMillis model.time)
    else if model.story1text == Step3 && model.gamepad.play == StartPressed && abs(Time.posixToMillis model.time - model.changeTimeLimit ) > 2000 then
        (Step4, Time.posixToMillis model.time)
    else
        (model.story1text,model.changeTimeLimit)
