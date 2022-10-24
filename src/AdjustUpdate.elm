module AdjustUpdate exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
import Time
--<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.7.2/animate.min.css">
getHoleInRange : Model -> Int -> (List Hole, List Hole)
getHoleInRange model radius =
    List.partition (\ x -> sqrt((x.wx - model.craft.cx)^2 + (x.wy - model.craft.cy)^2) < toFloat radius) (model.updateHoles ++ model.theRestHoles)

adjustedUpdate : Model -> Model
adjustedUpdate model =
    let
        (holeList1, theRest) = getHoleInRange model model.craft.updateRange 
    in
    if model.updateTimeLimit < Time.posixToMillis model.time then
        {model | updateTimeLimit = Time.posixToMillis model.time + 800
        , updateHoles = holeList1, theRestHoles = theRest}
    else
        model

adjustHoleTime : Model -> Model
adjustHoleTime model =
    let 
        adjustedTime = model.holeTime + model.deltat
    in
        {model | holeTime = adjustedTime}