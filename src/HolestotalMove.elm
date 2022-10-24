module HolestotalMove exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)

holestotalMove : Model -> Model
holestotalMove model =
    if model.status == Free || model.status == Available then
        model 
            |> holeRotateMove
            |> holeFreeMove
    else
        model 
            |> holeRotateMove
            |> holeFreeMove
            |> targetRotateMove


holeRotateData : List Hole -> List Hole -> Float -> List Hole
holeRotateData holes adjholes holeTime =
    if List.isEmpty holes then
        adjholes
    else
    let
        theHole = Maybe.withDefault holeMaybe (List.head holes)
        cx1 = theHole.planetin.px + theHole.radius * cos (degrees (theHole.angle + toFloat 125 * 15 / toFloat theHole.planetin.mass * holeTime * theHole.speedmag))
        cy1 = theHole.planetin.py + theHole.radius * sin (degrees (theHole.angle + toFloat 125 * 15 / toFloat theHole.planetin.mass * holeTime * theHole.speedmag))
        vx = -(theHole.radius * 125 /180 * 3.1415 * 15 / toFloat theHole.planetin.mass * theHole.speedmag) * sin (degrees (theHole.angle + toFloat 125 * 15 / toFloat theHole.planetin.mass * holeTime * theHole.speedmag))
        vy = (theHole.radius * 125 /180 * 3.1415 * 15 / toFloat theHole.planetin.mass * theHole.speedmag) * cos (degrees (theHole.angle + toFloat 125 * 15 / toFloat theHole.planetin.mass * holeTime * theHole.speedmag))
        v1 = (vx, vy)
        theHole1 = {theHole | speed = v1, wx = cx1, wy = cy1}
        
        adjustedList = adjholes ++ [theHole1]

    in
        holeRotateData (List.drop 1 holes) adjustedList holeTime

holeRotateMove : Model -> Model
holeRotateMove model =
    let
        (rotateList, theRest) = List.partition (\ x -> x.status == Rotate) model.updateHoles
        holes1 = holeRotateData rotateList [] model.holeTime
    in
        {model | updateHoles = holes1 ++ theRest}

holeFreeMove : Model -> Model
holeFreeMove model =
    let
        (freeList, theRest) = List.partition (\ x -> x.status == FreeRun) model.updateHoles
        newxlist = List.map (\ x -> {x | wx = x.wx + Tuple.first x.speed * model.deltat, wy = x.wy + Tuple.second x.speed * model.deltat}) freeList 
    in
        {model | updateHoles = newxlist ++ theRest}

targetRotateMove : Model -> Model 
targetRotateMove model =
    let
        updatedTarget = holeRotateData [model.targetHole] [] model.holeTime
        target1 = Maybe.withDefault holeMaybe (List.head updatedTarget)
    in
        {model | targetHole = target1}