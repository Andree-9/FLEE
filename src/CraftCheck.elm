module CraftCheck exposing (..)
import Model exposing (..)
import AdjustUpdate exposing (..)
import InitLevel1Part2 exposing (..)
import Time
craftCheck : Model -> Model
craftCheck model =
    if model.status == Free || model.status == Available then
        model 
            |> checkBrick
            |> changeStatus model.gamepad
            |> craftCheckPlanetin
    else
        model
            |> changeStatus model.gamepad
            --|> craftCheckPlanetin

{-craftCollide : Model -> Model --check if the craft collides with meteorite
craftCollide model =
    model
        |> checkBrick-}


checkBrick : Model -> Model
checkBrick model =
    let
        whiteholelist1 = List.filter ( \ x -> sqrt ((x.wx - model.craft.cx)^2 + (x.wy - model.craft.cy)^2)<150 && x.types == WhiteHole) model.updateHoles

        repairstationList = List.filter ( \ x -> sqrt ((x.wx - model.craft.cx)^2 + (x.wy - model.craft.cy)^2)<150 && x.types == Repairstation) model.updateHoles

        gasstationList = List.filter ( \ x -> sqrt ((x.wx - model.craft.cx)^2 + (x.wy - model.craft.cy)^2)<150 && x.types == Gasstation) model.updateHoles


    in
    if not (List.isEmpty whiteholelist1) {-&& model.craft.waveInfluence == False-} then
        {model | targetHole = Maybe.withDefault holeMaybe (List.head whiteholelist1)
                , status = Available}
    else if not (List.isEmpty repairstationList) {-&& model.craft.waveInfluence == False-} then
        {model | targetHole = Maybe.withDefault holeMaybe (List.head repairstationList)
                , status = Available}
    else if not (List.isEmpty gasstationList) {-&& model.craft.waveInfluence == False-} then
        {model | targetHole = Maybe.withDefault holeMaybe (List.head gasstationList)
                , status = Available}
    else
        {model | status = Free}

changeStatus : GamePad -> Model -> Model
changeStatus pad model =
    if model.status == Available && pad.hang == StartPressed then
        getHung { model | status = Hung, time_ = (Tuple.first model.time_, 0) }
    else if pad.escape == StartPressed then
        let
            model2 = cantRepair model
            craft1 = model.craft
        in
            { model2 | status = Free
                     , targetHole = holeMaybe
                     , time_ = (Tuple.first model.time_, 0)
                     , craft = {craft1 | time = 0}}
    else if pad.change == StartPressed && model.status == Hung then
        checkChangeHole model
    else
        model

cantRepair : Model -> Model 
cantRepair model =
    let
        (theHoleList, theRest) = List.partition (\ x -> sqrt ((x.wx - model.targetHole.wx)^2 + (x.wy- model.targetHole.wy)^2)< 10) model.updateHoles

        theHole = Maybe.withDefault holeMaybe (List.head theHoleList)

        adjustedHole = {theHole | canRepair = False} 

        adjustedHoleList = theRest ++ [adjustedHole]
    in
        {model | updateHoles = adjustedHoleList}

getHung : Model -> Model
getHung model =
    let 
        theta = atan ((model.targetHole.wy - model.craft.cy)/ (model.targetHole.wx - model.craft.cx)) *(180/3.1415926) 

        craft1 = model.craft
        
        dx = model.targetHole.wx - model.craft.cx

        dy = model.targetHole.wy - model.craft.cy

        r = sqrt(dx^2 + dy^2)

        speed = sqrt (15 * toFloat model.targetHole.mass/r) 
    in
    if model.targetHole.types == Repairstation && model.targetHole.canRepair == True then
        if Tuple.first model.craft.speed*sin (degrees theta) + Tuple.second model.craft.speed*cos (degrees theta) >= 0 then
            if model.craft.cx >= model.targetHole.wx then
                {model | craft = {craft1 | angle = theta, direction = 1, rotateRadius = r, rotateSpeed = speed}}
            else
                {model | craft = {craft1 | angle = theta + 180, direction = 1, rotateRadius = r, rotateSpeed = speed}}

        else
            if model.craft.cx >= model.targetHole.wx then
                {model | craft = {craft1 | angle = theta, direction = -1, rotateRadius = r, rotateSpeed = speed}}
            else
                {model | craft = {craft1 | angle = theta + 180, direction = -1, rotateRadius = r, rotateSpeed = speed}}
    else if model.targetHole.types == Gasstation && model.targetHole.canRepair == True then
        if Tuple.first model.craft.speed*sin (degrees theta) + Tuple.second model.craft.speed*cos (degrees theta) >= 0 then
            if model.craft.cx >= model.targetHole.wx then
                {model | craft = {craft1 | angle = theta, direction = 1, rotateRadius = r, rotateSpeed = speed}}
            else
                {model | craft = {craft1 | angle = theta + 180, direction = 1, rotateRadius = r, rotateSpeed = speed}}

        else
            if model.craft.cx >= model.targetHole.wx then
                {model | craft = {craft1 | angle = theta, direction = -1, rotateRadius = r, rotateSpeed = speed}}
            else
                {model | craft = {craft1 | angle = theta + 180, direction = -1, rotateRadius = r, rotateSpeed = speed}}
    else
        if Tuple.first model.craft.speed*sin (degrees theta) + Tuple.second model.craft.speed*cos (degrees theta) >= 0 then
            if model.craft.cx >= model.targetHole.wx then
                {model | craft = {craft1 | angle = theta, direction = 1, rotateRadius = r, rotateSpeed = speed}}
            else
                {model | craft = {craft1 | angle = theta + 180, direction = 1, rotateRadius = r, rotateSpeed = speed}}

        else
            if model.craft.cx >= model.targetHole.wx then
                {model | craft = {craft1 | angle = theta, direction = -1, rotateRadius = r, rotateSpeed = speed}}
            else
                {model | craft = {craft1 | angle = theta + 180, direction = -1, rotateRadius = r, rotateSpeed = speed}}

craftCheckPlanetin : Model -> Model 
craftCheckPlanetin model =
    let
        planetin = List.filter (\ x -> sqrt ((x.px - model.craft.cx)^2 + (x.py - model.craft.cy)^2) < toFloat x.range) model.planetList
        craft1 = model.craft
        thePlanet = Maybe.withDefault { px = 0, py = 0, range = 1000, rplanet = 100, speed = (0, 0), mass = 3000, id = 0} (List.head (planetin ++ [model.coreplanet]))
    in  
    if not (List.isEmpty planetin) then
        {model | craft = {craft1 | planetin = thePlanet}}
    else
        {model | craft = {craft1 | planetin = model.coreplanet}}

checkChangeHole : Model -> Model
checkChangeHole model =
    let
        theRestHoles = List.filter (\ x -> sqrt ((x.wx - model.targetHole.wx)^2 + (x.wy - model.targetHole.wy)^2) > 3) model.updateHoles
        newTargetHole = List.filter (\ x -> sqrt ((x.wx - model.craft.cx)^2 + (x.wy - model.craft.cy)^2)<150 && x.types /= Meteorite && x.types /= AttackHole) theRestHoles
        targetHoleHead = Maybe.withDefault holeMaybe (List.head newTargetHole)
        model1 = {model | targetHole = targetHoleHead}
        craft1 = model.craft
        adjcraft = {craft1 | time = 0}
        model2 = {model1 | craft = adjcraft, changeTimeLimit = Time.posixToMillis model.time}
    in
    if not (List.isEmpty newTargetHole) && abs (Time.posixToMillis model.time - model.changeTimeLimit) > 900 then
        getHung model2
    else
        model
            
