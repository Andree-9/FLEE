module HolestotalCollide exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
import CrafttotalMove exposing (..)
--import HolesAttack exposing (model1)
--import HolesAttack exposing (theRest)
--import HolesAttack exposing (theRest)
--import Model exposing (HoleType(..))


holestotalCollide : Model -> Model
holestotalCollide model =
    let
        freeList = List.filter (\ x -> x.status == FreeRun) model.updateHoles
        --model1 = holesCollide freeList model
        model1 = 
            model
            |> craftCollideHoles
            |> holesCollidePlanet 
            |> holesCollide freeList
    in
    if model.ifInfinity == True then
        model1
            |> infinityholesCollideWall
    else if model.ifInfinity == False && model.page == Map4 then
        model1
            |> map4holesCollideWall
    else if model.ifInfinity == False && model.page == Map3 then
        model1
            |> map3holesCollideWall
    else if model.ifInfinity == False && model.page == Map2 then
        model1
            |> map2holesCollideWall
    else if model.ifInfinity == False && model.page == Map1 then
        model1
            |> map1holesCollideWall
    else
        model1

craftCollideHoles : Model -> Model
craftCollideHoles model =
    let
        (chosedHoles, theRest1) = List.partition (\ x -> sqrt ((model.craft.cx - x.wx)^2 + (model.craft.cy - x.wy)^2) <= toFloat (x.selfRange + model.craft.rcraft)) model.updateHoles

        (targetHoleList, theRest) = List.partition (\ x -> (model.craft.cx - x.wx) * Tuple.first model.craft.speed + (model.craft.cy - x.wy) * Tuple.second model.craft.speed < 0 ) chosedHoles
    
        (metoriteList1, whiteHoleList1) = List.partition (\ x -> x.types == Meteorite) targetHoleList 

        changedHoleList = List.map (\ x -> { x | speed = (toFloat (x.mass - model.craft.mass)/toFloat (x.mass + model.craft.mass)* Tuple.first x.speed + (2*toFloat model.craft.mass/toFloat (model.craft.mass + x.mass))* Tuple.first model.craft.speed
                                        , toFloat (x.mass - model.craft.mass)/toFloat (x.mass + model.craft.mass)* Tuple.second x.speed + (2*toFloat model.craft.mass/toFloat (model.craft.mass + x.mass))* Tuple.second model.craft.speed)
                                        , status = FreeRun}) metoriteList1
        
        collideHole = Maybe.withDefault holeMaybe (List.head targetHoleList )
        collideHole1 = Maybe.withDefault holeMaybe (List.head chosedHoles )
        dmag = sqrt ((model.craft.cx - collideHole1.wx)^2 + (model.craft.cy - collideHole1.wy)^2)

        vx = toFloat (model.craft.mass - collideHole.mass)/toFloat (model.craft.mass + collideHole.mass)/6* Tuple.first model.craft.speed + (2*toFloat collideHole.mass/toFloat (model.craft.mass + collideHole.mass))/6* Tuple.first collideHole.speed

        vy = toFloat (model.craft.mass - collideHole.mass)/toFloat (model.craft.mass + collideHole.mass)/6* Tuple.second model.craft.speed + (2*toFloat collideHole.mass/toFloat (model.craft.mass + collideHole.mass))/6* Tuple.second collideHole.speed

        vmag = sqrt(vx^2 + vy^2)
        craft1 = model.craft
        dx = (model.craft.cx - collideHole1.wx) / dmag *3
        dy = (model.craft.cy - collideHole1.wy) / dmag *3
        adjustedCraft = { craft1 | speed = (toFloat (model.craft.mass - collideHole.mass)/toFloat (model.craft.mass + collideHole.mass)/6* Tuple.first model.craft.speed + (2*toFloat collideHole.mass/toFloat (model.craft.mass + collideHole.mass))/6* Tuple.first collideHole.speed
                                        , toFloat (model.craft.mass - collideHole.mass)/toFloat (model.craft.mass + collideHole.mass)/6* Tuple.second model.craft.speed + (2*toFloat collideHole.mass/toFloat (model.craft.mass + collideHole.mass))/6* Tuple.second collideHole.speed)
                                        , hp = craft1.hp - vmag/20, cx = craft1.cx + dx, cy = craft1.cy + dy, time = 0} 

        adjustedHoles =  whiteHoleList1 ++ theRest ++ changedHoleList ++ theRest1
        --freeHoleNotCounted = List.filter (\ x -> not (List.member x model.freeHoles)) changedHoleList
    in
    if not (List.isEmpty targetHoleList) && craft1.waveInfluence == True then
        {model | craft = adjustedCraft, updateHoles = adjustedHoles, status = Free, waveHit = True}
    else if not (List.isEmpty targetHoleList) && craft1.waveInfluence == False then
        {model | craft = adjustedCraft, updateHoles = adjustedHoles, status = Free, waveHit = False}
    else
        {model | waveHit = False}

holesCollidePlanet : Model -> Model
holesCollidePlanet model =
    let
        (freeList, theRest1) = List.partition (\ x -> x.status == FreeRun) model.updateHoles

        (hitList, theRest) = List.partition (\ x -> sqrt ((x.planetin.px - x.wx)^2 + (x.planetin.py - x.wy)^2) <= toFloat (x.selfRange + x.planetin.rplanet)  
                                                    && (x.planetin.px - x.wx) * Tuple.first x.speed + (x.planetin.py - x.wy) * Tuple.second x.speed > 0 ) freeList
        changedHitList = List.map (\ x -> {x | speed = (-(Tuple.first x.speed), -(Tuple.second x.speed))}) hitList
    in
        {model | updateHoles = changedHitList ++ theRest ++ theRest1}

holesCollide :  List Hole -> Model -> Model
holesCollide holes model =    --holes --freeHoles
    if List.length holes == 0 then
        model
    else
    let
        theHole = Maybe.withDefault holeMaybe (List.head holes)

        modelholeList = List.filter (\ x -> sqrt ((x.wx - theHole.wx)^2 + (x.wy - theHole.wy)^2)> 10) model.updateHoles
        holelist = List.drop 1 holes
        (cddHoleList, theRest1) = List.partition (\ x -> sqrt ((x.wx - theHole.wx)^2 + (x.wy - theHole.wy)^2)< toFloat (x.selfRange + theHole.selfRange) && x.types == Meteorite) modelholeList

        (targetHoleList, therest) = List.partition (\ x -> (theHole.wx - x.wx) * Tuple.first theHole.speed + (theHole.wy - x.wy) * Tuple.second theHole.speed < 0) cddHoleList
        
        theHole2 = Maybe.withDefault holeMaybe (List.head cddHoleList)

        dmag = sqrt ((theHole2.wx - theHole.wx)^2 + (theHole2.wy - theHole.wy)^2)
        dx = (theHole.wx - theHole2.wx) / dmag * 5
        dy = (theHole.wy - theHole2.wy) / dmag * 5

        (metoriteList1, whiteHoles1) = List.partition (\ x -> x.types == Meteorite) targetHoleList

        changedHoleList = List.map (\ x -> { x | speed = (-(Tuple.first x.speed)
                                        , -(Tuple.second x.speed))
                                        , status = FreeRun}) metoriteList1
        theHole1 = Maybe.withDefault holeMaybe (List.head targetHoleList)
        adjustedHole = { theHole | speed = (-(Tuple.first theHole.speed)
                                        , -(Tuple.second theHole.speed))
                                        , status = FreeRun, wx = theHole.wx + dx, wy = theHole.wy + dy} 
        
        newHoleList = therest ++ whiteHoles1 ++ changedHoleList ++ theRest1 ++ [adjustedHole] 
        --coreplanet1 = model.coreplanet
        model1 = {model | updateHoles = newHoleList}
        --model2 = {model | holes = therest ++ whiteHoles1, freeHoles = newFreeHoleList2}
        --model2 = {model | holes = newHoles ++ [theHole]}
    in 
    if not (List.isEmpty targetHoleList) then
        holesCollide holelist model1 
    else 
        holesCollide holelist model 

infinityholesCollideWall : Model -> Model
infinityholesCollideWall model =
    let
        (holesAtUp,theRest) = List.partition (\ x -> x.wy < 5 && Tuple.second x.speed < 0 && x.types == Meteorite) model.updateHoles

        (holesAtDown, theRest1) = List.partition (\ x -> x.wy > toFloat model.mapHeight && Tuple.second x.speed > 0 && x.types == Meteorite) theRest

        (holesAtLeft, theRest2) = List.partition (\ x -> x.wx < 5 && Tuple.first x.speed < 0 && x.types == Meteorite) theRest1

        (holesAtRight, theRest3) = List.partition (\ x -> x.wx > toFloat model.mapWidth && Tuple.first x.speed > 0 && x.types == Meteorite) theRest2
        adjustedHolesUp = List.map (\ x -> {x | speed = (Tuple.first x.speed,-(Tuple.second x.speed))}) (holesAtUp ++ holesAtDown)
        adjustedHolesLR = List.map (\ x -> {x | speed = (-(Tuple.first x.speed), Tuple.second x.speed)}) (holesAtLeft ++ holesAtRight)
    in
        {model | updateHoles = adjustedHolesUp ++ adjustedHolesLR ++ theRest3}

map4holesCollideWall : Model -> Model 
map4holesCollideWall model =
    let
        (holesOut, theRest) = List.partition (\ x -> sqrt (x.wx^2 + x.wy^2) > 13000 - 35 
            && Tuple.first x.speed*x.wx + Tuple.second x.speed*x.wy >=0 && x.types == Meteorite) model.updateHoles

        (holesRight, theRest1) = List.partition (\ x -> x.wx > -30 && Tuple.first x.speed > 0 && x.types == Meteorite) theRest

        (holesUp, theRest2) = List.partition (\ x -> x.wy < 30 && Tuple.second x.speed < 0 && x.types == Meteorite) theRest1

        adjustHolesOut = List.map (\ x -> {x | speed = adjustVector (x.wy, -x.wx) x.speed}) holesOut

        adjustHolesRight = List.map (\ x -> {x | speed = (-(Tuple.first x.speed), Tuple.second x.speed)}) holesRight

        adjustHolesUp = List.map (\ x -> {x | speed = (Tuple.first x.speed, -(Tuple.second x.speed))}) holesUp
    in
        {model | updateHoles = adjustHolesOut ++ adjustHolesRight ++ adjustHolesUp ++ theRest2} 
        

map3holesCollideWall : Model -> Model
map3holesCollideWall model =
    let
        (n1x,n1y) = (-1.02,-1)
        (w1x,w1y) = (1,-1.02)
        (n2x,n2y) = (-3.54,1)
        (w2x,w2y) = (1,3.54)
        (n3x,n3y) = (-0.25,1)
        (w3x,w3y) = (1,0.25)
        (n4x,n4y) = (1.036,1)
        (w4x,w4y) = (-1,1.036)
        (n5x,n5y) = (3.57,-1)
        (w5x,w5y) = (1,3.57)
        (n6x,n6y) = (0.25,-1)
        (w6x,w6y) = (1,0.25)
        (holesOut1, theRest) = List.partition (\ x -> abs(1.02*x.wx + x.wy - 7732.8)/1.428 < 50 && Tuple.first x.speed * n1x + Tuple.second x.speed * n1y > 0 && x.types == Meteorite) model.updateHoles

        (holesOut2, theRest1) = List.partition (\ x -> abs(3.54*x.wx - x.wy + 3856.2)/3.68 < 50 && Tuple.first x.speed * n2x + Tuple.second x.speed * n2y > 0 && x.types == Meteorite) theRest

        (holesOut3, theRest2) = List.partition (\ x -> abs(0.25*x.wx - x.wy + 14112.9)/1.03 < 50 && Tuple.first x.speed * n3x + Tuple.second x.speed * n3y > 0 && x.types == Meteorite) theRest1

        (holesOut4, theRest3) = List.partition (\ x -> abs(1.036*x.wx + x.wy - 28599.5)/1.44 < 50 && Tuple.first x.speed * n4x + Tuple.second x.speed * n4y > 0 && x.types == Meteorite) theRest2

        (holesOut5, theRest4) = List.partition (\ x -> abs(3.57*x.wx - x.wy - 49912.8)/3.7 < 50  && Tuple.first x.speed * n5x + Tuple.second x.speed * n5y > 0 && x.types == Meteorite) theRest3

        (holesOut6, theRest5) = List.partition (\ x -> abs(0.25*x.wx - x.wy - 784.3)/1.03 < 50 && Tuple.first x.speed * n6x + Tuple.second x.speed * n6y > 0 && x.types == Meteorite) theRest4

        adjustHoles1 = List.map (\ x -> {x | speed = adjustVector (w1x,w1y) x.speed}) holesOut1

        adjustHoles2 = List.map (\ x -> {x | speed = adjustVector (w2x,w2y) x.speed}) holesOut2

        adjustHoles3 = List.map (\ x -> {x | speed = adjustVector (w3x,w3y) x.speed}) holesOut3

        adjustHoles4 = List.map (\ x -> {x | speed = adjustVector (w4x,w4y) x.speed}) holesOut4

        adjustHoles5 = List.map (\ x -> {x | speed = adjustVector (w5x,w5y) x.speed}) holesOut5

        adjustHoles6 = List.map (\ x -> {x | speed = adjustVector (w6x,w6y) x.speed}) holesOut6
    in
        {model | updateHoles = adjustHoles1 ++ adjustHoles2 ++ adjustHoles3 ++ adjustHoles4 ++ adjustHoles5 ++ adjustHoles6 ++ theRest5}


map2holesCollideWall : Model -> Model
map2holesCollideWall model =
    let
        (holesAtUp,theRest) = List.partition (\ x -> x.wy < 5 && Tuple.second x.speed < 0 && x.types == Meteorite) model.updateHoles

        (holesAtDown, theRest1) = List.partition (\ x -> x.wy > 20495 && Tuple.second x.speed > 0 && x.types == Meteorite) theRest

        (holesAtLeft, theRest2) = List.partition (\ x -> x.wx < 5 && Tuple.first x.speed < 0 && x.types == Meteorite) theRest1

        (holesAtRight, theRest3) = List.partition (\ x -> x.wx > toFloat model.mapWidth-20 && Tuple.first x.speed > 0 && x.types == Meteorite) theRest2

        adjustedHolesUp = List.map (\ x -> {x | speed = (Tuple.first x.speed,-(Tuple.second x.speed))}) (holesAtUp ++ holesAtDown)

        adjustedHolesLR = List.map (\ x -> {x | speed = (-(Tuple.first x.speed), Tuple.second x.speed)}) (holesAtLeft ++ holesAtRight)
    in
        {model | updateHoles = adjustedHolesUp ++ adjustedHolesLR ++ theRest3}

map1holesCollideWall : Model -> Model
map1holesCollideWall model =
    let
        (holesAtUp,theRest) = List.partition (\ x -> x.wy < 5 && Tuple.second x.speed < 0 && x.types == Meteorite) model.updateHoles

        (holesAtDown, theRest1) = List.partition (\ x -> x.wy > 13120 && Tuple.second x.speed > 0 && x.types == Meteorite) theRest

        (holesAtLeft, theRest2) = List.partition (\ x -> x.wx < 5 && Tuple.first x.speed < 0 && x.types == Meteorite) theRest1

        (holesAtRight, theRest3) = List.partition (\ x -> x.wx > 14785 && Tuple.first x.speed > 0 && x.types == Meteorite) theRest2

        adjustedHolesUp = List.map (\ x -> {x | speed = (Tuple.first x.speed,-(Tuple.second x.speed))}) (holesAtUp ++ holesAtDown)

        adjustedHolesLR = List.map (\ x -> {x | speed = (-(Tuple.first x.speed), Tuple.second x.speed)}) (holesAtLeft ++ holesAtRight)
    in
        {model | updateHoles = adjustedHolesUp ++ adjustedHolesLR ++ theRest3}


{-let
        craft1 = model.craft
        cx = model.craft.cx
        cy = model.craft.cy
        (n1x,n1y) = (-1.02,-1)
        (w1x,w1y) = (1,-1.02)
        (n2x,n2y) = (-3.54,1)
        (w2x,w2y) = (1,3.54)
        (n3x,n3y) = (-0.25,1)
        (w3x,w3y) = (1,0.25)
        (n4x,n4y) = (1.036,1)
        (w4x,w4y) = (-1,1.036)
        (n5x,n5y) = (3.57,-1)
        (w5x,w5y) = (1,3.57)
        (n6x,n6y) = (0.25,-1)
        (w6x,w6y) = (1,0.25)

    in
    if model.ifInfinity == True then
        if model.craft.cx > toFloat (model.mapWidth - 5) && Tuple.first model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if model.craft.cx < 5 && Tuple.first model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if model.craft.cy > 20495 && Tuple.second model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}
        {-else if model.craft.cy < 5 && Tuple.second model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}-}
        else
            model
    else if model.ifInfinity == False && model.page == Map4 then
        if sqrt ((model.craft.cx)^2 + (model.craft.cy)^2) > 13000 - 20 && Tuple.first model.craft.speed*model.craft.cx + Tuple.second model.craft.speed*model.craft.cy >=0 then
            {model | craft = {craft1 | speed = adjustVector (craft1.cy, -craft1.cx) craft1.speed}, status = Free}
        else if craft1.cx > -10 && Tuple.first model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if craft1.cy < 10 && Tuple.second model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}
        else
            model
    else if model.ifInfinity == False && model.page == Map1 then
        if model.craft.cx > 14795 && Tuple.first model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if model.craft.cx < 5 && Tuple.first model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if model.craft.cy > 13120 && Tuple.second model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}
        else if model.craft.cy < 5 && Tuple.second model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}
        else
            model
    else if model.ifInfinity == False && model.page == Map2 then
        if model.craft.cx > toFloat (model.mapWidth - 5) && Tuple.first model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if model.craft.cx < 5 && Tuple.first model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (-(Tuple.first craft1.speed)/3, Tuple.second craft1.speed/3)}, status = Free}
        else if model.craft.cy > 20495 && Tuple.second model.craft.speed > 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}
        else if model.craft.cy < 5 && Tuple.second model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}
        else
            model
     else if model.page == Map3 then
        if abs(1.02*cx + cy - 7732.8)/1.428 < 50 && Tuple.first model.craft.speed * n1x + Tuple.second model.craft.speed * n1y > 0 then
            {model | craft = {craft1 | speed = adjustVector (w1x,w1y) model.craft.speed}, status = Free}
        else if abs(3.54*cx - cy + 3856.2)/3.68 < 50 && Tuple.first model.craft.speed * n2x + Tuple.second model.craft.speed * n2y > 0 then
            {model | craft = {craft1 | speed = adjustVector (w2x,w2y) model.craft.speed}, status = Free}
        else if abs(0.25*cx - cy + 14112.9)/1.03 < 50 && Tuple.first model.craft.speed * n3x + Tuple.second model.craft.speed * n3y > 0 then
            {model | craft = {craft1 | speed = adjustVector (w3x,w3y) model.craft.speed}, status = Free}
        {-else if model.craft.cy < 5 && Tuple.second model.craft.speed < 0 then
            {model | craft = {craft1 | speed = (Tuple.first craft1.speed/3, -(Tuple.second craft1.speed)/3)}, status = Free}-}
        else if abs(1.036*cx + cy - 28599.5)/1.44 < 50 && Tuple.first model.craft.speed * n4x + Tuple.second model.craft.speed * n4y > 0 then
            {model | craft = {craft1 | speed = adjustVector (w4x,w4y) model.craft.speed}, status = Free}
        else if abs(3.57*cx - cy - 49912.8)/3.7 < 50  && Tuple.first model.craft.speed * n5x + Tuple.second model.craft.speed * n5y > 0 then
            {model | craft = {craft1 | speed = adjustVector (w5x,w5y) model.craft.speed}, status = Free}
        else if abs(0.25*cx - cy - 784.3)/1.03 < 50 && Tuple.first model.craft.speed * n6x + Tuple.second model.craft.speed * n6y > 0 then
            {model | craft = {craft1 | speed = adjustVector (w6x,w6y) model.craft.speed}, status = Free}
        else
            model
    else
        model-}