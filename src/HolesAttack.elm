module HolesAttack exposing (..)
import Model exposing (..)
import AdjustUpdate exposing (..)
import InitLevel1Part2 exposing (..)
import Random
import Time
--import HolestotalCollide exposing (model1)

holesAttack : Model -> Model
holesAttack model =
    let
        attackHoles = List.filter (\ x -> x.types == AttackHole) model.updateHoles
        model1 = model 
            |> implementAttack attackHoles
            |> bulletMove
            |> bulletCollide
            |> bulletDisappear
            --|> prepareAttack
    in
    if model.ifHolesAttack == True then
        if model.updateTimeLimit < Time.posixToMillis model.time +100 then
            model1
                |> prepareAttack
        else
            model1
    else
        model

--(Tuple.first (Random.step (Random.int 0 2) model.seed))
prepareAttack : Model -> Model 
prepareAttack model =
    let
        (attackHoleList, theRest) = List.partition (\ x -> sqrt((x.wx - model.craft.cx)^2 + (x.wy - model.craft.cy)^2 ) < toFloat model.craft.viewRange 
            && x.types == WhiteHole) model.updateHoles

        judgeAttackList = List.filter (\ x -> x.types == AttackHole ) model.updateHoles
    in
        if List.length judgeAttackList <5 then
            if not (List.isEmpty attackHoleList) then
                let
                    (choosedAttackHoles, seed2) = random1 model.seed 2 attackHoleList AttackHole
                    totHoles = choosedAttackHoles ++ theRest
                in
                    {model | updateHoles = totHoles, seed = seed2}
            else
                model
        else
            model
    
    {-if not (List.isEmpty attackHoleList) then
        {model | updateHoles = attackHoleList ++ theRest, seed = seed2}
    else
        model-}

bulletInit : (Float, Float) -> List Bullet
bulletInit (wx, wy) =
    let
        bullet1 = {x = wx , y = wy - 30, direction = (0, -1), tail = []}
        bullet2 = {x = wx + 30*cos(degrees 315), y = wy + 30*sin(degrees 315), direction = (cos(degrees 315), sin(degrees 315)), tail =[]}
        bullet3 = {x = wx + 30*cos(degrees 0), y = wy + 30*sin(degrees 0), direction = (cos(degrees 0), sin(degrees 0)), tail = []}
        bullet4 = {x = wx + 30*cos(degrees 45), y = wy + 30*sin(degrees 45), direction = (cos(degrees 45), sin(degrees 45)), tail = []}
        bullet5 = {x = wx + 30*cos(degrees 90), y = wy + 30*sin(degrees 90), direction = (cos(degrees 90), sin(degrees 90)), tail = []}
        bullet6 = {x = wx + 30*cos(degrees 135), y = wy + 30*sin(degrees 135), direction = (cos(degrees 135), sin(degrees 135)), tail = []}
        bullet7 = {x = wx + 30*cos(degrees 180), y = wy + 30*sin(degrees 180), direction = (cos(degrees 180), sin(degrees 180)), tail = []}
        bullet8 = {x = wx + 30*cos(degrees 225), y = wy + 30*sin(degrees 225), direction = (cos(degrees 225), sin(degrees 225)), tail = []}
    in
        [bullet1, bullet2, bullet3, bullet4, bullet5, bullet6, bullet7, bullet8]


implementAttack : List Hole -> Model-> Model    --give model the bullets
implementAttack attackHoles model =
    let
        theHole = Maybe.withDefault holeMaybe (List.head attackHoles)
    in
        if List.length attackHoles == 0 then
            model
        else if theHole.attackTime > Time.posixToMillis model.time || theHole.bulletsNum < 1 then
            implementAttack (List.drop 1 attackHoles) model 
        else
        let
            model1 = {model | bullets = model.bullets ++ bulletInit (theHole.wx, theHole.wy)}
            adjustedHole = {theHole | attackTime = Time.posixToMillis model.time + 4000, bulletsNum = theHole.bulletsNum - 1}
            theRest = List.filter (\ x -> not (List.member x [theHole])) model.updateHoles
            newUpdateHoles = theRest ++ [adjustedHole]
        in
            implementAttack (List.drop 1 attackHoles) {model1 | updateHoles = newUpdateHoles} 

bulletMove : Model -> Model
bulletMove model =
    let
        adjustedBullets = List.map (\ bullet -> {bullet | x = bullet.x + Tuple.first bullet.direction * 500 * model.deltat
            , y = bullet.y + Tuple.second bullet.direction * 500 * model.deltat}) model.bullets
    in
        {model | bullets = adjustedBullets}

bulletCollide : Model -> Model
bulletCollide model =
    let
        (hittedBullets, theRest) = List.partition (\ bullet -> sqrt ((bullet.x - model.craft.cx)^2 +(bullet.y - model.craft.cy)^2) < (8 + toFloat model.craft.rcraft)) model.bullets
    in
    if not (List.isEmpty hittedBullets) then
        let
            --(hittedBullets, theRest) = List.partition (\ bullet -> sqrt ((bullet.x - model.craft.x)^2 +(bullet.y - model.craft.y)^2) < 8+model.craft.rcarft) model.bullets
            craft1 = model.craft
            adjustedCraft = {craft1 | hp = craft1.hp - 10}
        in
        {model | bullets = theRest, craft = adjustedCraft}
    else
        model
        
bulletDisappear :Model -> Model
bulletDisappear model =
    let
        adjustedBulletsList = List.filter (\ bullet -> sqrt ((bullet.x - model.craft.cx)^2 +(bullet.y - model.craft.cy)^2) < 1700) model.bullets
    in
        {model | bullets = adjustedBulletsList}
        

