module CrafttotalMove exposing (..)
import HighCivil exposing (checkHigh1, checkHigh2)
import Level4 exposing (checkHab1, checkHab2, checkHab3)
import LowCivil exposing (checkLow1, checkLow2)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
--import HolesAttack exposing (model1)
--import CraftCheck exposing (model1)

crafttotalMove : Float -> Model -> Model
crafttotalMove time model =
    if model.status == Free || model.status == Available && model.craft.waveInfluence == False then
        model
            |> radialMove time 
            |> craftMove model.gamepad
            |> craftCollideWall
            |> adjustSpeedtoMax
            |> changePosition time
            |> adjustFuelHp
    else if (model.status == Free || model.status == Available) && model.craft.waveInfluence == True then
        model
            |> craftCollideWall
            |> adjustFuelHp
            |> adjustSpeedtoMax
    else
        model
            |> tangentialMove2
            |> craftCollideWall
            |> adjustFuelHp


craftMove : GamePad -> Model -> Model --move spacecraft according to keyboard
craftMove pad model =
    let
        speedupx = Tuple.first model.craft.speed 

        speedupy = Tuple.second model.craft.speed - 5500*model.deltat
        speedupmag = sqrt (speedupx^2 + speedupy^2)
        
        speedupmaxx = 600*speedupx/speedupmag
        speedupmaxy = 600*speedupy/speedupmag
        speeddownx = Tuple.first model.craft.speed 
        speeddowny = Tuple.second model.craft.speed + 5500*model.deltat
        speeddownmag = sqrt (speeddownx^2 + speeddowny^2)
        speeddownmaxx = 600*speeddownx/speeddownmag
        speeddownmaxy = 600*speeddowny/speeddownmag
        speedleftx = Tuple.first model.craft.speed - 5500*model.deltat
        speedlefty = Tuple.second model.craft.speed 
        speedleftmag = sqrt (speedleftx^2 + speedlefty^2)
        speedleftmaxx = 600*speedleftx/speedleftmag
        speedleftmaxy = 600*speedlefty/speedleftmag
        speedrightx = Tuple.first model.craft.speed + 5500*model.deltat
        speedrighty = Tuple.second model.craft.speed 
        speedrightmag = sqrt (speedrightx^2 + speedrighty^2)
        speedrightmaxx = 600*speedrightx/speedrightmag
        speedrightmaxy = 600*speedrighty/speedrightmag
        
        craft1 = model.craft
    in
    if pad.up == StartPressed && speedupmag < 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speedupx,speedupy), fuel = model.craft.fuel - 0.5 }}
    else if pad.up == StartPressed && speedupmag >= 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speedupmaxx,speedupmaxy), fuel = model.craft.fuel - 0.5 }}
    else if pad.left == StartPressed && speedleftmag < 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speedleftx,speedlefty), fuel = model.craft.fuel - 0.5 }}
    else if pad.left == StartPressed && speedleftmag >= 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speedleftmaxx,speedleftmaxy), fuel = model.craft.fuel - 0.5 }}
    else if pad.right == StartPressed && speedrightmag < 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speedrightx,speedrighty), fuel = model.craft.fuel - 0.5 }}
    else if pad.right == StartPressed && speedrightmag >= 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speedrightmaxx,speedrightmaxy), fuel = model.craft.fuel - 0.5 }}
    else if pad.down == StartPressed && speeddownmag < 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speeddownx,speeddowny), fuel = model.craft.fuel - 0.5 }}
    else if pad.down == StartPressed && speeddownmag >= 600 && (model.craft.fuel > 0 || model.invmode) then
        { model | craft = { craft1 | speed = (speeddownmaxx,speeddownmaxy), fuel = model.craft.fuel - 0.5 }}
    else
        model


changePosition : Float -> Model -> Model
changePosition time model = 
    let
        x1 = model.craft.cx + Tuple.first model.craft.speed *  model.deltat * time / 13
        y1 = model.craft.cy + Tuple.second model.craft.speed * model.deltat * time / 13
        craft1 = model.craft
    in
    if model.craft.waveInfluence == False then
        {model | craft = { craft1 | cx = x1, cy = y1}, newviewcen = (x1, y1)}
    else
        {model | newviewcen = (x1, y1)}

radialMove : Float -> Model -> Model --move spacecraft according to the gravity
radialMove time model =
    let
          dx = model.craft.planetin.px - model.craft.cx
          dy = model.craft.cy - model.craft.planetin.py
          r = sqrt(dx^2 + dy^2)
          --r1 = sqrt((model.craft.cy - model.targetHole.wy)^2 + (model.craft.cx - model.targetHole.wx)^2) 
          a = ( toFloat model.craft.planetin.mass /2) / 300^2
          --checklist = List.filter (\ x -> sqrt((model.craft.cy - x.wy)^2 + (model.craft.cx - x.wx)^2) < toFloat (x.selfRange + model.craft.rcraft) - 5) model.holes
          craft1 = model.craft
    in
    if r > toFloat model.craft.planetin.rplanet then
        { model | craft = { craft1 | speed = (Tuple.first model.craft.speed + a * 24000 * model.deltat * time / 13 * (dx/r) , Tuple.second model.craft.speed - a * 24000 * model.deltat * time / 13 * (dy/r) )}
        }
    else
        { model | craft = { craft1 | speed = (0,0)
        }
        }

tangentialMove2 : Model -> Model --move spacecraft according to the gravity
tangentialMove2 model =
    let
          cx1 = model.targetHole.wx + model.craft.rotateRadius * cos (degrees (model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time * model.craft.rotateSpeed/model.craft.rotateRadius))
          cy1 = model.targetHole.wy + model.craft.rotateRadius * sin (degrees (model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time * model.craft.rotateSpeed/model.craft.rotateRadius))
          vx = -(toFloat model.craft.direction * 24000 * model.craft.rotateSpeed) / (180/3.1415926)  * sin (degrees (model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time *model.craft.rotateSpeed/model.craft.rotateRadius))
          vy = (toFloat model.craft.direction * 24000 *  model.craft.rotateSpeed) / (180/3.1415926) * cos (degrees (model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time * model.craft.rotateSpeed/model.craft.rotateRadius))
          --vmag = sqrt(vx^2 + vy^2)
          craft1 = model.craft
          --speed = (-0.04*rx/r + 0.999* ry/r, -0.999*rx/r - 0.04*ry/r)
    in
    if model.targetHole.types == Repairstation then
        { model | craft ={ craft1 | cx = cx1, cy = cy1
        , speed = (vx, vy)--, craftDirection = model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time * model.craft.rotateSpeed/model.craft.rotateRadius
        , time = craft1.time + model.deltat, hp = min (craft1.hp + 0.5) 100}
        , newviewcen = (model.targetHole.wx, model.targetHole.wy)}
    else if model.targetHole.types == Gasstation then
        { model | craft ={ craft1 | cx = cx1, cy = cy1
        , speed = (vx, vy)--, craftDirection = model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time * model.craft.rotateSpeed/model.craft.rotateRadius
        , time = craft1.time + model.deltat, fuel = min (craft1.fuel + 0.5) 100}
        , newviewcen = (model.targetHole.wx, model.targetHole.wy)}
    else
        { model | craft ={ craft1 | cx = cx1, cy = cy1
        , speed = (vx, vy)--, craftDirection = model.craft.angle + toFloat model.craft.direction * 24000 * model.craft.time * model.craft.rotateSpeed/model.craft.rotateRadius
        , time = craft1.time + model.deltat}
        , newviewcen = (model.targetHole.wx, model.targetHole.wy)}


adjustVector : (Float, Float) -> (Float, Float) -> (Float, Float)
adjustVector dirvec vec1 = 
    let
        dirvecmag = sqrt ((Tuple.first dirvec)^2 + (Tuple.second dirvec)^2)
        normeddirx = (Tuple.first dirvec)/dirvecmag
        normeddiry = (Tuple.second dirvec)/dirvecmag
        adjvecx = (2*normeddirx^2 - 1)*(Tuple.first vec1) + 2*normeddirx*normeddiry*(Tuple.second vec1)
        adjvecy = 2*normeddirx*normeddiry*(Tuple.first vec1) + (2*normeddiry^2 - 1)*(Tuple.second vec1)
    in
        (adjvecx/2.5, adjvecy/2.5)


craftCollideWall : Model -> Model
craftCollideWall model =
    let
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
        model     --00  18.5*800 0    0 13120   14480 13120


adjustFuelHp : Model -> Model
adjustFuelHp model =
    let
        craft1 = model.craft
    in
        {model | craft = {craft1 | hp = max craft1.hp 0, fuel = max craft1.fuel 0}}

adjustSpeedtoMax : Model -> Model
adjustSpeedtoMax model =
    let
        craft1 = model.craft
        speedx = Tuple.first craft1.speed
        speedy = Tuple.second craft1.speed
        speedmag = sqrt (speedx^2 + speedy^2)
        speedmaxx = 700*speedx/speedmag
        speedmaxy = 700*speedy/speedmag
    in
    if speedmag > 700 then
        {model | craft = {craft1 | speed = (speedmaxx, speedmaxy)}}
    else
        model

brokenCraft : Model -> Model
brokenCraft model =
    if model.craft.hp == 0 && model.invmode == False then
        { model | gamestate = Paused, death = model.page, page = Ending1p2, time_ = (Tuple.first model.time_, 0) }
    else
        model