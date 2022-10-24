module CraftPush exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)

craftMove : GamePad -> Model -> Model --move spacecraft according to keyboard
craftMove pad model =
    let
   --     speed1 = sqrt (Tuple.first model.craft.speed^2 + Tuple.second model.craft.speed^2)
        speedx = Tuple.first model.craft.speed + cos (degrees model.craft.craftDirection)*1000*model.deltat
        speedy = Tuple.second model.craft.speed + sin (degrees model.craft.craftDirection)*1000*model.deltat
        x2 = sin (degrees model.craft.craftDirection)
        y2 = 0 - cos (degrees model.craft.craftDirection)
        x3 = 0 - sin (degrees model.craft.craftDirection)
        y3 = cos (degrees model.craft.craftDirection)
        speedx2 = Tuple.first model.craft.speed + x2*1000*model.deltat
        speedy2 = Tuple.second model.craft.speed + y2*1000*model.deltat
        speedx3 = Tuple.first model.craft.speed + x3*1000*model.deltat
        speedy3 = Tuple.second model.craft.speed + y3*1000*model.deltat
        theta = model.craft.craftDirection - 0.5
        theta1 = model.craft.craftDirection + 0.5
        craft1 = model.craft
    in
    if pad.up == StartPressed then
        { model | craft = { craft1 | speed = (speedx,speedy)}}
    else if pad.left == StartPressed then
        { model | craft = { craft1 | speed = (speedx3,speedy3), craftDirection = theta1}}
    else if pad.right == StartPressed then
        { model | craft = { craft1 | speed = (speedx2,speedy2), craftDirection = theta}}
    else
        model