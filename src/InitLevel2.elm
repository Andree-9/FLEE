module InitLevel2 exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
import Time

changeMaptoLevel2 : Pagetype -> Model -> Model
changeMaptoLevel2 ptype model =
    let
        newtheRestHoles = Tuple.first (initAllPlanet [] model.seed (initLevel1Part2 0))
        newUpdateHoles = []
        newcorePlanet = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}
        craft1 = { cx = 6000, cy = 700, hp = 100, fuel = 100, speed = (0 , 50), force = (0,0), mass = 3, rcraft = 15
                , planetin = { px = 1720, py = 1720, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 0}
                , angle = 0, direction = 1, time = 0, rotateRadius = 0, rotateSpeed = 0, craftDirection = 90,updateRange = 3200
                , viewRange = 700, waveInfluence = False, wavespeedyadded = 0}
        craft2 = model.craft
    in
    if ptype == Map2 && model.high1Passed == False && model.high2Passed == False then
        { model | coreplanet = newcorePlanet,updateHoles = newUpdateHoles, theRestHoles = newtheRestHoles, craft = craft1, planetList = initLevel1Part2 0, viewcen = (craft1.cx, craft1.cy), newviewcen = (craft1.cx, craft1.cy), gamestate = Countdown, countdownLimit = Time.posixToMillis model.time + 11000}
    else if ptype == Map2 && model.high2Passed == True then
        { model | ifHolesAttack = True, craft = { craft2 | hp = 100, fuel = 100 } }
    else
        { model | craft = { craft2 | hp = 100, fuel = 100 } }