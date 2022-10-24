module InitLevel3 exposing (..)

import InitLevel1Part2 exposing (..)
import Model exposing (..)
import Time

initLevel3 : Float -> List Planet
initLevel3 adjustNum =
    let
        planet1 = { px = 8068, py = 5795 - adjustNum, range = 1500, rplanet = 162, speed = (0, 0), mass = 3000, id = 1}
        planet2 = { px = 5808, py = 8118 - adjustNum, range = 1500, rplanet = 162, speed = (0, 0), mass = 3000, id = 2}
        planet3 = { px = 6690, py = 11236 - adjustNum, range = 1500, rplanet = 162, speed = (0, 0), mass = 3000, id = 3}
        planet4 = { px = 9832, py = 12031 - adjustNum, range = 1500, rplanet = 162, speed = (0, 0), mass = 3000, id = 4}
        planet5 = { px = 12091, py = 9708 - adjustNum, range = 1500, rplanet = 162, speed = (0, 0), mass = 3000, id = 5}
        planet6 = { px = 11209, py = 6590 - adjustNum, range = 1500, rplanet = 162, speed = (0, 0), mass = 3000, id = 6}
        planet7 = { px = 7602, py = 9294 - adjustNum, range = 400, rplanet = 47, speed = (0, 0), mass = 3000, id = 7}
        planet8 = { px = 10307, py = 8529 - adjustNum, range = 400, rplanet = 47, speed = (0, 0), mass = 3000, id = 8}
       -- planet9 = { px = 8949, py = 8909 - adjustNum, range = 812, rplanet = 81, speed = (0, 0), mass = 3000, id = 9}
        planet10= { px = 4926, py = 4999 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 10}
        planet11= { px = 3548, py = 10441 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 11}
        planet12= { px = 7572, py = 14355 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 12}
        planet13= { px = 12970, py = 12825 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 13}
        planet14= { px = 14349, py = 7385 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 14}
        planet15= { px = 10327, py = 3469 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 15}
        planet16= { px = 2667, py = 7323 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 16}
        planet17= { px = 4430, py = 13560 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 17}
        planet18= { px = 10715, py = 15153 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 18}
        planet19= { px = 15234, py = 10503 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 19}
        planet20= { px = 13469, py = 4266 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 20}
        planet21= { px = 7185, py = 2673 - adjustNum, range = 1500, rplanet = 163, speed = (0, 0), mass = 3000, id = 21}
    in
        [planet1, planet2, planet3, planet4, planet5, planet6, planet7
        , planet8{-, planet9-}, planet10, planet11, planet12, planet13, planet14, planet15
        , planet16, planet17, planet18, planet19, planet20, planet21] --planet9 is the core planet

changeMaptoLevel3 : Pagetype -> Model -> Model
changeMaptoLevel3 ptype model =
    let
        newtheRestHoles = Tuple.first (initAllPlanet [] model.seed (initLevel3 0))
        newUpdateHoles = []
        newcorePlanet = { px = 8949, py = 8909, range = 100, rplanet = 81, speed = (0, 0), mass = 3000, id = 9}
        craft1 = { cx = 16800, cy = 10900, hp = 100, fuel = 100, speed = (0 , 50), force = (0,0), mass = 3, rcraft = 15
                , planetin = { px = 1720, py = 1720, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 0}
                , angle = 0, direction = 1, time = 0, rotateRadius = 0, rotateSpeed = 0, craftDirection = 90,updateRange = 3200
                , viewRange = 700, waveInfluence = False, wavespeedyadded = 0}
        craft2 = model.craft
    in
    if ptype == Map3 && model.low1Passed == False && model.low2Passed == False then
        { model | coreplanet = newcorePlanet,updateHoles = newUpdateHoles, theRestHoles = newtheRestHoles, craft = craft1, planetList = initLevel3 0, ifHolesAttack = True, viewcen = (craft1.cx, craft1.cy), newviewcen = (craft1.cx, craft1.cy), gamestate = Countdown, countdownLimit = Time.posixToMillis model.time + 11000}
    else
        { model | craft = { craft2 | hp = 100, fuel = 100 } }