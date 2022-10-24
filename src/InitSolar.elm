module InitSolar exposing (..)


import InitLevel1Part2 exposing (..)
import Model exposing (..)


initSolarPlanet : Float -> List Planet
initSolarPlanet adjustNum=
    let
        planet1 = { px = 1600, py = 1600 - adjustNum, range = 1600, rplanet = 160, speed = (0, 0), mass = 3000, id = 1}
        planet2 = { px = 5496, py = 2368 - adjustNum, range = 2373, rplanet = 237, speed = (0, 0), mass = 3000, id = 2}
        planet3 = { px = 2112, py = 5267 - adjustNum, range = 2101, rplanet = 210, speed = (0, 0), mass = 3000, id = 3}
        planet4 = { px = 11250, py = 3470 - adjustNum, range = 3469, rplanet = 347, speed = (0, 0), mass = 3000, id = 4}
        planet5 = { px = 6573, py = 6870 - adjustNum, range = 2291, rplanet = 229, speed = (0, 0), mass = 3000, id = 5}
        planet6 = { px = 2632, py = 10031 - adjustNum, range = 2701, rplanet = 270, speed = (0, 0), mass = 3000, id = 6}
        planet7 = { px = 7093, py = 10984 - adjustNum, range = 1859, rplanet = 186, speed = (0, 0), mass = 3000, id = 7}
        planet8 = { px = 11704, py = 9768 - adjustNum, range = 2862, rplanet = 286, speed = (0, 0), mass = 3000, id = 8}
        --planet9 = { px = 0, py = 0 - adjustNum, range = 15600, rplanet = 15600, speed = (0, 0), mass = 3000, id = 9}
    in
        [planet1, planet2, planet3, planet4, planet5, planet6, planet7
        , planet8{-, planet9-}]

changeMaptoLevel2p2 : Pagetype -> Model -> Model
changeMaptoLevel2p2 ptype model =
    let
        newtheRestHoles = Tuple.first (initAllPlanet [] model.seed (initSolarPlanet 0))
        newUpdateHoles = []
        newcorePlanet = { px = 0, py = 0, range = 500, rplanet = 500, speed = (0, 0), mass = 3000, id = 9}
        craft1 = { cx = 3500, cy = 1000, hp = 100, fuel = 100, speed = (0 , 50), force = (0,0), mass = 3, rcraft = 15
                , planetin = { px = 1720, py = 1720, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 0}
                , angle = 0, direction = 1, time = 0, rotateRadius = 0, rotateSpeed = 0, craftDirection = 90,updateRange = 3200
                , viewRange = 700, waveInfluence = False, wavespeedyadded = 0}
    in
    if ptype == Map1 then
        {model | coreplanet = newcorePlanet,updateHoles = newUpdateHoles, theRestHoles = newtheRestHoles, craft = craft1, planetList = initSolarPlanet 0, viewcen = (craft1.cx, craft1.cy), newviewcen = (craft1.cx, craft1.cy)}
    else
        model
