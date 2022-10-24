module InitInfinity exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
import InitLevel2 exposing (..)
import Random
import Time

initPlanetList1 : Float -> List Planet
initPlanetList1 adjustNum=
    let
        planet1 = { px = 2850, py = 2840 - adjustNum, range = 2700, rplanet = 270, speed = (0, 0), mass = 3000, id = 1}
        planet2 = { px = 6190, py = 980 - adjustNum, range = 800, rplanet = 80, speed = (0, 0), mass = 3000, id = 2}
        planet3 = { px = 6410, py = 2680 - adjustNum, range = 700, rplanet = 70, speed = (0, 0), mass = 3000, id = 3}
        planet4 = { px = 6010, py = 5240 - adjustNum, range = 1000, rplanet = 100, speed = (0, 0), mass = 3000, id = 4}
        planet5 = { px = 1390, py = 6780 - adjustNum, range = 1300, rplanet = 130, speed = (0, 0), mass = 3000, id = 5}
        planet6 = { px = 4710, py = 8560 - adjustNum, range = 2300, rplanet = 230, speed = (0, 0), mass = 3000, id = 6}
        planet7 = { px = 1290, py = 9400 - adjustNum, range = 1100, rplanet = 110, speed = (0, 0), mass = 3000, id = 7}
        planet8 = { px = 2350, py = 12660 - adjustNum, range = 2200, rplanet = 220, speed = (0, 0), mass = 3000, id = 8}
        planet9 = { px = 5850, py = 12100 - adjustNum, range = 1300, rplanet = 130, speed = (0, 0), mass = 3000, id = 9}
        planet10 = { px = 5250, py = 15660 - adjustNum, range = 1800, rplanet = 180, speed = (0, 0), mass = 3000, id = 10}
        planet11 = { px = 1790, py = 16720 - adjustNum, range = 1700, rplanet = 170, speed = (0, 0), mass = 3000, id = 11}
        planet12 = { px = 4687, py = 20200 - adjustNum, range = 2500, rplanet = 250, speed = (0, 0), mass = 3000, id = 12}
        planet13 = { px = 1090, py = 19500 - adjustNum, range = 900, rplanet = 90, speed = (0, 0), mass = 3000, id = 13}
        planet14 = { px = 1309, py = 21860 - adjustNum, range = 1200, rplanet = 120, speed = (0, 0), mass = 3000, id = 14}
    in
        [planet1, planet2, planet3, planet4, planet5, planet6, planet7
        , planet8, planet9, planet10, planet11, planet12, planet13
        , planet14]

adjustMaptoInfinity : Model -> Model
adjustMaptoInfinity model =
    let
        (mapNum, seed1) = Random.step (Random.int 1 2) model.seed
    in
    if mapNum == 1 then
        let
            newRestHoleList = initAllPlanet model.theRestHoles seed1 (initLevel1Part2 (toFloat (-model.maptop + 20500)))
            newplanetList = model.planetList ++ initLevel1Part2 (toFloat (-model.maptop + 20500))
        in
            {model | theRestHoles = Tuple.first newRestHoleList, planetList = newplanetList, mapHeight = model.mapHeight + 20500
            , maptop = model.maptop - 20500, seed = Tuple.second newRestHoleList, gamestate = Countdown, countdownLimit = Time.posixToMillis model.time + 11000}
    else
        let
            newRestHoleList = initAllPlanet model.theRestHoles seed1 (initPlanetList1 (toFloat (-model.maptop + 23300)))
            newplanetList = model.planetList ++ initPlanetList1 (toFloat (-model.maptop + 23300))
        in
            {model | theRestHoles = Tuple.first newRestHoleList, planetList = newplanetList, mapHeight = model.mapHeight + 23300
            , maptop = model.maptop - 23300, seed = Tuple.second newRestHoleList}

implementInfinityChange : Model -> Model
implementInfinityChange model =
    if abs (model.craft.cy - toFloat model.maptop) < 3500 && model.ifInfinity == True then
        adjustMaptoInfinity model
    else
        model

initInfinity : Pagetype -> Model -> Model
initInfinity ptype model =
    let
        newtheRestHoles = Tuple.first (initAllPlanet [] model.seed (initPlanetList1 0))
        newUpdateHoles = []
        newcorePlanet = { px = 3600, py = 23300, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}
        craft1 = { cx = 3600, cy = 22500, hp = 100, fuel = 100, speed = (0 , -500), force = (0,0), mass = 3, rcraft = 15
                , planetin = { px = 1720, py = 1720, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 0}
                , angle = 0, direction = 1, time = 0, rotateRadius = 0, rotateSpeed = 0, craftDirection = 90,updateRange = 3200
                , viewRange = 700, waveInfluence = False, wavespeedyadded = 0}
    in
    if ptype == Inf1 || ptype == Inf2 || ptype == Inf3 then
        { model | coreplanet = newcorePlanet,updateHoles = newUpdateHoles, theRestHoles = newtheRestHoles, craft = craft1, planetList = initPlanetList1 0, viewcen = (craft1.cx, craft1.cy), newviewcen = (craft1.cx, craft1.cy)
                , ifInfinity = True, ifHolesAttack = if ptype == Inf2 || ptype == Inf3 then True else False, ifStrangeWave = if ptype == Inf3 then True else False}
    else
        model
