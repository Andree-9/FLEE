module InitLevel4 exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
import Time

initLevel4Map : List Planet
initLevel4Map =
    let
        planet1 = { px = -2870, py = 2852, range = 2760, rplanet = 276, speed = (0, 0), mass = 3000, id = 1}
        planet2 = { px = -6488, py = 1212, range = 1090, rplanet = 109, speed = (0, 0), mass = 3000, id = 2}
        planet3 = { px = -1229, py = 6500, range = 1220, rplanet = 122, speed = (0, 0), mass = 3000, id = 3}
        planet4 = { px = -6566, py = 3711, range = 950, rplanet = 95, speed = (0, 0), mass = 3000, id = 4}
        planet5 = { px = -9976, py = 2670, range = 2530, rplanet = 253, speed = (0, 0), mass = 3000, id = 5}
        planet6 = { px = -2635, py = 10060, range = 2570, rplanet = 257, speed = (0, 0), mass = 3000, id = 6}
        planet7 = { px = -3650, py = 6575, range = 1000, rplanet = 100, speed = (0, 0), mass = 3000, id = 7}
        planet8 = { px = -7295, py = 7200, range = 2549, rplanet = 255, speed = (0, 0), mass = 3000, id = 8}
        planet9 = { px = -6100, py = 10427, range = 840, rplanet = 84, speed = (0, 0), mass = 3000, id = 9}
        planet10 = { px = -10500, py = 6080, range = 770, rplanet = 77, speed = (0, 0), mass = 3000, id = 10}
        planet11 = { px = 0, py = 13000, range = 240, rplanet = 240, speed = (0, 0), mass = 3000, id = 11}
        planet12 = { px = -13000, py = 0, range = 240, rplanet = 240, speed = (0, 0), mass = 3000, id = 12}
        planet13 = { px = -9200, py = 9200, range = 240, rplanet = 240, speed = (0, 0), mass = 3000, id = 13}
    in
        [planet1, planet2, planet3, planet4, planet5, planet6, planet7
        , planet8, planet9, planet10, planet11, planet12, planet13
        ]

changeMaptoLevel4 : Pagetype -> Model -> Model
changeMaptoLevel4 ptype model =
    let
        newtheRestHoles = Tuple.first (initAllPlanet [] model.seed initLevel4Map)
        newUpdateHoles = []
        newcorePlanet = { px = 400, py = -400, range = 300, rplanet = 300, speed = (0, 0), mass = 3000, id = 9}
        craft1 = { cx = -1000, cy = 1000, hp = 100, fuel = 100, speed = (0 , 50), force = (0,0), mass = 3, rcraft = 15
                , planetin = { px = 1720, py = 1720, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 0}
                , angle = 0, direction = 1, time = 0, rotateRadius = 0, rotateSpeed = 0, craftDirection = 90,updateRange = 3200
                , viewRange = 700, waveInfluence = False, wavespeedyadded = 0}
        craft2 = model.craft
    in
    if ptype == Map4 then
        {model | coreplanet = newcorePlanet,updateHoles = newUpdateHoles, theRestHoles = newtheRestHoles, craft = craft1, planetList = initLevel4Map, ifHolesAttack = True, viewcen = (craft1.cx, craft1.cy), newviewcen = (craft1.cx, craft1.cy), gamestate = Countdown, countdownLimit = Time.posixToMillis model.time + 11000}
    else
        { model | craft = { craft2 | hp = 100, fuel = 100 } }