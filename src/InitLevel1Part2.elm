module InitLevel1Part2 exposing (..)
--import Model exposing (..)
import Random

type alias Planet =
    { px : Float
    , py : Float
    , range : Int -- gravitational radius
    , rplanet : Int
    --, holes : List Hole
    --, meteorites : List Meteorite
    , speed : (Float, Float)
    , mass : Int
    , id : Int
    }

type alias Hole =
    { wx : Float
    , wy : Float
    , selfRange : Int
    , gravityRange : Int
    , mass : Int
    , speed : (Float, Float)
    , force : (Float, Float)
    , types : HoleType
    , planetin : Planet
    , angle : Float 
    , status : RunType
    , time : Float
    , radius : Float
    , speedmag : Float
    , canRepair : Bool
    , bulletsNum : Int
    , attackTime : Int
    }

type RunType
    = Rotate
    | FreeRun

type HoleType
    = Normal
    | Meteorite
    | WhiteHole
    | Repairstation
    | Gasstation
    | AttackHole


holeMaybe : Hole
holeMaybe =
    {wx = 0, wy = 0, selfRange = 30, gravityRange = 150, mass = 15, speed = (0,0), force = (0,0), types = Normal
    , planetin = { px = 1000, py = 500, rplanet = 120, range = 150, speed = (0,0), mass = 3375, id = 0}
    , angle = 0, status = Rotate, time = 0, radius = 0, speedmag = 0, canRepair = True
    , bulletsNum = 2, attackTime = 0}

                    
mapHolesOnCircle : Int -> Int -> Int -> Int -> Float -> Planet -> List Hole -> List Hole
mapHolesOnCircle coX1 coY1 interval rad angle planet1 holeOnCircle =
    if angle >= 360 then
        holeOnCircle
    else
    let
        newHole = { holeMaybe | wx = (toFloat coX1) + (toFloat rad) * (cos (degrees angle)), wy = (toFloat coY1) + (toFloat rad) * (sin (degrees angle)), planetin = planet1}
    in 
        mapHolesOnCircle coX1 coY1 interval rad (angle + ((toFloat interval)/(toFloat rad))*(180/3.1415926)) planet1 (holeOnCircle ++ [newHole])


holesInit : Int -> Int -> Int -> Int -> Planet -> List Hole -> List Hole
holesInit coX coY rad interval planet holeList =
    if rad < 3*planet.rplanet then
        holeList
    else
    let
        holeOnCircle = mapHolesOnCircle coX coY interval rad 0 planet []
    in
        holesInit coX coY (rad - interval - 15) interval planet (holeList ++ holeOnCircle)


adjustInitHoles : List Hole -> List Hole
adjustInitHoles holes =
    let
        selectedholes = List.filter (\ x -> sqrt ((x.wx - x.planetin.px)^2 + (x.wy - x.planetin.py)^2) > toFloat (3*x.planetin.rplanet) 
                                    && sqrt ((x.wx - x.planetin.px)^2 + (x.wy - x.planetin.py)^2 ) < toFloat x.planetin.range - 100) holes
    in
        selectedholes

random : Random.Seed -> Int -> List Hole -> Int -> Int -> (List Hole, Random.Seed)
random seed numberwanted positionset coX coY=
    if numberwanted == 0 then
        (positionset, seed)
    else
    let
        (x1, seed1) = Random.step (Random.int 0 (List.length positionset - 1)) seed
        (x2, seed2) = Random.step (Random.int 1 2) seed1
        holex1 = Tuple.second (Maybe.withDefault (0,holeMaybe) (List.head (List.filter (\ x -> Tuple.first x == x1) (List.indexedMap Tuple.pair positionset))))

        holetype =
            case x2 of
                1 ->
                    WhiteHole
                2 ->
                    Meteorite
                {-3->
                    Repairstation-}
                _ ->
                    Normal

        list1 =  List.filter (\x -> (x.wx, x.wy) /= (holex1.wx, holex1.wy)) positionset
        list2 = list1 ++ [{holex1 | types = holetype}]
    in
        random seed2 (numberwanted - 1) list2 coX coY

random1 : Random.Seed -> Int -> List Hole -> HoleType-> (List Hole, Random.Seed)
random1 seed numberwanted positionset holetype =
    if numberwanted == 0 then
        (positionset, seed)
    else
    let
        (x1, seed1) = Random.step (Random.int 0 (List.length positionset - 1)) seed
        holex1 = Tuple.second (Maybe.withDefault (0,holeMaybe) (List.head (List.filter (\ x -> Tuple.first x == x1) (List.indexedMap Tuple.pair positionset))))
        list1 =  List.filter (\x -> (x.wx, x.wy) /= (holex1.wx, holex1.wy) ) positionset
        list2 = list1 ++ [{holex1 | types = holetype}]
    in
        random1 seed1 (numberwanted - 1) list2 holetype


angleInit : List Hole -> List Hole -> List Hole
angleInit holes adjholes =
    if List.isEmpty holes then
        adjholes
    else
    let
        theHole = Maybe.withDefault holeMaybe (List.head holes)
        theta = atan ((theHole.planetin.py - theHole.wy)/ (theHole.planetin.px - theHole.wx)) *(180/3.1415926)

        theta2 = theta + 180
        dx = theHole.planetin.px - theHole.wx
        dy = theHole.planetin.py - theHole.wy
        r = sqrt(dx^2 + dy^2)
        speed1 = sqrt (15 * toFloat theHole.planetin.mass/r)

        theHole1 = {theHole | angle = theta, radius = r, speedmag = speed1}
        theHole2 = {theHole | angle = theta2, radius = r, speedmag = speed1}
        adjustedList = adjholes ++ [theHole1]
        adjustedList1 = adjholes ++ [theHole2]
    in
    if theHole.wx >= theHole.planetin.px then
        angleInit (List.drop 1 holes) adjustedList 
    else
        angleInit (List.drop 1 holes) adjustedList1 

initLevel1Part2 : Float -> List Planet
initLevel1Part2 adjustNum =
    let
        planet1 = { px = 1720, py = 1720 - adjustNum, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 1}
        planet2 = { px = 4230, py = 880 - adjustNum, range = 840, rplanet = 110, speed = (0, 0), mass = 3000, id = 2}
        planet3 = { px = 1550, py = 4900 - adjustNum, range = 1440, rplanet = 160, speed = (0, 0), mass = 3000, id = 3}
        planet4 = { px = 5030, py = 3820 - adjustNum, range = 2100, rplanet = 220, speed = (0, 0), mass = 3000, id = 4}
        planet5 = { px = 2430, py = 8660 - adjustNum, range = 2400, rplanet = 240, speed = (0, 0), mass = 3000, id = 5}
        planet6 = { px = 5890, py = 7260 - adjustNum, range = 1300, rplanet = 130, speed = (0, 0), mass = 3000, id = 6}
        planet7 = { px = 1550, py = 12400 - adjustNum, range = 1400, rplanet = 140, speed = (0, 0), mass = 3000, id = 7}
        planet8 = { px = 4210, py = 11900 - adjustNum, range = 1300, rplanet = 120, speed = (0, 0), mass = 3000, id = 8}
        planet9 = { px = 6350, py = 11840 - adjustNum, range = 800, rplanet = 80, speed = (0, 0), mass = 3000, id = 9}
        planet10 = { px = 2592, py = 17910 - adjustNum, range = 2400, rplanet = 240, speed = (0, 0), mass = 3000, id = 10}
        planet11 = { px = 5810, py = 14220 - adjustNum, range = 1400, rplanet = 140, speed = (0, 0), mass = 3000, id = 11}
        planet12 = { px = 6110, py = 16760 - adjustNum, range = 1100, rplanet = 110, speed = (0, 0), mass = 3000, id = 12}
        planet13 = { px = 3281, py = 14346 - adjustNum, range = 1100, rplanet = 110, speed = (0, 0), mass = 3000, id = 13}
        planet14 = { px = 1027, py = 14804 - adjustNum, range = 1000, rplanet = 100, speed = (0, 0), mass = 3000, id = 14}
    in
        [planet1, planet2, planet3, planet4, planet5, planet6, planet7
        , planet8, planet9, planet10, planet11, planet12, planet13
        , planet14]

initPlanetHole : Planet -> Random.Seed -> (List Hole, Random.Seed)
initPlanetHole planet seed=
    let
        (holeslist, seed1) =
            random seed (round (88 * (toFloat planet.range/900))) (holesInit (round planet.px) (round planet.py) planet.range 183 planet []) (round planet.px) (round planet.py)

        --holeslist = holesInit (round planet.px) (round planet.py) planet.range 150 planet []
        adjustedHoles = List.filter (\ x -> x.types /= Normal ) holeslist
        --adjustedHoles1 = List.map (\ x -> {x | planetin = planet}) adjustedHoles
        --adjustedHoles2 = adjustInitHoles adjustedHoles1
        (adjustedHoles1, seed2) = random1 seed1 (round (1 * (toFloat planet.range/900)^(3/2))) adjustedHoles Repairstation
        (adjustedHoles2, seed3) = random1 seed2 (round (1 * (toFloat planet.range/900)^(3/2))) adjustedHoles1 Gasstation
        initialedHoles = angleInit adjustedHoles2 []
        --(updateholes, theRest) = List.partition (\ x -> sqrt ((x.wx - 1000)^2 + (x.wy - 500)^2) < 1700) initialedHoles
    in
        (initialedHoles, seed3)

initAllPlanet : List Hole -> Random.Seed -> List Planet -> (List Hole, Random.Seed)
initAllPlanet restHoleList seed planetlist=
    if List.length planetlist == 0 then
        (restHoleList, seed)
    else
    let
        thePlanet = Maybe.withDefault { px = 0, py = 0, range = 1000, rplanet = 100, speed = (0, 0), mass = 3000, id = 0} (List.head planetlist)
        planetSeedTuple = initPlanetHole thePlanet seed
        newRestHoles = restHoleList ++ Tuple.first planetSeedTuple
                    
    in
        initAllPlanet newRestHoles (Tuple.second planetSeedTuple) (List.drop 1 planetlist)




