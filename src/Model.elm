module Model exposing (..)

import Browser.Dom exposing (Viewport, getViewport)
import Dict exposing (Dict)
import Keyboard exposing (Key(..))
import Random
import Task
import Time
import InitLevel1Part2 exposing (..)

type Msg
    = Tick Float
    | Pressed Button
    | Released Button
    | Tock Time.Posix
    | Pause
    | Resume
    | UserHoveredButton String
    | UserUnhoveredButton String
    | Page Pagetype
    | GetViewport Viewport
    | Resize Int Int
    | Reset Pagetype
    | BGM
    | Noop


type Button
    = GoUp
    | GoLeft
    | GoRight
    | GoDown
    | Hang
    | Escape
    | Change
    | Pause_
    | Resume_
    | Yes_
    | No_
    | Inv
    | DInv



type alias Model =
    { pressedKeys : List Key --keyboard input
    , coreplanet : Planet
    , coreplanetRange : Int
    , planetList : List Planet
    , craft : Craft
    , holes : List Hole
    --, freeHoles : List Hole
    , coX : Int --coordinate origin
    , coY : Int --coordinate origin
    , wsize : Int
    , seed : Random.Seed
    , status : Status
    , targetHole : Hole
    , gamepad : GamePad
    , deltat : Float
    , time_ : (Time.Posix, Int)
    , time : Time.Posix
    , millisclock : Float
    , gamestate : Gamestate
    , animationState : Dict String AnimationState
    , page : Pagetype
    , boxstate : Boxstate
    , viewcen : (Float, Float)
    , newviewcen : (Float, Float)
    , viewbox : (Float, Float)
    , zone : Time.Zone
    , updateHoles : List Hole
    , theRestHoles : List Hole
    , updateTimeLimit : Int
    , bullets : List Bullet
    , tutorial : Step
    , story1text : Step
    , highfirsttext : Step
    , highsecondtext : Step
    , story2to3text: Step
    , story3to4text : Step
    , lowfirsttext: Step
    , lowsecondtext: Step
    , mapWidth : Int
    , mapHeight : Int
    , menustate : Menustate
    , tail : List Tail
    , fire : List Fire
    , holeTime : Float
    , size : (Float, Float)
    , maptop : Int
    , strangeWave : Maybe StrangeWave
    , wavedirection : Bool
    , waveHit : Bool
    , changeTimeLimit : Int
    , bgm : String
    , ifInfinity : Bool
    , ifStrangeWave : Bool
    , ifHolesAttack : Bool
    , high1Passed : Bool
    , high2Passed : Bool
    , low1Passed : Bool
    , low2Passed : Bool
    , isRobber : Bool
    , lowHP : Bool
    , death : Pagetype
    , wishedMapSize : Float
    , viewBoxSize : Float
    , craftTail : List CraftTail
    , theSide : Side
    , bullettail : List Tail
    , invmode : Bool
    , countdownLimit : Int
    }

type alias CraftTail =
    { sideone : Side
    , sidetwo : Side
    }

type alias Side =
    { px : Float
    , py : Float
    , radius : Float
    , direction : (Float, Float)
    }

type alias StrangeWave =
    { sy : Float
    , speed : Float
    , length : Int
    }

type Menustate
    = MenuOut
    | MenuIn
    | MenuNone

type Step
    = Step1
    | Step2
    | Step3
    | Step4
    | Step5
    | Step6
    | Step7
    | Step8
    | Step9
    | Step10
    | Step11
    | Step12
    | StepNone

type Boxstate
    = BoxUp
    | BoxDown
    | BoxNone

type Pagetype
    = Begining
    | Cover
    | Mapall
    | Map1
    | Map2
    | Map3
    | Map4
    | BE1
    | BE2
    | BE3
    | Story1
    | HighFirst
    | HighSecond
    | Story2to3
    | LowFirst
    | LowSecond
    | Story3to4
    | Help
    | Levelall
    | Inf1
    | Inf2
    | Inf3
    | Ending2p1
    | Ending2p2
    | Ending2p3
    | Ending1p2
    | Ending1p3
    | Ending1p4
    | Ending3p1
    | Ending3p2
    | Ending4p1
    | Notany

type Gamestate
    = Paused
    | Playing
    | Countdown

type Pressed
    = NotPressed
    | StartPressed

type AnimationState
    = None
    | Pulse

type alias GamePad =
    { up : Pressed
    , left : Pressed
    , right : Pressed
    , down : Pressed
    , hang : Pressed
    , escape : Pressed
    , change : Pressed
    , stop : Pressed
    , play : Pressed
    , yes : Pressed
    , no : Pressed
    , inv : Pressed
    , dinv : Pressed
    }

type alias Craft =
    { cx : Float
    , cy : Float
    , hp : Float
    , fuel : Float
    , speed : (Float, Float)
    --, direction : ( Float, Float )
    , force : (Float, Float)
    , mass : Int
    , rcraft : Int
    , planetin : Planet
    , angle : Float
    , direction : Int
    , time : Float
    , rotateRadius : Float
    , rotateSpeed : Float
    , craftDirection : Float
    , updateRange : Int
    , viewRange : Int
    , waveInfluence : Bool
    , wavespeedyadded : Int
    }

type Status
    = Hung
    | Free
    | Available --available to hang on
    --| CanChange


type alias Bullet =
    { x : Float
    , y : Float
    , direction : (Float, Float)
    , tail : List Tail
    }


type alias Tail =
    { cx : Float
    , cy : Float
    , r : Float
    }


type alias Fire =
    { cx : Float
    , cy : Float
    , rx : Float
    , ry : Float
    }

init : () -> (Model, Cmd Msg)
init _ =
    let
        thePlanet = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}
        (holeslist, seed1) =
            random (Random.initialSeed 36000) 230 (holesInit 3600 20500 3600 183 thePlanet []) 3600 20500
        adjustedHoles = List.filter (\ x -> x.types /= Normal ) holeslist
        (adjustedHoles1, seed2) = random1 seed1 20 adjustedHoles Repairstation
        initialedHoles = angleInit adjustedHoles1 []
        initialHolesadjusted = List.map (\ x -> {x | planetin = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}}) initialedHoles
        (updateholes, theRest) = List.partition (\ x -> sqrt ((x.wx - 1000)^2 + (x.wy - 1000)^2) < 3200) initialHolesadjusted
        newRestHoles = Tuple.first (initAllPlanet theRest seed2 (initLevel1Part2 0))
    in
    ({
      pressedKeys = []
    , coreplanet = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}
    , coreplanetRange = 1750
    , planetList = initLevel1Part2 0
    , craft = { cx = 6000, cy = 18000, hp = 100, fuel = 100, speed = (0 , 50), force = (0,0), mass = 3, rcraft = 15
                , planetin = { px = 1720, py = 1720, range = 1700, rplanet = 200, speed = (0, 0), mass = 3000, id = 0}
                , angle = 0, direction = 1, time = 0, rotateRadius = 0, rotateSpeed = 0, craftDirection = 90,updateRange = 3200
                , viewRange = 700, waveInfluence = False, wavespeedyadded = 0}
    , holes = initialedHoles
    , coX = 1000
    , coY = 500
    , wsize = 30
    , seed = Tuple.second (initAllPlanet theRest seed2 (initLevel1Part2 0))
    , status = Free
    , targetHole = holeMaybe
    , gamepad =
            { up = NotPressed
            , left = NotPressed
            , right = NotPressed
            , down = NotPressed
            , hang = NotPressed
            , escape = NotPressed
            , change = NotPressed
            , stop = NotPressed
            , play = NotPressed
            , yes = NotPressed
            , no = NotPressed
            , inv = NotPressed
            , dinv = NotPressed
            }
    , deltat = 1/60
    , time_ = (Time.millisToPosix 0, 0)
    , millisclock = 0
    , gamestate = Paused
    , animationState = Dict.fromList [ ( "1", None ), ( "2", None ), ( "3", None ), ( "4", None ), ( "5", None ), ( "6", None ), ( "7", None ), ("8", None), ("9", None), ("10", None), ("11", None), ("12", None), ("13", None), ("14", None) ]
    , page = Begining
    , boxstate = BoxNone
    , viewcen = (6000, 18000)
    , newviewcen = (6000, 18000)
    , viewbox = (1700, 1000)
    , zone = Time.utc
    , time = Time.millisToPosix 0
    , updateHoles = updateholes
    , theRestHoles = newRestHoles
    , updateTimeLimit = 0
    , bullets = []
    , tutorial = StepNone
    , story1text = StepNone
    , highfirsttext = StepNone
    , highsecondtext = StepNone
    , story2to3text = StepNone
    , lowfirsttext = StepNone
    , lowsecondtext = StepNone
    , story3to4text = StepNone
    , mapWidth = 7200
    , mapHeight = 20500
    , menustate = MenuNone
    , tail = []
    , fire = []
    , holeTime = 0
    , size = (0, 0)
    , maptop = 0
    , strangeWave = Nothing
    , wavedirection = True
    , waveHit = False
    , changeTimeLimit = 0
    , bgm = ""
    , ifInfinity = False
    , ifStrangeWave = False
    , ifHolesAttack = False
    , high1Passed = False
    , high2Passed = False
    , low1Passed = False
    , low2Passed = False
    , isRobber = False
    , lowHP = False
    , death = Cover
    , wishedMapSize = 1
    , viewBoxSize = 1
    , craftTail = []
    , theSide = {px = 0, py = 0, radius = 0, direction = (0,0)}
    , bullettail = []
    , invmode = False
    , countdownLimit = 0
    }
    , Task.perform GetViewport getViewport
    )

