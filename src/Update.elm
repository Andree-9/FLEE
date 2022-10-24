module Update exposing (..)
import Chatbox exposing (changeBoxState)
import CraftTail exposing (bulletTail2, changeBulletTail, craftTail)
import Dict
import HighCivil exposing (changeHighFirsttext, changeHighSecondtext, checkEndMap2, checkHigh1, checkHigh2)
import Json.Decode as Decode
import Level4 exposing (changeS3to4text, checkHab1, checkHab2, checkHab3)
import LowCivil exposing (changeS2to3text, changeLowFirsttext, changeLowSecondtext)
import Level1 exposing (changeS1text, changeTutorStep, checkEndMap1)
import Model exposing (..)
import PauseInterface exposing (changeMenuState)
import Random
import String exposing (pad)
import HolestotalCollide exposing (..)
import CraftCheck exposing (..)
import HolestotalMove exposing (..)
import CrafttotalMove exposing (..)
import Model exposing (Status(..))
import Svg.Attributes exposing (speed)
import Model exposing (..)
import AdjustUpdate exposing (..)
import HolesAttack exposing (..)
import InitLevel1Part2 exposing (HoleType(..), angleInit, holeMaybe, holesInit, initAllPlanet, initLevel1Part2, random, random1)
import InitLevel3 exposing (..)
import Time
import InitInfinity exposing (..)
import StrangeWave exposing (..)
import InitLevel4 exposing (..)
import InitLevel2 exposing (..)
import InitSolar exposing (..)
import LowCivil exposing (..)
import Viewbox exposing (changeViewCenter, changeViewbox)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize width height ->
            ( { model | size = ( toFloat width, toFloat height ) }
            , Cmd.none
            )
        GetViewport { viewport } ->
            ( { model
                | size = ( viewport.width, viewport.height)
              }
            , Cmd.none
            )
        Pressed button ->
            ( { model
                | gamepad =
                    applyButtonToGamepad button True model.gamepad
              }
            , Cmd.none
            )
        Released button ->
            ( { model
                | gamepad =
                    applyButtonToGamepad button False model.gamepad
              }
            , Cmd.none
            )
        Tick time ->
                (model
                    |> changeInvstate
                    |> adjustHoleTime
                    |> holesAttack
                    |> adjustedUpdate
                    |> craftCheck
                    |> strangewave time
                    |> crafttotalMove time
                    |> craftTail
                    |> changeBulletTail
                    |> bulletTail2
                    |> holestotalCollide
                 --   |> craftFire
                    |> holestotalMove
                    |> changeViewCenter
                    |> implementInfinityChange
                    |> brokenCraft
                , Cmd.none )
        Tock newTime ->
            ( changeViewbox { model | gamestate = changeGameState model
                , boxstate = changeBoxState model
                , tutorial = Tuple.first (changeTutorStep model)
                , story1text = Tuple.first (changeS1text model)
                , story2to3text = Tuple.first (changeS2to3text model)
                , highfirsttext = Tuple.first (changeHighFirsttext model)
                , highsecondtext = Tuple.first (changeHighSecondtext model)
                , lowfirsttext = Tuple.first (changeLowFirsttext model)
                , lowsecondtext = Tuple.first (changeLowSecondtext model)
                , story3to4text = Tuple.first (changeS3to4text model)
                , changeTimeLimit =
                    if model.page == Map1 then
                        Tuple.second (changeTutorStep model)
                    else if model.page == Story1 then
                        Tuple.second (changeS1text model)
                    else if model.page == HighFirst then
                        Tuple.second (changeHighFirsttext model)
                    else if model.page == HighSecond then
                        Tuple.second (changeHighSecondtext model)
                    else if model.page == Story2to3 then
                        Tuple.second (changeS2to3text model)
                    else if model.page == LowFirst then
                        Tuple.second (changeLowFirsttext model)
                    else if model.page == LowSecond then
                        Tuple.second (changeLowSecondtext model)
                    else if model.page == Story3to4 then
                        Tuple.second (changeS3to4text model)
                    else
                        model.changeTimeLimit
                , menustate = changeMenuState model
                , time_ =
                    if Tuple.second model.time_ == 0 then
                        (newTime, 1)
                    else
                        model.time_
                , time = newTime
                , millisclock = toFloat(Time.posixToMillis newTime - Time.posixToMillis (Tuple.first model.time_))/1000
                , high1Passed = if model.highfirsttext == Step8 then True else False
                , high2Passed = if model.highsecondtext == Step7 || model.highsecondtext == Step8 then True else False
                , low1Passed = if model.lowfirsttext == Step6 then True else False
                , low2Passed = if model.lowsecondtext == Step11 || model.lowsecondtext == Step12 then True else False
                , isRobber = if model.lowsecondtext == Step11 then True else False
                , lowHP = if model.craft.hp <= 30 then True else False
                }, Cmd.none )
        Pause ->
            ({ model | gamestate = Paused }, Cmd.none)
        Resume ->
            ({ model | gamestate = Countdown, countdownLimit = Time.posixToMillis model.time + 11000 }, Cmd.none)
        BGM ->
           if model.page == Begining || model.page == Cover then
                 ( { model | bgm = "assets/bgm1.mp3" }, Cmd.none )
           else
                 ( model , Cmd.none )
        UserHoveredButton id ->
            ({ model | animationState = Dict.insert id Pulse model.animationState }, Cmd.none)
        UserUnhoveredButton id ->
            ({ model | animationState = Dict.insert id None model.animationState }, Cmd.none)
        Page ptype ->
            ({ model | page = ptype
                     , tutorial = if ptype == Map1 then
                                    Step1
                                  else
                                    StepNone
                     {-, map2 = if ptype == Map2 then
                                    Step1
                                  else
                                    StepNone-}
                     , boxstate = if (ptype == Map1 || ptype == Map2) then
                                    BoxUp
                                  else
                                    BoxNone
                     , time_ = if ptype == Begining then
                                  model.time_
                               else
                                  (Tuple.first model.time_, 0)
                     , story1text = if ptype == Story1 then
                                        Step1
                                    else
                                        StepNone
                     , highfirsttext = if ptype == HighFirst && model.high1Passed == False then
                                            Step1
                                       else
                                            model.highfirsttext
                     , highsecondtext = if ptype == HighSecond && model.high2Passed == False then
                                            Step1
                                       else
                                            model.highsecondtext
                     , story2to3text = if ptype == Story2to3 then
                                            Step1
                                       else
                                            StepNone
                     , lowfirsttext = if ptype == LowFirst && model.low1Passed == False then
                                            Step1
                                       else
                                            model.lowfirsttext
                     , lowsecondtext = if ptype == LowSecond && model.low2Passed == False then
                                             Step1
                                        else
                                             model.lowsecondtext
                     , story3to4text = if ptype == Story3to4 then
                                             Step1
                                       else
                                             StepNone
             }
             |> changeMaptoLevel3 ptype
             |> changeMaptoLevel4 ptype
             |> changeMaptoLevel2 ptype
             |> changeMaptoLevel2p2 ptype
             |> initInfinity ptype
             , Cmd.none)
        Reset pagetype ->
            (reset pagetype model, Cmd.none)
        Noop ->
            ( model, Cmd.none )



changeGameState : Model -> Gamestate
changeGameState model =
    if model.gamepad.stop == StartPressed && model.gamestate == Playing then
        Paused
    else if model.tutorial /= Step2 && model.tutorial /= Step3 && model.tutorial /= Step4 && model.gamepad.play == StartPressed && model.gamestate == Paused then
        Countdown
    else if model.gamepad.no == StartPressed && model.tutorial == Step1 && model.gamestate /= Countdown then
        Playing
    else if model.tutorial == Step5 && (model.gamepad.up == StartPressed || model.gamepad.left == StartPressed || model.gamepad.right == StartPressed) && model.gamestate /= Countdown then
        Playing
    else if model.tutorial == Step6 && model.gamestate /= Countdown then
        Paused
    else if model.tutorial == Step7 && model.gamestate /= Countdown then
        Playing
    else if model.gamestate == Countdown && (model.countdownLimit - Time.posixToMillis model.time) // 1000 <= 0 then
        Playing
    else
        model.gamestate




decodeButton : Decode.Decoder Button
decodeButton =
    Decode.andThen toButton
        (Decode.field "key" Decode.string)

toButton : String -> Decode.Decoder Button
toButton string =
    case string of
        "ArrowUp" ->
            Decode.succeed GoUp

        "ArrowLeft" ->
            Decode.succeed GoLeft

        "ArrowRight" ->
            Decode.succeed GoRight

        "ArrowDown" ->
            Decode.succeed GoDown

        "A" ->
            Decode.succeed Change
        
        "a" ->
            Decode.succeed Change

        "S" ->
            Decode.succeed Hang

        "D" ->
            Decode.succeed Escape

        "s" ->
            Decode.succeed Hang

        "d" ->
            Decode.succeed Escape

        "Q" ->
            Decode.succeed Pause_

        "q" ->
            Decode.succeed Pause_

        "W" ->
            Decode.succeed Resume_

        "w" ->
            Decode.succeed Resume_

        "Y" ->
            Decode.succeed Yes_

        "y" ->
            Decode.succeed Yes_

        "N" ->
            Decode.succeed No_

        "n" ->
            Decode.succeed No_

        "E" ->
            Decode.succeed Inv

        "e" ->
            Decode.succeed Inv

        "R" ->
            Decode.succeed DInv

        "r" ->
            Decode.succeed DInv

        _ ->
            Decode.fail "Skip"


applyButtonToGamepad : Button -> Bool -> GamePad -> GamePad
applyButtonToGamepad button pressed gamepad =
    case button of
        GoUp->
            { gamepad
                | up =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        GoLeft->
            { gamepad
                | left =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        GoRight->
            { gamepad
                | right =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        GoDown->
            { gamepad
                | down =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Hang ->
            { gamepad
                | hang =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Escape ->
            { gamepad
                | escape =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Change ->
            { gamepad
                | change =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Pause_ ->
            { gamepad
                | stop =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Resume_ ->
            { gamepad
                | play =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Yes_ ->
            { gamepad
                | yes =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        No_ ->
             { gamepad
                 | no =
                     if pressed then
                         StartPressed

                     else
                         NotPressed
             }

        Inv ->
             { gamepad
                 | inv =
                     if pressed then
                         StartPressed

                     else
                         NotPressed
             }

        DInv ->
             { gamepad
                 | dinv =
                     if pressed then
                         StartPressed

                     else
                         NotPressed
             }

changeInvstate : Model -> Model
changeInvstate model =
    if model.gamepad.inv == StartPressed then
        { model | invmode = True }
    else if model.gamepad.dinv == StartPressed then
        { model | invmode = False }
    else
        model


reset : Pagetype -> Model -> Model
reset ptype model =
    let
        thePlanet = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}
        (holeslist, seed1) =
            random (Random.initialSeed 36000) 100 (holesInit 3600 20500 3600 150 thePlanet []) 3600 20500
        adjustedHoles = List.filter (\ x -> x.types /= Normal ) holeslist
        (adjustedHoles1, seed2) = random1 seed1 20 adjustedHoles Repairstation
        initialedHoles = angleInit adjustedHoles1 []
        initialHolesadjusted = List.map (\ x -> {x | planetin = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}}) initialedHoles
        (updateholes, theRest) = List.partition (\ x -> sqrt ((x.wx - 1000)^2 + (x.wy - 1000)^2) < 2700) initialHolesadjusted
        newRestHoles = Tuple.first (initAllPlanet theRest seed2 (initLevel1Part2 0))
    in
    { pressedKeys = []
    , coreplanet = { px = 3600, py = 20500, rplanet = 400, range = 150, speed = (0,0), mass = 3375, id = 0}
    , coreplanetRange = 1750
    , planetList = initLevel1Part2 0
    , craft = { cx = 6000, cy = 800, hp = 100, fuel = 100, speed = (0 , 50), force = (0,0), mass = 3, rcraft = 15
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
    , page = ptype
    , boxstate = if model.page == Map1 then BoxUp else BoxNone
    , viewcen = (6000, 800)
    , newviewcen = (6000, 800)
    , viewbox = (1700, 1000)
    , zone = Time.utc
    , time = Time.millisToPosix 0
    , updateHoles = updateholes
    , theRestHoles = newRestHoles
    , updateTimeLimit = 0
    , bullets = []
    , tutorial = Step1
    , story1text = Step1
    , highfirsttext = if ptype == HighFirst then Step1 else StepNone
    , highsecondtext = if ptype == HighSecond then Step1 else StepNone
    , story2to3text = Step1
    , story3to4text = Step1
    , lowfirsttext = if ptype == LowFirst then Step1 else StepNone
    , lowsecondtext = if ptype == LowSecond then Step1 else StepNone
    , mapWidth = 7200
    , mapHeight = 20500
    , menustate = MenuNone
    , tail = []
    , bullettail = []
    , fire = []
    , holeTime = 0
    , size = (Tuple.first model.size, Tuple.second model.size)
    , maptop = 0
    , strangeWave = Nothing
    , wavedirection = True
    , waveHit = False
    , changeTimeLimit = 0
    , bgm = "assets/bgm1.mp3"
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
    , countdownLimit = Time.posixToMillis model.time + 11000
    , wishedMapSize = 1
    , viewBoxSize = 1
    , craftTail = []
    , theSide = {px = 0, py = 0, radius = 0, direction = (0,0)}
    , invmode = False
    }
         |> initInfinity ptype
         |> changeMaptoLevel3 ptype
         |> changeMaptoLevel4 ptype
         |> changeMaptoLevel2 ptype
         |> changeMaptoLevel2p2 ptype








