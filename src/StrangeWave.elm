module StrangeWave exposing (..)
import Model exposing (..)
import InitLevel1Part2 exposing (..)
--import HolesAttack exposing (model1)
--import HolesAttack exposing (adjustedCraft)

strangewave : Float -> Model -> Model
strangewave time model =
    if model.ifStrangeWave == True then
        if model.strangeWave == Nothing && model.waveHit == False then
            model
                |> generateWave
                |> waveChangeMetePos
                |> waveChangeCraftPos time
                |> waveMove
                |> waveDisappear
        else if model.strangeWave == Nothing && model.waveHit == False then
            model
                |> generateWave
                |> waveChangeMetePos
                --|> waveChangeCraftPos time
                |> waveMove
                |> waveDisappear
        else
            model 
                |> waveChangeMetePos
                |> waveChangeCraftPos time
                |> waveMove
                |> waveDisappear
    else
        model

generateWave : Model -> Model           --True : Up, False : Down
generateWave model =
    let
        theWave = Just {sy = model.craft.cy + 1000, speed = -2000, length = 400}
        theWave2 = Just {sy = model.craft.cy - 1400, speed = 2000, length = 400}
    in 
    if model.wavedirection == True then
        {model | strangeWave = theWave, wavedirection = False}
    else
        {model | strangeWave = theWave2, wavedirection = True}


waveChangeMetePos : Model -> Model
waveChangeMetePos model =
    let
        thestrangeWave = Maybe.withDefault {sy = 0, speed = 0, length = 0} model.strangeWave
        (meteList, theRest) = List.partition (\ x -> x.wy < thestrangeWave.sy + toFloat thestrangeWave.length 
            && x.wy > thestrangeWave.sy && x.types == Meteorite) model.updateHoles
        adjustedmeteList = List.map (\ x -> {x | wy = x.wy + 0.5*thestrangeWave.speed*model.deltat, status = FreeRun}) meteList
    in
        {model | updateHoles = adjustedmeteList ++ theRest}

waveChangeCraftPos : Float -> Model -> Model
waveChangeCraftPos time model =
    let
        thestrangeWave = Maybe.withDefault {sy = 0, speed = 0, length = 0} model.strangeWave
        craft1 = model.craft
        speedy = Tuple.second craft1.speed - toFloat model.craft.wavespeedyadded
        adjx = craft1.cx + Tuple.first craft1.speed * model.deltat * time/13
        adjy = craft1.cy + speedy * model.deltat * time/13
        adjustedCraft = {craft1 | cx = adjx, cy = adjy, waveInfluence = True, wavespeedyadded = 500} 
        adjustedCraft2 = {craft1 | cx = adjx, cy = adjy, waveInfluence = True, wavespeedyadded = -500}
    in
    if craft1.cy < thestrangeWave.sy + toFloat thestrangeWave.length && craft1.cy > thestrangeWave.sy && model.status /= Hung && thestrangeWave.speed < 0 then
        {model | craft = adjustedCraft{-, status = Free-}}

    else if craft1.cy < thestrangeWave.sy + toFloat thestrangeWave.length && craft1.cy > thestrangeWave.sy && model.status /= Hung && thestrangeWave.speed > 0 then
        {model | craft = adjustedCraft2{-, status = Free-}}

    else
        if craft1.waveInfluence == True then
            {model | craft = {craft1 | waveInfluence = False, speed = (Tuple.first craft1.speed/6, Tuple.second craft1.speed/6)}}
        else
            {model | craft = {craft1 | waveInfluence = False}}

waveMove : Model -> Model
waveMove model=
    let
        thestrangeWave = Maybe.withDefault {sy = 0, speed = 0, length = 0} model.strangeWave
        adjustedWave = Just {thestrangeWave | sy = thestrangeWave.sy+thestrangeWave.speed*model.deltat}
    in
        {model | strangeWave = adjustedWave}

waveDisappear : Model -> Model
waveDisappear model =
    let
        thestrangeWave = Maybe.withDefault {sy = 0, speed = 0, length = 0} model.strangeWave
    in
    if thestrangeWave.speed < 0 && model.craft.cy-1000 > thestrangeWave.sy + toFloat thestrangeWave.length then
        {model | strangeWave = Nothing}

    else if thestrangeWave.speed > 0 && model.craft.cy+1000 < thestrangeWave.sy then
        {model | strangeWave = Nothing}

    else
        model
        

