module CraftTail exposing (..)

import Model exposing (Bullet, Model, Tail)


craftTail : Model -> Model
craftTail model =
    let
        cx1 = model.craft.cx
        cy1 = model.craft.cy
        speedx = Tuple.first model.craft.speed
        speedy = Tuple.second model.craft.speed
        speedmag = sqrt (speedx^2 + speedy^2)
        theside = {px = cx1, py = cy1, radius = 17, direction = (-speedy/speedmag, speedx/speedmag)}
        tail1 = model.tail ++ [{cx = cx1, cy = cy1, r = 23.5}]
        tail2 = List.map(\x -> {r = x.r - 0.6, cx = x.cx, cy = x.cy}) tail1
        tail3 = List.filter (\x -> x.r > 5 ) tail2

        crafttail1 = model.craftTail ++ [{sideone = theside, sidetwo = model.theSide}]
        crafttail2 = List.map(\x -> let
                                        side1 = x.sideone
                                        side2 = x.sidetwo
                                    in
                                        {x | sideone = {side1 | radius = x.sideone.radius-0.7*(0.95+6/x.sideone.radius)}
                                        ,sidetwo = {side2 | radius = x.sidetwo.radius-0.7*(0.95+6/x.sideone.radius)}}) crafttail1
        crafttail3 = List.filter (\x -> x.sideone.radius > 1 && x.sidetwo.radius > 1) crafttail2
    in
    if model.ifStrangeWave == False then
        { model | craftTail = crafttail3, theSide = {theside | radius = theside.radius-0.7*(0.95+6/theside.radius)}}
    else
         { model | tail = tail3 }

mapBulletTail : ( Float, Float, List Tail ) -> List Tail
mapBulletTail (x, y, tail) =
    let
        tail1 = tail ++ [{cx = x, cy = y, r = 6}]
        tail2 = List.map(\t -> {r = t.r - 0.8, cx = t.cx , cy = t.cy  }) tail1
        tail3 = List.filter (\t -> t.r > 0 ) tail2
    in
        tail3

changeBulletTail : Model -> Model
changeBulletTail model =
    { model|
    bullets = List.map(\bullet -> {x= bullet.x, y = bullet.y, direction = bullet.direction, tail = mapBulletTail(bullet.x,bullet.y,bullet.tail)}) model.bullets}

bulletTail : List Bullet -> List Tail -> List Tail
bulletTail bulletlist taillist =
    if List.length bulletlist == 0 then
        taillist
    else
        let
            firstbullet = Maybe.withDefault {x = 0,y = 0, direction = (0,0), tail = []} (List.head bulletlist)
            taillist1 = mapBulletTail (firstbullet.x, firstbullet.y, firstbullet.tail)
        in
            bulletTail (List.drop 1 bulletlist) (taillist ++ taillist1)

bulletTail2 : Model -> Model
bulletTail2 model =
    let
        bullets_ = model.bullets
        bullettail_ = bulletTail bullets_ []
    in
    { model | bullettail = bullettail_ }