module Viewbox exposing (..)

import Model exposing (Model, Pagetype(..), Status(..))


changeViewCenter : Model -> Model
changeViewCenter model =
    let
        x = (Tuple.first model.viewcen) * (max 0 (1 - 0.25 * model.millisclock)) + (Tuple.first model.newviewcen) * (min 1 (0.25 * model.millisclock))
        y = (Tuple.second model.viewcen) * (max 0 (1 - 0.25 * model.millisclock)) + (Tuple.second model.newviewcen) * (min 1 (0.25 * model.millisclock))
        viewcen = if model.status == Hung && model.millisclock > 4 then
                    (model.targetHole.wx, model.targetHole.wy)
                  else if (model.status == Free || model.status == Available) && model.millisclock > 4 then
                    (model.craft.cx, model.craft.cy)
                  else
                    (x, y)
        x_ =
            if model.page == Inf1 || model.page == Inf2 || model.page == Inf3 then
                if Tuple.first viewcen - Tuple.first model.viewbox / 2 <= 0 then
                    Tuple.first model.viewbox / 2
                else if Tuple.first viewcen + Tuple.first model.viewbox / 2 >= 7200 then
                    7200 - Tuple.first model.viewbox / 2
                else
                    Tuple.first viewcen
            else
                Tuple.first viewcen
        y_ = Tuple.second viewcen
    in
    { model | viewcen = (x_, y_)  }

adjustWishedMapSize : Model -> Model
adjustWishedMapSize model =
    let
        speedmag = sqrt (Tuple.first model.craft.speed^2 + Tuple.second model.craft.speed^2)
        wishedsize = 0.8 + 0.5*speedmag/600
    in 
        {model | wishedMapSize = wishedsize}


changeViewbox : Model -> Model
changeViewbox model =
    let
        model1 = adjustWishedMapSize model
        trueBoxSize = model1.viewBoxSize*39/40 + model1.wishedMapSize/40
    in
    { model | viewbox = (trueBoxSize*Tuple.first model.size, trueBoxSize*Tuple.second model.size) 
            , viewBoxSize = trueBoxSize, wishedMapSize = model1.wishedMapSize}
