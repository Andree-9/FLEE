module Bgm exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (autoplay, controls, loop, src, style)
import Html.Events exposing (..)
import Model exposing (..)

renderBgm: Model -> Html Msg
renderBgm model =
        div []
            [ audio
                [ autoplay True, src model.bgm, controls False, loop True ]
                []
            ]