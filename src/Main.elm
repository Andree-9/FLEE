module Main exposing (..)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onResize)
import Json.Decode as Decode
import Keyboard
import Model exposing (Model, Msg(..), init)
import Task
import Update exposing ( decodeButton, update)
import View exposing (view)
import Time

main =
    Browser.element { init = init
                    , update = update, view = view, subscriptions = subscriptions}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [  if model.gamestate == Model.Playing then
             onAnimationFrameDelta Tick
           else
             Sub.none
        , Browser.Events.onKeyDown (Decode.map Pressed decodeButton)
        , Browser.Events.onKeyUp (Decode.map Released decodeButton)
        , Time.every 1 Tock
        , onResize Resize
        ]

