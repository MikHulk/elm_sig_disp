module Main exposing (..)

import Browser
import SimpleSinOsc exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = styledView
        , subscriptions = \_ -> Sub.none
        }
