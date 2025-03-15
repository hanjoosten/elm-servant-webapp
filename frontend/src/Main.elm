module Main exposing (main)

import Browser
import Html exposing (text)

main : Program () String msg
main =
    Browser.sandbox 
        { init = "Hello, Elm!"
        , update = \_ model -> model
        , view = text
        }
