module Main exposing (main)

import Browser
import Html exposing (text)

main =
    Browser.sandbox { init = "Hello, Elm!", update = always "Hello, Elm!", view = text }
