module Server where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp

import API

server :: Server API
server = return "Hello from Servant!"

app :: Application
app = serve api server

main :: IO ()
main = run 8080 app
