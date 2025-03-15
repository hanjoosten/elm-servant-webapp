{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Data.Aeson (ToJSON, object, (.=))
import qualified Data.Text as T

-- Define the API
type API = "api" :> "data" :> Get '[JSON] [T.Text]

-- Define the server that will handle the request
server :: Server API
server = return ["Hello", "from", "Haskell", "Servant"]

-- Create the application (API + server)
app :: Application
app = serve (Proxy :: Proxy API) server

-- Run the application on port 8080
main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    run 8080 app
