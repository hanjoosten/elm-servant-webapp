{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Servant

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy
