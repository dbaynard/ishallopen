{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IsHallOpenToday
    ( startApp
    , API
    ) where

import BasicPrelude

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid

import IsHallOpenToday.Common

type API = Get '[JSON] Message

instance ToHtml Message where
    toHtml = toHtml . interpret
    toHtmlRaw = toHtml

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = lift ishallopen
