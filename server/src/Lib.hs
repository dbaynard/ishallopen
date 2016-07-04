{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import BasicPrelude

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Lucid

import HallDates
import Css

type API = Get '[HTML] Message

instance ToHtml Message where
    toHtml msg = doctypehtml_ $ do
        head_ $ do
            title_ "Is Hall Open Today?"
            style_ $ renderStrict css
        body_ $
            p_ . toHtmlRaw $ msg
    toHtmlRaw = toHtml . interpret

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = lift ishallopen
