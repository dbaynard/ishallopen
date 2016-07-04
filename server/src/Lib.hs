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
import Lucid.Base (makeAttribute)
import Lucid.Bootstrap

import HallDates
import Css

type API = Get '[HTML] Message

instance ToHtml Message where
    toHtml msg = doctypehtml_ $ do
        head_ $ do
            title_ "Is Hall Open Today?"
            bootStrap
            -- style_ $ renderStrict css
        body_ $
            containerFluid_ . p_ . toHtmlRaw $ msg
    toHtmlRaw = toHtml . interpret

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = lift ishallopen

bootStrap :: Monad m => HtmlT m ()
bootStrap = do
        -- Latest compiled and minified CSS
        link_ [
                rel_ "stylesheet"
              , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
              , integrity_ "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7"
              , crossorigin_ "anonymous"
              ]
        -- Optional theme
        link_ [
                rel_ "stylesheet"
              , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap-theme.min.css"
              , integrity_ "sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r"
              , crossorigin_ "anonymous"
              ]
        -- Latest compiled and minified JavaScript
        link_ [
                rel_ "stylesheet"
              , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
              , integrity_ "sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS"
              , crossorigin_ "anonymous"
              ]
        script_ [
                  src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"
                ] ("" :: ByteString)
    where
        integrity_ = makeAttribute "integrity"
        crossorigin_ = makeAttribute "crossorigin"
