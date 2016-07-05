{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}

module HallWebSimple (
    renderToFile
  , webPage
)   where

import BasicPrelude
import Lucid
import Css

webPage :: Monad m => HtmlT m ()
webPage = doctypehtml_ $ do
        head_ $ do
            title_ "Is Hall Open Today?"
            style_ $ renderStrict css
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.0.0/jquery.min.js" ] ("" :: ByteString)
        body_ $ do
            p_ [id_ "message"] "Checking…"
            script_ [ src_ "halldates.js" ] ("" :: ByteString)
