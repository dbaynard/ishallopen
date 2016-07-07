{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday.Html (
    html
  , renderIndex
)   where

import BasicPrelude

import Lucid

import IsHallOpenToday.Css

renderIndex :: Text -> IO ()
renderIndex = renderToFile "index.html" . html

html :: Monad m => Text -> HtmlT m ()
html identifier = doctypehtml_ $ do
        head_ $ do
            meta_ [ charset_ "utf-8" ]
            title_ "Is Hall Open Today?"
            style_ $ renderStrict css
        body_ $ do
            section_ [ id_ identifier ] . pure $ ()
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/react/15.2.0/react.min.js" ] mempty
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/react/15.2.0/react-dom.min.js" ] mempty
            script_ [ src_ "client.js" ] mempty
    where mempty = "" :: ByteString

