{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday.Html (
    html
  , renderIndex
)   where

import BasicPrelude

import Lucid

import IsHallOpenToday.Css

import Data.String.Conv

renderIndex :: ByteString -> Text -> ByteString
renderIndex = curry $ toS . renderBS . uncurry html

html :: Monad m => ByteString -> Text -> HtmlT m ()
html ghcjsStatic ghcjsID = doctypehtml_ $ do
        head_ $ do
            meta_ [ charset_ "utf-8" ]
            title_ "Is Hall Open Today?"
            style_ $ renderStrict css
        body_ $ do
            section_ [ id_ ghcjsID ] . toHtmlRaw $ ghcjsStatic
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/react/15.2.0/react.min.js" ] mempty
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/react/15.2.0/react-dom.min.js" ] mempty
            script_ [ src_ "ishallopentoday.js" ] mempty
            noscript_ $
                p_ [ class_ "message" ] "This site requires javascript. It does some calculations to work out whether hall is open, and that needs javascript."
            p_ $
                a_ [  class_ "refresh", href_ "//github.com/dbaynard/ishallopen/issues" ] "Issue tracker"
    where mempty = "" :: ByteString

