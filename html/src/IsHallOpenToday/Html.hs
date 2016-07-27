{-# LANGUAGE OverloadedStrings #-}    
{-# LANGUAGE NoImplicitPrelude #-}

module IsHallOpenToday.Html (
    html
  , renderIndex
)   where

import BasicPrelude

import Lucid
import Lucid.Base (makeAttribute)

import IsHallOpenToday.Css

import Data.String.Conv

renderIndex :: ByteString -> Text -> ByteString
renderIndex = curry $ toS . renderBS . uncurry html

html :: Monad m => ByteString -> Text -> HtmlT m ()
html ghcjsStatic ghcjsID = doctypehtml_ $ do
        head_ $ do
            meta_ [ charset_ "utf-8" ]
            meta_ [ property_ "og:image", content_ "hall-today.png" ]
            meta_ [ property_ "og:image:type", content_ "image/png" ]
            meta_ [ property_ "og:image:width", content_ "1382" ]
            meta_ [ property_ "og:image:height", content_ "920" ]
            meta_ [ property_ "og:url", content_ "https://dbaynard.github.io/ishallopen" ]
            meta_ [ property_ "og:title", content_ "Is hall open today?" ]
            metaDescription "Emma hall is sometimes open, sometimes not. What is it doing today?"
            meta_ [ property_ "og:locale", content_ "en_GB" ]
            meta_ [ property_ "og:type", content_ "business.business" ]
            link_ [ rel_ "publisher", href_ "//emmamcr.org.uk"]
            title_ "Is Hall Open Today?"
            style_ $ renderStrict css
            noscript_ . style_ $ renderStrict noScriptCss
        body_ $ do
            section_ [ id_ ghcjsID ] . toHtmlRaw $ ghcjsStatic
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/react/15.2.0/react.min.js" ] mempty
            script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/react/15.2.0/react-dom.min.js" ] mempty
            script_ [ src_ "ishallopentoday.js" ] mempty
            noscript_ [] $
                p_ [ class_ "warning"] "This site requires javascript. It does some calculations to work out whether hall is open, and that needs javascript. So turn on javascript."
            p_ $
                a_ [  class_ "refresh", href_ "//github.com/dbaynard/ishallopen/issues" ] "Issue tracker"
    where mempty = "" :: ByteString

property_ :: Text -> Attribute
property_ = makeAttribute "property"

metaDescription :: Monad m => Text -> HtmlT m ()
metaDescription desc = do
    meta_ [ name_ "description", content_ desc ]
    meta_ [ property_ "og:description", content_ desc ]
