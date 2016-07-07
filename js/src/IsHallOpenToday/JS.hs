{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module IsHallOpenToday.JS
    ( writeJsCode
    ) where

import BasicPrelude
import Data.Proxy

import Servant.JS
import IsHallOpenToday

api :: Proxy API
api = Proxy

jsCode :: Text
jsCode = jsForAPI api jquery

writeJsCode :: IO ()
writeJsCode = writeJSForAPI api jquery "js.js"
