{-# LANGUAGE OverloadedStrings #-}

module Main where

import IsHallOpenToday.Html
import qualified Data.ByteString as B

main :: IO ()
main = do
        staticHtml <- B.getContents
        B.putStr . renderIndex staticHtml $ "ishallopenApp"
