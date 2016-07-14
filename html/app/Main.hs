{-# LANGUAGE OverloadedStrings #-}

module Main where

import IsHallOpenToday.Html
import qualified Data.ByteString as B

main :: IO ()
main = B.putStr . renderIndex $ "ishallopenApp"
