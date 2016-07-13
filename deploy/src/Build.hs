{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Build (
    module Build
)   where

import BasicPrelude
import Data.Yaml
import qualified Data.ByteString as B
import Text.ParserCombinators.ReadP
import Data.List.Extra (trim)

import Development.Shake
-- import Development.Shake.Command
import Development.Shake.FilePath
-- import Development.Shake.Util
import Development.Shake.Classes

newtype GhcVersion = GhcVersion ()
        deriving (Show, Eq, Hashable, Binary, NFData, Typeable)

newtype GhcjsVersion = GhcjsVersion ()
        deriving (Show, Eq, Hashable, Binary, NFData, Typeable)

newtype GhcjsFolder = GhcjsFolder ()
        deriving (Show, Eq, Hashable, Binary, NFData, Typeable)

build :: IO ()
build = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want [ ishallopentodayHtml 
         , distDir </> "ishallopentoday" <.> "js"
         , distDir </> "index" <.> "html"
         ]
    phony "clean" clean
    void $ addOracle getGhcVersion
    void $ addOracle getGhcjsVersion
    void $ addOracle getGhcjsFolder
    ishallopentodayHtml %> getGen
    distDir </> "ishallopentoday" <.> "js" %> getJS
    distDir </> "index" <.> "html" %> getHtml

clean :: Action ()
clean = do
        removeFilesAfter "_build" ["//*"]
        removeFilesAfter buildDir ["//*"]
        removeFilesAfter "." [ishallopentodayHtml]

getJS :: FilePath -> Action ()
getJS out = do
        ghcjs <- askOracle (GhcjsVersion ())
        command_ [] "stack" ["build", "--compiler", ghcjs, "ishallopentoday-client"]
        lir <- localInstallRoot
        ghcjsFolder <- askOracle (GhcjsFolder ())
        let clientjs = lir </> ghcjsFolder </> "bin" </> "ishallopentoday-client.jsexe" </> "all.js"
        need [clientjs]
        liftIO $ do
            alljs <- B.readFile clientjs
            B.writeFile out $ "(function(global,React,ReactDOM) {" <> alljs <> "})(window, window['React'], window['ReactDOM']);"
        command_ [] "sed" ["-i", "s/goog.provide.*//", clientjs]
        command_ [] "sed" ["-i", "s/goog.require.*//", clientjs]

getMinJS :: FilePath -> Action ()
getMinJS out = do
        getJS out
        need [outputJS]
        Stdout minOut <- command [] "closure" ["--compilation_level=ADVANCED_OPTIMIZATIONS", outputJS] :: Action (Stdout ByteString)
        liftIO . B.writeFile outputMinJS $ minOut
        need [outputMinJS]
        outputMinJS `copyFileChanged` out

getHtml :: String -> Action ()
getHtml out = do
        need [ishallopentodayHtml]
        command_ [] "./ishallopentoday-html" []
        need [index]
        index `copyFile'` out
        removeFilesAfter index []

getGen :: String -> Action ()
getGen out = do
        command_ [] "stack" ["build", out]
        lir <- localInstallRoot
        let htmlexe = lir </> "bin" </> ishallopentodayHtml 
        need [htmlexe]
        htmlexe `copyFileChanged` out

getGhcVersion :: GhcVersion -> Action String
getGhcVersion (GhcVersion _) = trim . fromStdout <$> command [] "stack" ["ghc", "--", "--numeric-version"]

getGhcjsVersion :: GhcjsVersion -> Action String
getGhcjsVersion (GhcjsVersion _) = do
        need [cstackyaml]
        Just (yaml :: Object) <- liftIO . decodeFile $ cstackyaml
        Just ghcjs <- parseMonad (.: "compiler") yaml
        pure ghcjs

getGhcjsFolder :: GhcjsFolder -> Action String
getGhcjsFolder (GhcjsFolder _) = do
        ghcVer <- askOracle (GhcVersion ())
        ghcjsVer <- head . fmap fst . readP_to_S parseGhcjs <$> askOracle (GhcjsVersion ())
        pure $ ghcjsVer <> "_ghc-" <> ghcVer
    where
        parseGhcjs = (<>) <$> string "ghcjs-" <*> fmap (intersperse '.') (digit `endBy1` char '.') <* count 8 digit
        digit = choice . fmap char $ "0123456789"

localInstallRoot :: Action FilePath
localInstallRoot = trim . fromStdout <$> command [] "stack" ["path", "--local-install-root"]

distDir, buildDir, outputName, outputJS, outputMinJS, cstackyaml, index, ishallopentodayHtml :: FilePath
buildDir = "dist-js"
distDir = "dist"
outputName = "client"
outputJS = buildDir </> "client" <.> "js"
outputMinJS = buildDir </> "client" <.> "min" <.> "js"
cstackyaml = "client" </> "stack" <.> "yaml"
index = "index" <.> "html"
ishallopentodayHtml = "ishallopentoday-html" <.> exe

