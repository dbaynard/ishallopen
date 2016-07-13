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

import System.Console.GetOpt

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

data Flag = Develop
    deriving (Eq)

cmdFlags :: [OptDescr (Either String Flag)]
cmdFlags = [Option "D" ["develop"] (NoArg . Right $ Develop) "Develop mode — do not compile javascript"]

build :: IO ()
build = shakeArgsWith shakeOptions{shakeFiles=shakeDir} cmdFlags $ \flags targets -> pure . pure $ do
    want $ [ ishallopentodayHtml 
           , distDir </> "ishallopentoday" <.> "js"
           , distDir </> "index" <.> "html"
           ] <> targets
    void $ addOracle getGhcVersion
    void $ addOracle getGhcjsVersion
    void $ addOracle getGhcjsFolder
    "clean" ~> clean
    ishallopentodayHtml %> getGen
    outputJS %> genJS
    distDir </> "ishallopentoday" <.> "js" %> if Develop `elem` flags then getJS else getMinJS
    "index" <.> "html" %> genHtml
    distDir </> "index" <.> "html" %> getHtml

clean :: Action ()
clean = do
        putNormal $ "Removing files in " <> ", " `intercalate` [shakeDir, buildDir] <> "and removing " <> ishallopentodayHtml
        removeFilesAfter buildDir ["//*"]
        removeFilesAfter "." [ishallopentodayHtml, "index.html"]
        removeFilesAfter shakeDir ["//*"]

genJS :: FilePath -> Action ()
genJS out = do
        ghcjs <- askOracle (GhcjsVersion ())
        command_ [] "stack" ["build", "--compiler", ghcjs, "ishallopentoday-client"]
        lir <- localInstallRoot
        ghcjsFolder <- askOracle (GhcjsFolder ())
        let clientjs = lir </> ghcjsFolder </> "bin" </> "ishallopentoday-client.jsexe" </> "all.js"
        putNormal "clientjs"
        need [clientjs]
        liftIO $ do
            alljs <- B.readFile clientjs
            B.writeFile out $ "(function(global,React,ReactDOM) {" <> alljs <> "})(window, window['React'], window['ReactDOM']);"
        command_ [] "sed" ["-i", "s/goog.provide.*//", out]
        command_ [] "sed" ["-i", "s/goog.require.*//", out]

getJS :: FilePath -> Action ()
getJS out = do
        need [outputJS]
        outputJS `copyFileChanged` out

getMinJS :: FilePath -> Action ()
getMinJS out = do
        need [outputJS]
        Stdout minOut <- command [] "closure" ["--compilation_level=ADVANCED_OPTIMIZATIONS", outputJS] :: Action (Stdout ByteString)
        liftIO . B.writeFile outputMinJS $ minOut
        need [outputMinJS]
        outputMinJS `copyFileChanged` out

genHtml :: String -> Action ()
genHtml _ = do
        need [ishallopentodayHtml]
        command_ [] "./ishallopentoday-html" []

getHtml :: String -> Action ()
getHtml out = do
        need [index]
        index `copyFileChanged` out
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

shakeDir, distDir, buildDir, outputName, outputJS, outputMinJS, cstackyaml, index, ishallopentodayHtml :: FilePath
shakeDir = "_build"
buildDir = "dist-js"
distDir = "dist"
outputName = "client"
outputJS = buildDir </> "client" <.> "js"
outputMinJS = buildDir </> "client" <.> "min" <.> "js"
cstackyaml = "client" </> "stack" <.> "yaml"
index = "index" <.> "html"
ishallopentodayHtml = "ishallopentoday-html" <.> exe

