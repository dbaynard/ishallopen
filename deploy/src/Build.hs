{-|
Module      : Build
Description : Deploy website
Maintainer  : David Baynard <davidbaynard@gmail.com>

Copyright   : David Baynard 2016
License     : Apache

This uses <http://shakebuild.com/ Shake> to build the website.

-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Build (
    build
)   where

import BasicPrelude
import Data.Yaml
import qualified Data.ByteString as B
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

data Flag = Production
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

{-|
  Flags to be passed to the 'build' function.

  The default mode is development mode, in which javascript is not minified.
-}
cmdFlags :: [OptDescr (Either String Flag)]
cmdFlags = [Option "P" ["production"] (NoArg . Right $ Production) "Production mode — do compile javascript"]

{-|
  This is where the magic happens.

  'shakeArgsWith' uses 'cmdFlags' to toggle development mode (i.e. whether the javascript is minified).
  
  The 'want' function adds the 'phony' `clean` target to 'shakeArgsWith'. 

  The minified output javascript is only a dependency if development mode is off.

  Use the argument `clean` to clear up after compilation.
-}
build :: IO ()
build = shakeArgsWith shakeOptions{shakeFiles=shakeDir} cmdFlags $ \flags targets -> pure . pure $ do
    want $ [ ishallopentodayHtml 
           , outputJS
           , distDir </> "ishallopentoday" <.> "js"
           , distDir </> index
           , distDir </> previewImage
           ] <> targets -- Adds `clean` target
    -- | Only want outputMinJS if not in development mode.
    action $
        when (Production `elem` flags) $ need [outputMinJS]
    -- | Get information about compilers, therefore about stack directory
    -- structure
    addOracle' getGhcVersion
    addOracle' getGhcjsVersion
    -- | Clear up
    "clean" ~> clean
    -- | The heavy lifting
    ishallopentodayHtml %> getGen
    outputJS %> genJS
    outputMinJS %> genMinJS
    packageJSON react %> npmLink
    packageJSON reactDom %> npmLink
    outputNode %> genNodeJS
    distDir </> "ishallopentoday" <.> "js" %> if Production `elem` flags then getMinJS else getJS
    distDir </> index %> getHtml
    distDir </> previewImage %> getPreviewImage

{-|
  This helper function ignores the return value for 'addOracle'.
-}
addOracle' :: (ShakeValue q, ShakeValue a) => (q -> Action a) -> Rules ()
addOracle' = void . addOracle

{-|
  Clear up afterwards!

  This has to be fixed manually after any changes.
-}
clean :: Action ()
clean = do
        putNormal $ "Removing files in " <> ", " `intercalate` [shakeDir, buildDir] <> "and removing " <> ishallopentodayHtml
        removeFilesAfter buildDir ["//"]
        removeFilesAfter "." [ishallopentodayHtml]
        removeFilesAfter shakeDir ["//"]

{-|
  Produce the `client.js` file. Track the whole client directory.
-}
genJS :: FilePath -> Action ()
genJS out = do
        need =<< getDirectoryFiles "" [outputName <//> "*.hs", outputName <//> "*.cabal", "common" <//> "*.hs", "common" <//> "*.cabal"]
        ghcjs <- askOracle (GhcjsVersion ())
        command_ [] "stack" ["build", "--compiler", ghcjs, "ishallopentoday-client"]
        lir <- localInstallRoot (Just ghcjs)
        let clientjs = lir </> "bin" </> "ishallopentoday-client.jsexe" </> "all.js"
        putNormal "clientjs"
        need [clientjs]
        liftIO $ do
            alljs <- B.readFile clientjs
            B.writeFile out $ "(function(global,React,ReactDOM) {" <> alljs <> "})(window, window['React'], window['ReactDOM']);"
        command_ [] "sed" ["-i", "s/goog.provide.*//", out]
        command_ [] "sed" ["-i", "s/goog.require.*//", out]

{-|
  Produce the `client.node.js` file. Track the whole client directory.
-}
genNodeJS :: FilePath -> Action ()
genNodeJS out = do
        need [outputJS, packageJSON react, packageJSON reactDom]
        ghcjs <- askOracle (GhcjsVersion ())
        lir <- localInstallRoot (Just ghcjs)
        let nodejs = lir </> "bin" </> "ishallopentoday-node.jsexe" </> "all.js"
        need [nodejs]
        writeFile' out $ "React = require(\"react\");\n"
                      <> "ReactDOMServer = require(\"react-dom/server\");\n"
                      <> "require(\"" <> nodejs <> "\");"

{-|
  Copy the `client.js` file to the distribution (gh-pages) directory.
-}
getJS :: FilePath -> Action ()
getJS out = do
        alwaysRerun
        need [outputJS]
        outputJS `copyFile'` out

{-|
  Produce the `client.min.js` file.
-}
genMinJS :: FilePath -> Action ()
genMinJS out = do
        need [outputJS]
        Stdout minOut <- command [] "closure" ["--compilation_level=ADVANCED_OPTIMIZATIONS", outputJS] :: Action (Stdout ByteString)
        liftIO . B.writeFile out $ minOut

{-|
  Copy the `client.min.js` file to the distribution (gh-pages) directory.
-}
getMinJS :: FilePath -> Action ()
getMinJS out = do
        alwaysRerun
        need [outputMinJS]
        outputMinJS `copyFile'` out

getPreviewImage :: FilePath -> Action ()
getPreviewImage out = do
        alwaysRerun
        need [staticDir </> previewImage]
        ( staticDir </> previewImage ) `copyFileChanged` out

{-|
  Produce the `index.html` file in the distribution directory.
-}
getHtml :: String -> Action ()
getHtml out = do
        need [ishallopentodayHtml, outputNode, distDir </> previewImage]
        Stdout reactHtml <- command [] "node" [outputNode]
        Stdout html <- command [StdinBS reactHtml] "./ishallopentoday-html" []
        liftIO . B.writeFile out $ html

{-|
  Compile the executable that will produce the html for the site.
-}
getGen :: String -> Action ()
getGen out = do
        need =<< getDirectoryFiles "" ["html" <//> "*.hs", "html" <//> "*.cabal"]
        command_ [] "stack" ["build", out]
        lir <- localInstallRoot Nothing
        let htmlexe = lir </> "bin" </> ishallopentodayHtml 
        need [htmlexe]
        htmlexe `copyFileChanged` out

{-|
  Call `stack` for the ghc version.
-}
getGhcVersion :: GhcVersion -> Action String
getGhcVersion (GhcVersion _) = trim . fromStdout <$> command [] "stack" ["ghc", "--", "--numeric-version"]

{-|
  Check `stack.yaml` for the ghcjs compiler version.
-}
getGhcjsVersion :: GhcjsVersion -> Action String
getGhcjsVersion (GhcjsVersion _) = do
        need [cstackyaml]
        Just (yaml :: Object) <- liftIO . decodeFile $ cstackyaml
        Just ghcjs <- parseMonad (.: "compiler") yaml
        pure ghcjs

npmLink :: FilePath -> Action ()
npmLink out = command_ [Cwd (takeDirectory1 out)] "npm" ["link", (!! 1) . reverse . splitDirectories $ out]

{-|
  Call `stack` to find where programs are (locally) installed.
-}
localInstallRoot :: Maybe String -> Action FilePath
localInstallRoot = fmap (trim . fromStdout) . command [] "stack" . (["path", "--local-install-root"] <>) . specifiedCompiler
    where
        specifiedCompiler (Just path) = ["--compiler", path]
        specifiedCompiler Nothing     = []

{-|
  Settings; the names should be relatively clear.
-}
shakeDir, distDir, buildDir, staticDir, outputName, react, reactDom, outputJS, outputMinJS, outputNode, cstackyaml, index, ishallopentodayHtml, previewImage :: FilePath
shakeDir = ".build"
buildDir = "dist-js"
distDir = "dist"
staticDir = "static"
outputName = "client"
react = buildDir </> "node_modules" </> "react"
reactDom = buildDir </> "node_modules" </> "react-dom"
outputJS = buildDir </> outputName <.> "js"
outputMinJS = buildDir </> outputName <.> "min" <.> "js"
outputNode = buildDir </> outputName <.> "node" <.> "js"
cstackyaml = outputName </> "stack" <.> "yaml"
index = "index" <.> "html"
ishallopentodayHtml = "ishallopentoday-html" <.> exe
previewImage = "hall-today" <.> "png"

packageJSON :: FilePath -> FilePath
packageJSON = (</> "package" <.> "json")
