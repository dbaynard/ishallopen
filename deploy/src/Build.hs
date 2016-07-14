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

data Flag = Development
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

{-|
  Flags to be passed to the 'build' function.
-}
cmdFlags :: [OptDescr (Either String Flag)]
cmdFlags = [Option "D" ["development"] (NoArg . Right $ Development) "Development mode — do not compile javascript"]

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
           ] <> targets -- Adds `clean` target
    -- | Only want outputMinJS if not in development mode.
    action $
        when (Development `notElem` flags) $ need [outputMinJS]
    -- | Get information about compilers, therefore about stack directory
    -- structure
    addOracle' getGhcVersion
    addOracle' getGhcjsVersion
    addOracle' getGhcjsFolder
    -- | Clear up
    "clean" ~> clean
    -- | The heavy lifting
    ishallopentodayHtml %> getGen
    outputJS %> genJS
    outputMinJS %> genMinJS
    distDir </> "ishallopentoday" <.> "js" %> if Development `elem` flags then getJS else getMinJS
    distDir </> index %> getHtml

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
        removeFilesAfter buildDir ["//*"]
        removeFilesAfter "." [ishallopentodayHtml]
        removeFilesAfter shakeDir ["//*"]

{-|
  Produce the `client.js` file. Track the whole client directory.
-}
genJS :: FilePath -> Action ()
genJS out = do
        need =<< getDirectoryFiles "" [outputName <//> "*.hs", outputName <//> "*.cabal"]
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

{-|
  Produce the `index.html` file in the distribution directory.
-}
getHtml :: String -> Action ()
getHtml out = do
        need [ishallopentodayHtml]
        Stdout html <- command [] "./ishallopentoday-html" []
        liftIO . B.writeFile out $ html

{-|
  Compile the executable that will produce the html for the site.
-}
getGen :: String -> Action ()
getGen out = do
        need =<< getDirectoryFiles "" ["html" <//> "*.hs", "html" <//> "*.cabal"]
        command_ [] "stack" ["build", out]
        lir <- localInstallRoot
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

{-|
  Combine the GHC and GHCJS versions for the local stack ghcjs directory.
-}
getGhcjsFolder :: GhcjsFolder -> Action String
getGhcjsFolder (GhcjsFolder _) = do
        ghcVer <- askOracle (GhcVersion ())
        ghcjsVer <- head . fmap fst . readP_to_S parseGhcjs <$> askOracle (GhcjsVersion ())
        pure $ ghcjsVer <> "_ghc-" <> ghcVer
    where
        parseGhcjs = (<>) <$> string "ghcjs-" <*> fmap (intersperse '.') (digit `endBy1` char '.') <* count 8 digit
        digit = choice . fmap char $ "0123456789"

{-|
  Call `stack` to find where programs are (locally) installed.
-}
localInstallRoot :: Action FilePath
localInstallRoot = trim . fromStdout <$> command [] "stack" ["path", "--local-install-root"]

{-|
  Settings; the names should be relatively clear.
-}
shakeDir, distDir, buildDir, outputName, outputJS, outputMinJS, cstackyaml, index, ishallopentodayHtml :: FilePath
shakeDir = ".build"
buildDir = "dist-js"
distDir = "dist"
outputName = "client"
outputJS = buildDir </> outputName <.> "js"
outputMinJS = buildDir </> outputName <.> "min" <.> "js"
cstackyaml = outputName </> "stack" <.> "yaml"
index = "index" <.> "html"
ishallopentodayHtml = "ishallopentoday-html" <.> exe
