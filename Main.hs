module Main where

import qualified Data.Map as Map
import Distribution.Hackage.DB.Parsed
import Distribution.Package (PackageName(..), PackageIdentifier(PackageIdentifier, pkgName, pkgVersion))
import Distribution.PackageDescription (GenericPackageDescription(packageDescription), PackageDescription(package))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils (defaultPackageDesc)
-- import Distribution.Version (Version)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory ( getAppUserDataDirectory )
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath ( (</>), (<.>) )
import System.IO (hPutStr, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process

local :: Verbosity -> IO PackageDescription
local v =
  fmap packageDescription (readPackageDescription v =<< defaultPackageDesc v)

hackage :: PackageName
        -> IO (Maybe PackageDescription)
hackage (PackageName pn) = do
  hackage <- readHackage
  case Map.lookup pn hackage of
    Nothing ->
      return Nothing
    (Just gpdMap) ->
      return $ Just $ packageDescription (snd $ head $ Map.toDescList gpdMap)

packageDir :: FilePath -> PackageIdentifier -> FilePath
packageDir repoLocal (PackageIdentifier pn version) =
    repoLocal </> display pn </> display version

packageFile :: FilePath -> PackageIdentifier -> FilePath
packageFile repoLocal pi =
    (packageDir repoLocal pi) </> display pi <.> "tar.gz"

fetchFromHackage :: PackageIdentifier -> IO FilePath
fetchFromHackage pi@(PackageIdentifier (PackageName pn) version) = do
  (ec, out, err) <- readProcessWithExitCode "cabal" ["fetch", "--no-dependencies", display pi] ""
  case ec of
    ExitSuccess -> do
      dd <- getAppUserDataDirectory "cabal"
      let repoLocal = dd </> "packages" </> "hackage.haskell.org" -- FIXME: this is the default, but users could be using a mirror or alternative repo
      return $ packageFile repoLocal pi
    (ExitFailure _) -> do
      putStrLn "Failed to fetch package from hackage."
      putStr out
      hPutStr stderr err
      exitWith (ExitFailure 2)

cabalSDist :: PackageIdentifier -> IO FilePath
cabalSDist pi = do
  (ec, out, err) <- readProcessWithExitCode "cabal" ["sdist"] ""
  case ec of
    ExitFailure _ -> do
      putStr out
      hPutStr stderr err
      exitWith (ExitFailure 2)
    ExitSuccess ->
      return $ "dist" </> display pi <.> "tar.gz"

sha256Sum :: FilePath -> IO String
sha256Sum fp =
    do sum <- readProcess "sha256sum" [fp] "" -- `catch`
       return $ takeWhile (/= ' ') sum

tardiff :: FilePath -> FilePath -> IO ()
tardiff tar1 tar2 =
  withSystemTempDirectory "tardiff-XXXXXX" $ \tar1dir ->
  withSystemTempDirectory "tardiff-XXXXXX" $ \tar2dir -> do
    callProcess "tar" ["-C",tar1dir, "-x", "-z", "--strip=1", "-f", tar1]
    callProcess "tar" ["-C",tar2dir, "-x", "-z", "--strip=1", "-f", tar2]
    (ec, out, err) <- readProcessWithExitCode "diff" ["-r","-N","-u", tar1dir, tar2dir] ""
    case ec of
      ExitSuccess -> exitSuccess
      ExitFailure _ -> do
        putStr out
        hPutStr stderr err
        exitWith (ExitFailure 1)

main :: IO ()
main = do
  let v = normal
  localPackage <- local v
  mHackagePackage <- hackage (pkgName $ package $ localPackage)
  case mHackagePackage of
    Nothing ->
        do hPutStrLn stderr $ "Could not find " ++ (unPackageName $ pkgName $ package $ localPackage) ++  " on hackage."
           exitWith (ExitFailure 2)
    (Just hackagePackage) -> do
      if (pkgVersion $ package localPackage) /= (pkgVersion $ package hackagePackage)
        then do hPutStrLn stderr $ "Versions do not match. Local = " ++ display (pkgVersion $ package $ localPackage) ++ ", Hackage = " ++ display (pkgVersion $ package $ hackagePackage)
        else return ()
      pf <- fetchFromHackage (package hackagePackage)
      lf <- cabalSDist (package localPackage)
      tardiff pf lf
