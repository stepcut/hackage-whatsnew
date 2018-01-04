{-# language CPP #-}
module Main where

import qualified Data.Map as Map
import Distribution.Hackage.DB.Parsed
#if MIN_VERSION_hackage_db(2,0,0)
import Distribution.Hackage.DB.Path
#endif
import Distribution.Package (PackageName, PackageIdentifier(PackageIdentifier, pkgName, pkgVersion), unPackageName)
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

-- | get the package description for the local directory
local :: Verbosity -> IO PackageDescription
local v =
  fmap packageDescription (readPackageDescription v =<< defaultPackageDesc v)

#if MIN_VERSION_hackage_db(2,0,0)
readHackage = hackageTarball >>= readTarball Nothing
#endif

-- | get the package description from hackage
--
-- This gets the latest version available.
hackage :: PackageName -- ^ the package name
        -> IO (Maybe PackageDescription)
hackage pn = do
  hackage <- readHackage
#if MIN_VERSION_hackage_db(2,0,0)
  case Map.lookup pn hackage of
#else
  case Map.lookup (unPackageName pn) hackage of
#endif
    Nothing ->
      return Nothing
    (Just gpdMap) ->
#if MIN_VERSION_hackage_db(2,0,0)
      return $ Just $ packageDescription (cabalFile $ snd $ head $ Map.toDescList gpdMap)
#else
      return $ Just $ packageDescription (snd $ head $ Map.toDescList gpdMap)
#endif

-- | calculate the directory to the tarball for `PackageIndentifier`
packageDir :: FilePath -> PackageIdentifier -> FilePath
packageDir repoLocal (PackageIdentifier pn version) =
    repoLocal </> display pn </> display version

-- | calculate the path to the tarball for `PackageIndentifier`
packageFile :: FilePath -> PackageIdentifier -> FilePath
packageFile repoLocal pi =
    (packageDir repoLocal pi) </> display pi <.> "tar.gz"

-- | fetch `PackageIdentifier` from hackage
--
-- returns path to downloaded `.tar.gz`
fetchFromHackage :: PackageIdentifier
                 -> IO FilePath
fetchFromHackage pi@(PackageIdentifier _ version) = do
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

-- | create a `.tar.gz` in the local directory by calling `cabal sdist`.
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

-- | calculate the `sha256sum`

sha256Sum :: FilePath -> IO String
sha256Sum fp =
    do sum <- readProcess "sha256sum" [fp] "" -- `catch`
       return $ takeWhile (/= ' ') sum

-- | extract two `tar.gz` filesa nd run a recursive diff on them.
tardiff :: FilePath -- ^ tarball 'a'
        -> FilePath -- ^ tarball 'b'
        -> IO ()
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

-- | tie all the bits together
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
