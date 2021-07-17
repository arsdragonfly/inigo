module Inigo.Async.Package

import Inigo.Paths
import Inigo.Async.Base
import Inigo.Async.CloudFlare.KV
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Package.PackageDhall
import Inigo.Package.PackageIndex
import Inigo.Paths

||| Gets a package from the "packages" KV
export
getPackage : String -> Promise (Either String Package)
getPackage package =
  map parsePackage (read "packages" package)

||| Returns an index with all packages
export
index : Promise (Either String PackageIndex)
index =
  map parsePackageIndex (read "packages" "index")

public export
data InigoPackagePath
  = TomlPath String
  | DhallPath String

export
Show InigoPackagePath where
  show (TomlPath x) = x
  show (DhallPath x) = x

export
packageFilePath : String -> Promise InigoPackagePath
packageFilePath src =
  let tomlPath = src </> inigoTomlPath
      dhallPath = src </> inigoDhallPath
  in do
    tp <- fs_exists $ tomlPath
    dp <- fs_exists $ dhallPath
    case (tp, dp) of
         (True, False) => pure $ TomlPath tomlPath
         (False, True) => pure $ DhallPath dhallPath
         (True, True) => reject "Conflict: both Inigo.toml and Inigo.dhall found"
         (False, False) => reject "Inigo.toml not found"

parseFile : InigoPackagePath -> Promise Package
parseFile (TomlPath path) = do
  contents <- fs_readFile path
  let Right package = parsePackage contents
    | Left err => reject ("Error reading toml package: " ++ err)
  pure package
parseFile (DhallPath path) = do
  -- can pass the file path to dhall so it can handle relative imports
  Right package <- parsePackageDhall path
    | Left err => reject ("Error reading dhall package: " ++ err)
  pure package

export
readPackage : String -> Promise Package
readPackage dir = parseFile !(packageFilePath dir)

export
currPackage : Promise Package
currPackage = readPackage "." -- current dir
