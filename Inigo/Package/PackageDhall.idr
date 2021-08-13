module Inigo.Package.PackageDhall

import Data.List
import Data.Maybe
import Extra.Either
import Extra.List
import Extra.String
import Fmt
import public Inigo.Package.ExtraDep
import Inigo.Package.Package
import Inigo.Package.ParseHelpers
import Inigo.Paths
import Inigo.PkgTree
import Inigo.Async.Promise
import SemVar
import System.Path
import Toml

import Idrall.API.V2
import Idrall.Parser
import Idrall.APIv1

import Language.Reflection
%language ElabReflection

record GitDep where
  constructor MkGitDep
  commit : String
  url : String
  subDirs : List String
%runElab (deriveFromDhall Record `{ GitDep })

Show GitDep where
  show (MkGitDep commit url subDirs) =
    "MkGitDep \{show commit} \{show url} \{show subDirs}"

gitDepDhallType : String
gitDepDhallType =
  "{ commit : Text, url : Text, subDirs : List Text }"

record DepPackage where
  constructor MkDepPackage
  ns : String
  name : String
  requirement : String
%runElab (deriveFromDhall Record `{ DepPackage })

Show DepPackage where
  show (MkDepPackage ns name requirement) =
    "{ ns = \{show ns}, name = \{show name}, requirement = \{show requirement} }"

public export
record PackageDhall where
  constructor MkPackageDhall
  depends : List String
  deps : List (DepPackage)
  description : Maybe String
  devDeps : List (DepPackage)
  executable : Maybe String
  license : Maybe String
  link : Maybe String
  main : Maybe String
  modules : List String
  ns : String
  package : String
  readme : Maybe String
  sourcedir : String
  version : String
  localDeps : List String
  gitDeps : List GitDep
%runElab (deriveFromDhall Record `{ PackageDhall })

depPackageDhallType : String
depPackageDhallType = "{ name : Text, ns : Text, requirement : Text }"

Show PackageDhall where
  show (MkPackageDhall {depends, deps, description, devDeps, executable, license, link, main, modules, ns, package, readme, sourcedir, version, localDeps, gitDeps}) =
    """
    { depends = \{show depends} : List Text
    , deps = \{show deps} : List \{ depPackageDhallType }
    , description = \{show description}
    , devDeps = \{show devDeps}
    , executable = \{show executable}
    , license = \{show license}
    , link = \{show link}
    , main = \{show main}
    , modules = \{show modules}
    , ns = \{show ns}
    , package = \{show package}
    , readme = \{show readme}
    , sourcedir = \{show sourcedir}
    , version = \{show version}
    , localDeps =
      \{show localDeps} : List String
    , gitDeps =
      \{show gitDeps} : List String
    }
    """

parsePackageDhall' : String -> IO $ Either String PackageDhall
parsePackageDhall' path = do
  Right package <- liftIOEither $ deriveFromDhallString {ty=PackageDhall} path
    | Left err => do
        pure $ Left $ show err
  pure $ Right package

inigoPackageFromDhall : PackageDhall -> Either String Package
inigoPackageFromDhall (MkPackageDhall {depends, deps, description, devDeps, executable, license, link, main, modules, ns, package, readme, sourcedir, version, localDeps, gitDeps}) =
  let packageVersion = !(versionFromDhall version)
      packageDeps = !(traverse depFromDhall deps)
      packageDevDeps = !(traverse depFromDhall devDeps)
      packageExtraDepsFromLocal = MkExtraDep SubDir () "TODO unused?" localDeps
      packageExtraDepsFromGit = map extraDepFromGit gitDeps
  in
  pure $ MkPackage
    { ns=ns
    , package=package
    , version=packageVersion
    , description=description
    , link=link
    , readme=readme
    , modules=modules
    , depends=depends
    , license=license
    , sourcedir=sourcedir
    , main=main
    , executable=executable
    , deps=packageDeps
    , devDeps=packageDevDeps
    , extraDeps=packageExtraDepsFromLocal :: packageExtraDepsFromGit
    }
  where
    requirementFromDhall : String -> Either String Requirement
    requirementFromDhall x =
      case parseRequirement x of
           Nothing => Left "Error parsing Requirement"
           (Just r) => pure r
    versionFromDhall : String -> Either String Version
    versionFromDhall x =
      case parseVersion x of
           Nothing => Left "Error parsing Version"
           (Just v) => pure v
    extraDepFromGit : GitDep -> ExtraDep
    extraDepFromGit (MkGitDep commit url subDirs) =
      MkExtraDep Git commit url subDirs
    depFromDhall : DepPackage -> Either String (List String, Requirement)
    depFromDhall (MkDepPackage ns name requirement) =
      pure ([ns, name], !(requirementFromDhall requirement))

export
parsePackageDhall : String -> Promise $ Either String Package
parsePackageDhall x = do
  Right package <- liftIO $ parsePackageDhall' x | Left err => pure $ Left err
  pure $ inigoPackageFromDhall package
