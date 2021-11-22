{ ns = "MyNameSpace"
, package = "MyPkg"
, version = "9.0.0"
, sourcedir = "./."
, description = Some "a random test package"
, executable = Some "MyPkg"
, modules = [] : List Text
, readme = Some "./README.md"
, license = Some "MPL2"
, link = Some "www.example.com"
, main = Some "MyPkg"
, depends = [] : List Text
, deps = [{ ns = "Base" , name = "IdrTest", requirement = "0.0.1" }]
, devDeps = [{ ns = "Other" , name = "SomeDebug", requirement = "1.0.1" }]
, localDeps = ["./someDir"] : List Text
, gitDeps =
  [ { url = "git@github.com/foo/bar"
    , commit = "master"
    , subDirs = ([] : List Text)
    }
  ] : List { url : Text, commit : Text, subDirs : List Text }
} : ./Type.dhall
