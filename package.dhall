{ depends = [] : List Text
, deps = [{ ns = "Base" , name = "IdrTest", requirement = "0.0.1" }]
, description = Some "a random test package"
, devDeps = [{ ns = "Other" , name = "SomeDebug", requirement = "1.0.1" }]
, executable = Some "MyPkg"
, license = Some "stuff"
, link = Some "www.example.com"
, main = None
, modules = [] : List Text
, ns = "Alexhumphreys"
, package = "MyPkg"
, readme = Some "./README.md"
, sourcedir = "."
, version = "9.0.0"
, localDeps = ["./someDir"] : List Text
, gitDeps =
  [ { url = "git@github.com/foo/bar"
    , commit = "master"
    , subDirs = ([] : List Text)
    }
  ] : List { url : Text, commit : Text, subDirs : List Text }
}
