*************
Dhall support
*************

Inigo packages can be configured with a `Inigo.dhall` file, as opposed to the default `Inigo.toml`. The format is as follows:

.. code-block::
    { ns = "MyNameSpace"
    , package = "MyPkg"
    , version = "9.0.0"
    , sourcedir = "."
    , description = Some "a random test package"
    , executable = Some "MyPkg"
    , modules = [] : List Text
    , readme = Some "./README.md"
    , license = Some "MPL2"
    , link = Some "www.example.com"
    , main = None Text
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
    }

``ns`` stands for namespace and it's used if you publish to the central package repository.
``package`` is the name of your package.
``version`` is the version of your package.
``sourcedir`` is the path to your modules.
``description`` a brief description of your package.
``executable`` the name of the executable your project compiles to.
``modules`` is a list of all the modules your package exports.
``readme`` path to your package's README file.
``license`` the license your package is published under.
``deps`` describes your packages dependencies on the central package repository.
``devDeps`` same as ``deps`` but only used during development, not needed for the final release.
``localDeps`` package dependencies that are defined in your repository.
``gitDeps`` package dependencies that are hosted remotely on github.

You can use the example file `docs/src/Inigo.dhall` to get started.

Dhall support is provided by the package
https://github.com/alexhumphreys/idrall, so any issues you run into with Dhall
parsing/evaluating should be filed there. It is still in alpha so expect some
rough edges.

For more information on the Dhall language, visit https://dhall-lang.org.
