{ depends : List Text
, deps : List { name : Text, ns : Text, requirement : Text }
, description : Optional Text
, devDeps : List { name : Text, ns : Text, requirement : Text }
, executable : Optional Text
, gitDeps : List { commit : Text, subDirs : List Text, url : Text }
, license : Optional Text
, link : Optional Text
, localDeps : List Text
, main : Optional Text
, modules : List Text
, ns : Text
, package : Text
, readme : Optional Text
, sourcedir : Text
, version : Text
}
