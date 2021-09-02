module Client.Action.Init

import Client.Skeleton.Skeleton
import Data.List
import Fmt
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Template
import System.Path

export
init :
    String -> -- template
    String -> -- package namespace
    String -> -- package name
    Promise ()
init tmplFile packageNS packageName = do
    tmplInp <- fs_readFile tmplFile
    tmpl <- liftEither $ runTemplate packageNS packageName tmplFile tmplInp
    ignore $ all $ map writeTmplFile tmpl
    log (fmt "Successfully built %s" tmplFile)
  where
    ensureParent : String -> Promise ()
    ensureParent path = case parent path of
        Just parentPath => unless (parentPath == "") $ fs_mkdir True parentPath
        Nothing => pure ()

    writeTmplFile : (String, String) -> Promise ()
    writeTmplFile (path, contents) = do
        ensureParent path
        fs_writeFile path contents
