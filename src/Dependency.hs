module Dependency where

import System.Directory (findExecutable)

type Dependency = String -- Name

checkDependency :: Dependency -> IO Bool
checkDependency dep = do
    mf <- findExecutable dep
    case mf of
        Just fp -> return True
        Nothing -> return False

withDependency :: Dependency -> IO a -> IO a
withDependency dep fun = do
    meets <- checkDependency dep
    if meets
    then fun
    else error $ "Missing dependency: " ++ dep

withDependencies :: [Dependency] -> IO a -> IO a
withDependencies [] fun = fun
withDependencies (d:ds) fun = withDependency d $ withDependencies ds fun

