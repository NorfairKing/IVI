{-|
Module      : Dependency
Description : Support for external dependencies.
-}
module Dependency where

import           System.Directory (findExecutable)

-- | An external dependency
type Dependency = String -- Name

-- | Check whether a given dependency is satisfied
checkDependency :: Dependency -> IO Bool
checkDependency dep = do
    mf <- findExecutable dep
    case mf of
        Just fp -> return True
        Nothing -> return False

-- | Perform an IO action, but only when the dependency is resolved.
withDependency :: Dependency -> IO a -> IO a
withDependency dep fun = do
    meets <- checkDependency dep
    if meets
    then fun
    else error $ "Missing dependency: " ++ dep

-- | Perform an IO action, but only when all the dependencies are resolved.
withDependencies :: [Dependency] -> IO a -> IO a
withDependencies [] fun = fun
withDependencies (d:ds) fun = withDependency d $ withDependencies ds fun

