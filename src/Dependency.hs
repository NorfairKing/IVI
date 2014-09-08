module Dependency where

import System.Process
import System.IO
import System.Exit

type Dependency = String -- Name

checkDependency :: Dependency -> IO Bool
checkDependency dep = do
    nullHandleOut <- openFile "/dev/null" AppendMode
    nullHandleIn  <- openFile "/dev/null" ReadMode
    let process = (shell cmd)
            { std_out = UseHandle nullHandleOut
            , std_err = UseHandle nullHandleOut
            , std_in  = UseHandle nullHandleIn }

    (_, _, _, ph) <- createProcess process 
    ex <- waitForProcess ph
    case ex of
        ExitSuccess -> return True
        ExitFailure _ -> return False
    
    where
        cmd = "command -v" ++ " " ++ dep

withDependency :: Dependency -> IO a -> IO a
withDependency dep fun = do
    meets <- checkDependency dep
    if meets
    then fun
    else error $ "Missing dependency: " ++ dep

withDependencies :: [Dependency] -> IO a -> IO a
withDependencies [] fun = fun
withDependencies (d:ds) fun = withDependency d $ withDependencies ds fun

