module Dependency where

import System.Process
import System.IO
import System.Exit

data Dependency = Command String -- Name

checkDependency :: Dependency -> IO Bool
checkDependency (Command name) = do
    nullHandleOut <- openFile "/dev/null" AppendMode
    nullHandleIn  <- openFile "/dev/null" ReadMode
    let process = (shell cmd)
            { std_out  = UseHandle nullHandleOut
            , std_err = UseHandle nullHandleOut
            , std_in  = UseHandle nullHandleIn}
    (_, _, _, ph) <- createProcess process 
    ex <- waitForProcess ph
    case ex of
        ExitSuccess -> return True
        ExitFailure _ -> return False
    
    where
        cmd = "command -v" ++ " " ++ name
