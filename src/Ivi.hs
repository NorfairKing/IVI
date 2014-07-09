module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (runCommand, waitForProcess)

import Script

main :: IO ()
main = do    
    testExecute

    args <- getArgs         
    -- Try to make out which script is meant.
    case recognise args of
    
        -- Run the arguments as a shell command if no script is recognised.
        Nothing -> do
            phandle <- runCommand $ unwords args
            exitcode <- waitForProcess phandle
            exitWith exitcode
            
        -- Run the script that is recognised.
        Just scriptArgs -> print scriptArgs
                           




-- Try to make out which script is meant by the given arguments.
recognise :: [String] -> Maybe IVIScriptArgs
recognise args = Just (Args (unwords args) "more")
