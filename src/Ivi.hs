module Main where

import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)
import System.Exit (exitWith)


main :: IO ()
main = do
    args <- getArgs
    
    -- Try to make out which script is meant.
    case recognise args of
    
        -- Run the arguments as a shell command if no script is recognised.
        Nothing -> do
            phandle <- runCommand $ unwords args
            exitcode <- waitForProcess phandle
            exitWith exitcode
            
        -- Run the script that is recognised.
        Just scriptArgs -> putStrLn "placeholder"


-- The datastructure that is given to run a script.
data IVIScriptArgs = ScriptArgs
                        String -- Raw command
                        String  -- Script name


-- Try to make out which script is meant by the given arguments.
recognise :: [String] -> Maybe IVIScriptArgs
recognise args = Nothing -- ScriptArgs args scriptName
