module Main where

import System.Environment (getArgs)
import System.Process (runCommand, waitForProcess)
import System.Exit (exitWith)


main :: IO ()
main = do
    args <- getArgs
    case recognise args of
        Nothing -> do
            phandle <- runCommand $ unwords args
            exitcode <- waitForProcess phandle
            exitWith exitcode
        Just scriptArgs -> putStrLn "placeholder"



data IVIScriptArgs = ScriptArgs
                        [String] -- Raw command
                         String  -- Script name


recognise :: [String] -> Maybe IVIScriptArgs
recognise args = Nothing -- ScriptArgs args scriptName
