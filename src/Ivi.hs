module Main where

import Data.List (find)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (runCommand, waitForProcess)

import Script
import Scripts.ScriptsList (scripts)

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
        Just name -> do
            let scrargs = Args $ unwords args
            executeScript name scrargs
                           




-- Try to make out which script is meant by the given arguments.
recognise :: [String] -> Maybe String
recognise args = Just $ head args -- Just (Args $ unwords args)

executeScript :: String -> IVIScriptArgs -> IO ()
executeScript name args = do
    let mscript = find (\(n,_,_) -> n == name) scripts
    case mscript of
        Nothing -> putStrLn "something went wrong"
        Just (_,(Script name exec _),_) -> do
            result <- exec args
            case result of
                Success -> putStrLn "yeah"
                Failure str -> putStrLn $ "nope: " ++ str

