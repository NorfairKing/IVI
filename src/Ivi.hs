module Main where

import Data.List (find)
import System.Environment (getArgs)
import System.Exit (exitSuccess,exitFailure)
import Text.Regex.Posix ((=~))

import Script
import Scripts.ScriptsList (scripts)

main :: IO ()
main = do    
    args <- getArgs         
    -- Try to make out which script is meant.
    case recognise args of
    
        -- Do nothing when no command is recognised.
        Nothing -> do
            putStrLn "Command not recognised."
            exitFailure
            
        -- Run the script that is recognised.
        Just name -> do
            let scrargs = Args $ unwords args
            executeScript name scrargs



-- Try to make out which script is meant by the given arguments.
recognise :: [String] -> Maybe IVIScript
recognise [] = Nothing
recognise args = do
    let scriptByName = findByName
    case scriptByName of
        Just _ -> scriptByName
        Nothing -> findByRegex
    where
        findByName = find (\(Script name _ _) -> name == head args) scripts
        findByRegex = find (\(Script _ _ regexes) -> unwords args `matchesAnyOf` regexes) scripts
        matchesAnyOf str = any (str =~)

-- Execute the script given by its name
executeScript :: IVIScript -> IVIScriptArgs -> IO ()
executeScript (Script _ exec _) args = do
    result <- exec args
    case result of
        Success -> exitSuccess
        Failure str -> do
            putStrLn $ "ERROR: " ++ str
            exitFailure

