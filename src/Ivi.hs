{-|
Module      : Main
Description : IVI, the main module
-}
module Main (
    -- * Main function
      main
    -- ** Recognition
    , recognize
    -- ** Execution
    , executeScript
    ) where

import Data.List (find)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Exit (exitSuccess,exitFailure)
import Text.Regex.Posix ((=~))

import Script
import Constants
import Scripts.ScriptsList (scripts)

-- | Run IVI
main :: IO ()
main = do    
    args <- getArgs         
    -- Try to make out which script is meant.
    case recognize args of
    
        -- Do nothing when no command is recognised.
        Nothing -> do
            putStrLn "Command not recognized."
            exitFailure
            
        -- Run the script that is recognised.
        Just name -> do
            let scrargs = Args $ unwords args
            executeScript name scrargs



-- | Try to make out which script is meant by the given arguments.
recognize :: [String] -- ^ The bare command-line arguments
          -> Maybe IVIScript -- ^ Either the recognised script, or nothing
recognize [] = Nothing
recognize args = do
    if isJust findByName
    then findByName
    else findByRegex
    where
        findByName = find (\(Script name _ _) -> name == head args) scripts
        findByRegex = find (\(Script _ _ regexes) -> unwords args `matchesAnyOf` regexes) scripts
        matchesAnyOf str = any (str =~)

-- | Execute the script given by its name
executeScript :: IVIScript -- ^ The script to execute
              -> IVIScriptArgs -- ^ The arguments to the script
              -> IO ()
executeScript (Script _ exec _) args = do
    result <- exec args
    case result of
        Success -> exitSuccess
        Failure str -> do
            putStrLn $ "ERROR: " ++ str
            exitFailure

