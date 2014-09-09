{-|
Module      : Input
Description : All encapsulated input functionality for IVI
-}
module Input where

import           Control.Exception (bracket_)
import           System.IO         (hFlush, hGetEcho, hSetEcho, stderr, stdin,
                                    stdout)

-- | prompt for a line with the given prompt string
promptLine :: String -> IO String
promptLine str = do
    putStr $ str ++ " > "
    answer <- getLineNow
    return answer

-- | Prompt for a number with the given prompt string
promptNum :: (Num a, Read a) => String -> IO a
promptNum str = do
    ans <- promptLine str
    let num = read ans
    return num

-- | Prompt for a password
promptPassword :: IO String
promptPassword = promptSilently "password"

-- | Prompt for a line without echoing typed charactes back to the screen
promptSilently :: String -> IO String
promptSilently str = do
    putStr $ str ++ " > "
    getSilentLine

-- | Get a line without echoing typed charactes back to the screen
getSilentLine :: IO String
getSilentLine = do
    hFlush stdout
    pass <- withEcho False getLineNow
    putChar '\n'
    return pass

-- | Get a line of input, but make sure all output and error IO is done beforehand
getLineNow :: IO String
getLineNow = do
    hFlush stdout
    hFlush stderr
    answer <- getLine
    return answer

-- | Perform an IO action with or without echoing typed characters back to the screen
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
