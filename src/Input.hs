module Input where

import System.IO (stdout, stdin, hFlush, hSetEcho, hGetEcho)
import Control.Exception (bracket_)


promptLine :: String -> IO String
promptLine str = do
    putStr $ str ++ " > "
    hFlush stdout -- To make sure the putStr happens _before_ getLine
    answer <- getLine
    return answer

promptInteger :: String -> IO Integer
promptInteger str = do
    ans <- promptLine str
    let num = read ans :: Integer
    return num

promptPassword :: IO String
promptPassword = promptSilently "password"

promptSilently :: String -> IO String
promptSilently str = do
    putStr $ str ++ " > "
    getSilentLine

getSilentLine :: IO String
getSilentLine = do
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
