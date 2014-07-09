module Main (main) where

import Control.Monad
import Language.Haskell.Interpreter

main :: IO ()
main = do 
  r <- runInterpreter testHint'
  case r of
    Left err -> printInterpreterError err
    Right () -> putStrLn "Done"
  
testHint' :: Interpreter ()
testHint' =
    do
      say "Trying to execute a script from an absolute path."
      executeScript "/home/syd/ivi/scripts/TestScript.hs" "TestScript" "execute"
      say "It worked!"

executeScript :: FilePath -> String -> String -> Interpreter ()
executeScript fullpath mod fun =
    do
      loadModules [fullpath]
      setImportsQ [("Prelude", Nothing),(mod,Nothing)]
      interpret fun (as :: IO ()) >>= liftIO


say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)
