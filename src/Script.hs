module Script where

import Control.Monad
import Language.Haskell.Interpreter

-- The datastructure that is given to run a script.
data IVIScriptArgs = Args
                        String -- Raw command
                        String -- Script name

data IVIScript = Script
                    String -- Script name

data IVIExitStatus = ExitSuccess 
                   | ExitFailure String




executeDynamic :: FilePath -> String -> String -> Interpreter ()
executeDynamic path mod fun =
    do
      loadModules [path]
      setImportsQ [("Prelude", Nothing), (mod, Just qualification)]
      let action = qualification ++ "." ++ fun
      interpret action (as :: IO() ) >>= liftIO
       where qualification = "DynModule"

testExecute :: IO()
testExecute = do
  r <- runInterpreter $ executeDynamic "/home/syd/ivi/scripts/TestScript.hs" "TestScript" "execute"
  case r of
      Left _ -> putStrLn "woops"
      Right () -> putStrLn "done"
