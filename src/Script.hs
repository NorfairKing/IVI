{-# LANGUAGE DeriveDataTypeable #-}

module Script where

import Language.Haskell.Interpreter
import Data.Typeable.Internal

-- The datastructure that is given to run a script.
data IVIScriptArgs = Placeholder 
                     | Args
                        String -- Raw command
                        String -- Script name
                     deriving (Typeable)

data IVIScript = Script
                    String -- Script name
                    FilePath -- Script dir

data IVIExitStatus = ExitSuccess -- Everything went allright! 
                   | ExitFailure String -- Something went wrong, as specified by the string
                     deriving (Typeable)



executeDynamic :: FilePath -> String -> String -> Interpreter ()
executeDynamic path mod fun =
    do
      loadModules [path]
      setImportsQ [("Prelude", Nothing), ("Script", Nothing), (mod, Just qualification)]
      let scriptArgs = Placeholder
      let action = qualification ++ "." ++ fun
      toEx <- interpret action (as :: IVIScriptArgs -> IO IVIExitStatus)
      status <- liftIO $ toEx scriptArgs
      case status of
        ExitSuccess -> say "yay"
        ExitFailure str -> say $ "aww," ++ str
       where qualification = "DynModule"

testExecute :: IO()
testExecute = do
  let testScript = Script "TestScript" "/home/syd/ivi/scripts/TestScript.hs"
  r <- runInterpreter $ executeDynamic "/home/syd/ivi/scripts/TestScript.hs" "TestScript" "execute"
  case r of
      Left e -> putStrLn $ "woops... " ++ show e
      Right () -> putStrLn "done"

say :: String -> Interpreter ()
say = liftIO . putStrLn
