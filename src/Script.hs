{-# LANGUAGE DeriveDataTypeable #-}

module Script where

import Language.Haskell.Interpreter
import Data.Typeable.Internal

-- The datastructure that is given to run a script.
data IVIScriptArgs = Placeholder 
                     | Args
                        String -- Raw command
                        String -- Script name
                     deriving (Show ,Typeable)

data IVIScript = Script
                    String -- Script name
                    FilePath -- Script dir
                     deriving (Show)


executeDynamic :: IVIScript -> IVIScriptArgs -> Interpreter (IO ())
executeDynamic (Script mod path) args=
    do
      -- Load the script module
      loadModules [path]

      -- Set the imports that the script can use
      setImportsQ [("Prelude", Nothing), ("Script", Nothing), (mod, Just qualification)]
      
      -- Define the name of the function to be gotten
      let action = qualification ++ "." ++ fun
       
      -- Get the function 
      toEx <- interpret action (as :: IVIScriptArgs -> IO ())
      
      -- evaluate it
      return $ toEx args
          where 
            qualification = "DynModule"
            fun = "execute"


testExecute :: IO()
testExecute = do
  let testScript = Script "TestScript" "/home/syd/ivi/scripts/TestScript.hs"
  r <- runInterpreter $ executeDynamic testScript (Args "wut" "wut²")
  case r of
      Left e -> putStrLn $ "woops... " ++ show e
      Right stuff -> stuff


say :: String -> Interpreter ()
say = liftIO . putStrLn
