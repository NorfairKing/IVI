{-# LANGUAGE DeriveDataTypeable #-}

module Script where

import Language.Haskell.Interpreter
import Data.Typeable.Internal

import Constants

-- The datastructure that is given to run a script.
data IVIScriptArgs = Args
                        String -- Raw command
                        String -- Script name
                     deriving (Show ,Typeable)

data IVIScript = Script
                    String -- Script name
                    FilePath -- Script dir
                     deriving (Show)

-- TODO remove
testExecute = executeScript (Script "TestScript" "/home/syd/ivi/scripts/TestScript.hs") (Args "heloo" "sweety")

executeDynamic :: IVIScript -> IVIScriptArgs -> Interpreter ()
executeDynamic (Script modu path) args=
    do
      -- Load the script module
      loadModules [path]

      -- Set the imports that the script can use
      setImportsQ [
                     ("Prelude", Nothing)
                   , ("Script", Nothing)
                   , (modu, Just qualification) -- The script itself
                  ]
      
      -- Define the name of the function to be gotten
      let action = qualification ++ "." ++ iviScriptFunction
       
      -- Get the function 
      ioFunction <- interpret action (as :: IVIScriptArgs -> IO ())
      
      -- evaluate it
      liftIO $ ioFunction args
          where 
            qualification = "DynModule" -- Just to avoid conflicting names
      

executeScript :: IVIScript -> IVIScriptArgs -> IO()
executeScript script args = do
  r <- runInterpreter $ executeDynamic script args
  case r of
      -- Everything was fine!
      Right _ -> return ()
                 
      -- Something went wront
      Left e -> case e of
                  UnknownError str -> putStrLn $ "ERROR, something went wrong: " ++ str
                  WontCompile errs -> putStrLn $ "ERROR, compilation errors: " ++  (unlines $ map toStr errs)
                  NotAllowed str   -> putStrLn $ "ERROR, you are not allowed to do this: " ++ str	 
                  GhcException str -> putStrLn $ "ERROR, something went wrong in ghc: " ++ str

      where 
        toStr :: GhcError -> String
        toStr e = errMsg e
