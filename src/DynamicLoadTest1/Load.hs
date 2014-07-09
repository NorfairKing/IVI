-----------------------------------------------------------------------------
-- | Example for loading Haskell source code dynamically using the GHC api
-- Useful links:
-- GHC api:
-- http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/GHC.html
-- Wiki:
-- http://www.haskell.org/haskellwiki/GHC/As_a_library
-----------------------------------------------------------------------------
module Main where

import GHC
import GhcMonad (liftIO)
import GHC.Paths (libdir)
import Name (getOccString)
import Data.Dynamic (fromDyn)



main :: IO ()
main = 
  runGhc (Just libdir) $ do
    modSums <- initSession ["DynTest"]

    importDecl_RdrName <- parseImportDecl "import Subpackage.Dyntest as D"
    setContext [IIDecl $ simpleImportDecl (mkModuleName "Subpackage.Dyntest")]
    dynVal <- dynCompileExpr "D.aString"
    liftIO $ print $ (fromDyn dynVal "nope-nothing")


-- | Init interactive session and load modules
initSession :: [String] -> Ghc SuccessFlag
initSession modStrNames = do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags {
    hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    -- , importPaths = ["/home/syd/ivi/scripts/"]
    }
  targets <- mapM
              (\modStrName -> do
                  target <- guessTarget ("*"++modStrName++".hs") Nothing
                  return target
              ) modStrNames
  setTargets targets
  load LoadAllTargets

