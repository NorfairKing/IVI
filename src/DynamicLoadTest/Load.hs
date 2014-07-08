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



-- |  List all exports of this module
--    and evaluate a symbol from a module DynTest 
main :: IO ()
main = 
  runGhc (Just libdir) $ do
    -- putString ":::Display exports of modules:::"
    modSums <- initSession ["DynTest"]
    --let thisModSum = head modSums
    --exports <- listExports thisModSum
    --mapM_ putString exports

    -- putString ":::Evaluate a name from module DynTest:::"
    importDecl_RdrName <- parseImportDecl "import DynTest as D"
    setContext [IIDecl importDecl_RdrName]
    dynVal <- dynCompileExpr "D.aString"
    liftIO $ print $ (fromDyn dynVal "nope-nothing")


-- | Init interactive session and load modules
initSession :: [String] -> Ghc SuccessFlag
initSession modStrNames = do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags {
    hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
    }
  targets <- mapM
              (\modStrName -> do
               --putString modStrName
                  target <- guessTarget ("*"++modStrName++".hs") Nothing
                  return target
              ) modStrNames
  setTargets targets
  load LoadAllTargets
  --modSums <- mapM
  --            (\modStrName -> do
  --                --putString modStrName
  --                modSum <- getModSummary $ mkModuleName modStrName
  --                return $ ms_mod modSum
  --            ) modStrNames
  --areturn modSums


-- | List exported names of this or a sibling module
listExports :: GhcMonad m => Module -> m [String]
listExports mod = do
  maybeModInfo <- getModuleInfo mod
  case maybeModInfo of
    (Just modInfo) -> do
      let expNames = modInfoExports modInfo
          expStrNames = map getOccString expNames
      return expStrNames
    _ -> return []

-- | Util for printing
putString :: String -> Ghc ()
putString = liftIO . putStrLn
