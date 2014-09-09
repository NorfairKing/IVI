{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Constants
Description : IVI's constant values
-}
module Constants (
    -- * Strings
      iviName
    , iviExtension
    , iviDirName
    -- * Paths
    , iviHomeDirectory
    , iviSrcDirectory
    ) where

import System.Directory (getHomeDirectory, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
--import System.FilePath.Posix ((</>))
import Data.Functor ((<$>))
import Language.Haskell.TH (runIO, location, stringL, litE, loc_filename)

iviName, iviExtension, iviDirName :: String

-- | The name of the program. This is constant, yes.
iviName = "IVI"

-- | The extension for ivi config files
iviExtension = "ivi"

-- | The name of the ivi config directory
iviDirName = "." ++ iviName


-- | The users IVI directory in the home directory: ~/.IVI
iviHomeDirectory :: IO FilePath
iviHomeDirectory = getHomeDirectory >>= (\x -> return $ x </> iviDirName)

-- | The absolute path to the parent of the source directory this file is in.
iviRootDirectory :: FilePath
iviRootDirectory = takeDirectory iviSrcDirectory

-- | The absolute path to the source directory of ivi code
iviSrcDirectory :: FilePath
iviSrcDirectory = takeDirectory $(do 
    dir <- runIO getCurrentDirectory
    filename <- loc_filename <$> location
    litE $ stringL $ dir </> filename)

iviScriptsDirrectory :: FilePath
iviScriptsDirectory = iviSrcDirectory </> "Scripts"
