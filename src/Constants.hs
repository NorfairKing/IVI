{-|
Module      : Constants
Description : IVI's constant values
-}
module Constants (
    -- * Strings
      iviName
    , iviExtension
    , iviDirname
    -- * Paths
    , iviDirectory
    ) where

import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))

iviName, iviExtension, iviDirname :: String

-- | The name of the program. This is constant, yes.
iviName = "IVI"

-- | The extension for ivi config files
iviExtension = "ivi"

-- | The name of the ivi config directory
iviDirname = "." ++ iviName


-- | The users IVI directory 
iviDirectory :: IO FilePath
iviDirectory = getHomeDirectory >>= (\x -> return $ x </> iviDirname)
