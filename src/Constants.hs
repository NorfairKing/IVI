module Constants where

import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))

iviName = "IVI"
iviExtension = "ivi"
iviDirname = "." ++ iviName

iviDirectory = getHomeDirectory >>= (\x -> return $ x </> iviDirname)
iviScriptsFile = iviDirectory >>= (\x -> return $ x </> scriptsFileName)
    where scriptsFileName = "scripts" ++ "." ++ iviExtension

