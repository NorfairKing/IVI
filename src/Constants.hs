module Constants where

import System.Directory (getHomeDirectory)
import System.FilePath.Posix ((</>))

iviName, iviExtension, iviDirname :: String
iviName = "IVI"
iviExtension = "ivi"
iviDirname = "." ++ iviName


iviDirectory, iviScriptsFile :: IO FilePath
iviDirectory = getHomeDirectory >>= (\x -> return $ x </> iviDirname)
iviScriptsFile = iviDirectory >>= (\x -> return $ x </> scriptsFileName)
    where scriptsFileName = "scripts" ++ "." ++ iviExtension

iviScriptFunction :: String 
iviScriptFunction = "execute"

