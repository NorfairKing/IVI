module Scripts.TestScript.TestScript where

import Script

execute :: IVIScriptArgs -> IO IVIScriptResult
execute _ = 
    do 
        putStrLn "hi"
        return Success
