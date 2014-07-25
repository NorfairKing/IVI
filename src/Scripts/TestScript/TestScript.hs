module Scripts.TestScript.TestScript where

import Script

execute :: IVIScriptArgs -> IO IVIScriptResult
execute _ = 
    do 
        putStrLn "this is testscript 1"
        return Success
