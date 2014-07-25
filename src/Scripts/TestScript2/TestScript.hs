module Scripts.TestScript2.TestScript where

import Script

execute :: IVIScriptArgs -> IO IVIScriptResult
execute _ = 
    do 
        putStrLn "this is test script 2"
        return Success
