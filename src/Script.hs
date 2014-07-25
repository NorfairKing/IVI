module Script where

data IVIScriptArgs = Args
                        String -- Raw command
    deriving (Show)

data IVIScript = Script
                    String -- Script name
                    (IVIScriptArgs -> IO IVIScriptResult)-- execute function
                    [String] -- Regexes

data IVIScriptResult = Success
                        | Failure String -- Description of what went wrong
    deriving (Show)

