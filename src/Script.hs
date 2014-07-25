module Script where


-- The datastructure that is given to run a script.
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

scriptNoOp :: IVIScriptArgs -> IO IVIScriptResult
scriptNoOp _ = return Success
