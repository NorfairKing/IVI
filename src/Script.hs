module Script where

-- The datastructure that is given to run a script.
data IVIScriptArgs = Args
                        String -- Raw command
                        String -- Script name

data IVIScript = Script
                    String -- Script name

data IVIExitStatus = ExitSuccess 
                   | ExitFailure String
