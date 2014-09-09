{-|
Module      : Script
Description : All script datastructures
-}
module Script (
    -- * Data structures
      IVIScriptArgs(..)
    , IVIScript(..)
    , IVIScriptResult(..)
    ) where

-- | IVI script arguments
data IVIScriptArgs =
    -- | Script arguments consist of the raw command (currently)
    Args
    String -- Raw command
    deriving (Show)

-- | IVI script
data IVIScript =
    -- | Scripts consist of their name, the function to enter and the regexes to match
    Script
        String -- Script name
        (IVIScriptArgs -> IO IVIScriptResult)-- execute function
        [String] -- Regexes

-- | IVI script result
data IVIScriptResult
    -- | The script executed with success.
    = Success
    -- | The script executed with (an) error(s).
    | Failure String -- Description of what went wrong
    deriving (Show)

