module Speech where

import           Dependency     (withDependencies)
import           System.Process (runCommand, waitForProcess)

say :: String -> IO ()
say str = withDependencies [playBin, generationBin] go
    where
        go = do
            generateSound
            playSound
        generateSound = do
                let cmd = unwords [generationBin, "-l=" ++ language, "--wave=" ++ soundFile, "\"" ++ str ++ "\""]
                h <- runCommand $ cmd
                _ <- waitForProcess h
                return ()
        playSound = do
                let cmd = unwords [playBin, "--no-show-progress", soundFile ]
                h <- runCommand cmd
                _ <- waitForProcess h
                return ()
        soundFile = "/tmp/speech.wav"
        generationBin = "pico2wave"
        playBin = "play"
        language = "en-GB"
