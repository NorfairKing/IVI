import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist)
import Control.Monad (filterM)

scriptListFile :: FilePath
scriptListFile = "ScriptsList.hs"

main :: IO ()
main = do
    candidates <- filterM isScriptDir =<< getDirectoryContents =<< getCurrentDirectory
    scripts <- mapM strapScript candidates
    joinList scripts

-- Determine whether the given path leads to a script directory
isScriptDir :: FilePath -> IO Bool
isScriptDir "."  = return False
isScriptDir ".." = return False
isScriptDir dirName = do
    isdir <- doesDirectoryExist dirName
    if not isdir
    then return False
    else do
        dirContents <- getDirectoryContents dirName
        let stuff = filter (\x -> takeExtension x == ".ivi") dirContents
        print stuff
        return True



strapScript :: FilePath -> IO (String, String)
strapScript dir = do 
    iviFile <- getIviFile dir
    case iviFile of
        Nothing -> return ("","")
        Just file -> parse dir file
                

getIviFile :: FilePath -> IO (Maybe FilePath)
getIviFile dir = do
    isDir <- doesDirectoryExist dir
    if isDir
    then do
        dirContents <- getDirectoryContents dir
        return $ Just (dir </> "script.ivi")
    else
        return $ Nothing

parse :: FilePath -> FilePath -> IO (String, String)
parse dir iviFile = do
    cts <- readFile iviFile  
    let ls = lines cts
    let [scriptFileName, name, function] = ls
    return (
            "import Scripts." ++ dir ++ "." ++ scriptFileName
            , "(\""++ name ++ "\", Script \"" ++ name ++ "\" " ++ function ++ ")"
            )

joinList :: [(String, String)] -> IO()
joinList scripts = do
    writeFile scriptListFile contents
    where 
        imports = filter (not.null) $ map fst scripts          
        entries = map prependIndentation $ filter (not.null) $ map snd scripts
        prependIndentation str = "            " ++ str
        contents = "module Scripts.ScriptsList where\n"
                ++ "import Script\n"
                ++ unlines imports
                ++ "scripts :: [(String, IVIScript)] \n"
                ++ "scripts = [\n"
                ++ unlines entries
                ++ "          ]\n"
