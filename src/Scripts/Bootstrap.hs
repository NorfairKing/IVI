import Data.List (isPrefixOf)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist)

scriptListFile :: FilePath
scriptListFile = "ScriptsList.hs"

main :: IO ()
main = do
    here <- getCurrentDirectory
    contents <- getDirectoryContents here
    let candidates = filter strapCondition contents
    scripts <- mapM strap candidates
    joinList scripts

strap :: FilePath -> IO (String, String)
strap dir = do 
    iviFile <- getIviFile dir
    case iviFile of
        Nothing -> return ("","")
        Just file -> parse dir file
                

                
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

strapCondition :: FilePath -> Bool
strapCondition fileName = not $ "." `isPrefixOf` fileName

getIviFile :: FilePath -> IO (Maybe FilePath)
getIviFile dir = do
    isDir <- doesDirectoryExist dir
    if isDir
    then do
        dirContents <- getDirectoryContents dir
        putStrLn $ unlines dirContents
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
