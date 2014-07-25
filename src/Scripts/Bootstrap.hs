import Control.Monad (filterM)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import System.FilePath.Posix (takeExtension)

scriptListFile :: FilePath
scriptListFile = "ScriptsList.hs"

-- | Execute IVI's bootstrap procedure
main :: IO ()
main = do
    candidates <- filterM isScriptDir =<< getDirectoryContents =<< getCurrentDirectory
    scripts <- mapM parseScriptDir candidates
    let fileContents = joinList $ concat scripts
    writeFile scriptListFile fileContents    
    
-- | Determine whether the given path leads to a script directory
isScriptDir :: FilePath -> IO Bool
isScriptDir "."  = return False
isScriptDir ".." = return False
isScriptDir dirName = do
    isdir <- doesDirectoryExist dirName
    if not isdir
    then return False
    else do
        iviFiles <- getIviFiles dirName
        return $ (not . null) iviFiles

-- | Get all ivi files in a given directory
getIviFiles :: FilePath -> IO [FilePath]
getIviFiles dir = do
        dirContents <- getDirectoryContents dir
        let iviFiles = filter (\x -> takeExtension x == ".ivi") dirContents
        return iviFiles    

-- | Parse all scripts in a script dir
parseScriptDir :: FilePath -> IO [(String, String)]
parseScriptDir dir = do
    scriptVersion <- readFile $ dir </> "VERSION"
    iviVersion <- readFile $  ".." </> ".." </> "VERSION"
    if checkVersion scriptVersion iviVersion
    then do
        iviFiles <- getIviFiles dir  
        putStrLn dir   
        scripts <- mapM (parseScript dir) iviFiles
        putStrLn ""    
        return scripts    
    else do 
        putStrLn $ "Not adding scripts in " ++ dir ++ "because the ivi versions aren't compatible"
        return []

checkVersion :: String -> String -> Bool
checkVersion sv iv = 
    sa == ia && sb == ib && ic >= sc
    where
        (sa,sb,sc,_) = stripVersion sv
        (ia,ib,ic,_) = stripVersion iv 

stripVersion :: String -> (String, String, String, String)
stripVersion str = (\[a,b,c,d] -> (a,b,c,d)) $ go str [] []
    where
        go [] acc accs = accs ++ [acc]
        go ('.':vs) acc accs = go vs [] (accs ++ [acc])
        go ( c :vs) acc accs = go vs (acc ++ [c]) accs

-- | Parse a script file into the necesary imports and entry
parseScript :: FilePath -> FilePath -> IO (String, String)
parseScript scriptDir scriptFile = do
    cts <- readFile $ scriptDir </> scriptFile  
    let ls = lines cts
    let scriptFileName: name: function: regexes = ls
    putStrLn $ "|- " ++ name
    return ("import " 
            ++ "Scripts" 
            ++ "."
            ++ scriptDir 
            ++ "." 
            ++ scriptFileName
            , 
               "Script " 
            ++ show name 
            ++ " "
            ++ "Scripts." ++ scriptDir ++ "." ++ scriptFileName ++ "." ++ function 
            ++ " " 
            ++ show regexes 
            )

-- | Join the imports and entries into the final source file.
joinList :: [(String, String)] -> String
joinList scripts = contents
    where 
        (imports,entries) = unzip scripts
        
        fix [] = ""
        fix [e] = "              " ++ e ++ "\n"
        fix (e:es) = fix es ++ "            , " ++ e ++ "\n"
        
        contents = "{-|\n"
                ++ "Module      : ScriptsList\n"
                ++ "Description : The list of scripts that can be used\n"
                ++ "This file is generated, there is no use in modifying it directly\n"
                ++ "Please just make sure the .ivi files are in order.\n"
                ++ "-}\n"
                ++ "module Scripts.ScriptsList where\n"
                ++ "import Script\n"
                ++ unlines imports
                ++ "\n"
                ++ "-- | The list of scripts"
                ++ "scripts :: [IVIScript] \n"
                ++ "scripts = [\n"
                ++ fix entries
                ++ "          ]\n"

