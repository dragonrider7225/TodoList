module IOAll (addContents, readTaskFile, readTasks, writeTasks) where
import System.Directory (listDirectory)
import System.IO.Error
import TodoData (readTaskLines)
import Utils

addContents :: String -> [String] -> IO [String]
addContents dir filenames = do
    newNames <- listDirectory dir
    return $ filenames ++ (filter (endsWith ".lst") newNames)

readTaskFile :: FilePath -> IO (FilePath, [Task])
readTaskFile filename = do
    fileExists <- doesFileExist filename
    h <- openFile filename $ if fileExists then ReadMode else ReadWriteMode
    cont <- hGetContents h
    return (filename, readTaskLines cont)

readTasks :: IO [(FilePath, [Task])]
readTasks = do
    files <- getFiles
    sequence . map readTaskFile $ files

writeTasks :: [(FilePath, [Task])] -> IO ()
writeTasks = mapM_ (\(fp, taskObjs) -> openFile fp WriteMode >>= flip hPutStr (showTaskLines taskObjs))
