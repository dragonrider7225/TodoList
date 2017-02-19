module IOAll (addContents, addNewTask, readTaskFile, readTasks,
              writeTasks) where
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.Natural
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.IO
import System.IO.Error
import TodoData (Days(..), Task(..), noDays, readTaskLines, showTaskLines)
import Utils

--{- In command line
getFiles :: IO [FilePath]
getFiles = do
    putStr "Enter file or folder name: "
    filename <- getLine
    if filename == ""
    then return []
    else do
        dirExists <- doesDirectoryExist filename
        files <- getFiles
        if dirExists
        then addContents filename files
        else return $ filename:files

getNewTask :: IO (FilePath, Task)
getNewTask = do
    name <- prompt "Enter task name: "
    maxRep <- prompt "Enter number of repeats (0 to repeat forever): " >>= return . read :: IO Natural
    repNum <- if maxRep > 0 then prompt "Enter current repeat: " >>= (return . read :: String -> IO Natural) else return 0
    reps <- if maxRep /= 1 then prompt "Enter days to repeat (sun mon tue wed thu fri sat): " >>= return . readRepDays else return noDays
    fp <- prompt "Enter file to save task to (default \"tasks.lst\"): " >>= (\fp -> return $ if fp == "" then "tasks.lst" else fp)
    return $ (fp, Task name maxRep repNum reps)
      where
        prompt p = putStr p >> getLine
        readRepDays s = let [su, mo, tu, we, th, fr, sa] = checkConts ["sun", "mon", "tue", "wed", "thu", "fri", "sat"] . map toLower $ s
                        in Days su mo tu we th fr sa
          where
            checkConts [] str = []
            checkConts (day:rest) str =
                let sfx = stripPrefix day str
                in case sfx of
                    Nothing -> False:(checkConts rest str)
                    Just tl -> True:(checkConts rest $ drop 1 tl)
---}

{- In GUI
getFiles :: IO [FilePath]
getFiles = return undefined

getNewTask :: IO (FilePath, Task)
getNewTask = return undefined
---}

addContents :: String -> [String] -> IO [String]
addContents dir filenames = do
    newNames <- listDirectory dir
    return $ filenames ++ (filter (endsWith ".lst") newNames)

addNewTask :: Map FilePath [Task] -> IO (Map FilePath [Task])
addNewTask tasks = do
    (fp, task) <- getNewTask
    return $ Map.insertWith ((:) . head) fp [task] tasks

readTaskFile :: FilePath -> IO (FilePath, [Task])
readTaskFile filename = do
    fileExists <- doesFileExist filename
    h <- openFile filename $ if fileExists then ReadMode else ReadWriteMode
    cont <- hGetContents h
    return (filename, readTaskLines cont)

readTasks :: IO (Map FilePath [Task])
readTasks = getFiles >>= sequence . map readTaskFile >>= return . Map.fromList


writeTasks :: Map FilePath [Task] -> IO ()
writeTasks = mapM_ (\(fp, taskObjs) -> withFile fp WriteMode . flip hPutStr $ showTaskLines taskObjs) . Map.assocs
