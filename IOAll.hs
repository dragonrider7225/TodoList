module IOAll (readTaskFile, readTasks, writeTasks) where

import Control.Monad (filterM)

import Data.Map (Map)
import qualified Data.Map as Map

import IODiscrete

import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.IO
import System.IO.Error

import TodoData (Task(..), readTaskLines, showTaskLines)

import Utils

-- |'addNewTask' @m@ retrieves a new and inserts it into the map m.
addNewTask :: Map FilePath [Task] -> IO (Map FilePath [Task])
addNewTask tasks = do
    (fp, task) <- getNewTask
    return $ Map.insertWith ((:) . head) fp [task] tasks

expandDirs :: FilePath -> IO [FilePath]
expandDirs dir = do
    names <- listDirectory dir
    dirs <- filterM doesDirectoryExist names
    ((if dirs == [] then return
      else sequence . (map expandDirs dirs ++) . map return)
     [filter (endsWith ".lst") names]) >>= return . concat

readTaskFile :: FilePath -> IO (FilePath, [Task])
readTaskFile filename = do
    fileExists <- doesFileExist filename
    h <- openFile filename $ if fileExists then ReadMode else ReadWriteMode
    cont <- hGetContents h
    return (filename, readTaskLines cont)

readTasks :: IO (Map FilePath [Task])
readTasks = getFiles >>= sequence . map readTaskFile >>= return . Map.fromList

writeTasks :: Map FilePath [Task] -> IO ()
writeTasks = mapM_ (\(fp, taskObjs) -> withFile fp WriteMode . flip hPutStr $
                         showTaskLines taskObjs) . Map.assocs
