module IOAll (addNewTask, readTaskFile, readTasks, writeTasks) where

import Control.Monad (filterM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import GHC.Conc (pseq)

import IODiscrete

import System.Directory (
    doesDirectoryExist, doesFileExist, listDirectory, removeFile, renameFile)
import System.IO
import System.IO.Error

import TodoData (Task(..), readTaskLines, showTaskLines, mkDate)

import Utils

-- |A value of type 'TFVer' describes the version of a task file.
data TFVer = V0_1 | Undefined

-- |'addNewTask' @m@ retrieves a new task and inserts it into the map m.
addNewTask :: Map FilePath [Task] -> IO (Map FilePath [Task])
addNewTask tasks = do
    (fp, task) <- getNewTask
    return $ Map.insertWith ((:) . head) fp [task] tasks

-- |'convert0_0To0_1' converts the original written version of the task files,
-- which lacked a date field, to the next version.
convert0_0To0_1 :: FilePath -> IO ()
convert0_0To0_1 fp = do
    dt <- getDate
    cont <- readFile fp
    writeFile (fp ++ "~") $ updateTasks dt cont
    renameFile (fp ++ "~") fp
      where
        getDate :: IO String
        getDate = do
            (y, m, d) <- getCurrentTime >>= return . toGregorian . utctDay
            let yN = fromIntegral y
                mN = fromIntegral m
                dN = fromIntegral d
            return . show $ mkDate yN mN dN
        updateTasks :: String -> String -> String
        updateTasks dt = unlines . ("v0.1":) . map insertDtLine . lines
          where
            fitDatetime :: [String] -> [String]
            fitDatetime [] = []
            fitDatetime (x:xs) = x:dt:xs
            insertDtLine :: String -> String
            insertDtLine = unwords . reverse . fitDatetime . reverse . words

-- |'expandDirs' @fp@ gets all files with names that end in ".lst" and are
-- contained in the directory fp, through any number of layers of directories.
expandDirs :: FilePath -> IO [FilePath]
expandDirs dir = do
    names <- listDirectory dir
    dirs <- filterM doesDirectoryExist names
    ((if dirs == [] then return
      else sequence . (map expandDirs dirs ++) . map return)
     [filter (endsWith ".lst") names]) >>= return . concat

-- |'getTFVer' @fp@ gets and returns the version of the task file. If the
-- given file contains no version information, then 'Undefined' is returned
-- instead.
getTFVer :: FilePath -> IO TFVer
getTFVer fp = openFile fp ReadMode >>= hGetLine >>= return . lineToVer
  where
    lineToVer :: String -> TFVer
    lineToVer str = if str == "v0.1" then V0_1 else Undefined

-- |'isTaskFile' @fp@ is True iff fp is at least four characters long and its
-- last four characters are ".lst".
isTaskFile :: FilePath -> Bool
isTaskFile fp = endsWith fp ".lst"

-- |'readTaskFile' @fp@ reads the file located by the path fp as list of tasks,
-- with one task on each line.
readTaskFile :: FilePath -> IO (FilePath, [Task])
readTaskFile fp = do
    fileExists <- doesFileExist fp
    if fileExists then return () else writeFile fp "\n"
    cont <- withFile fp ReadMode hGetLine
    tl <- if cont == "v0.1"
          then readFile fp >>= return . readTaskLines
          else convert0_0To0_1 fp >> readTaskFile fp >>= return . snd
    return (fp, tl)

-- |'readTasks' gets a list of files to read and reads them into a map from
-- filename to lists of tasks.
readTasks :: IO (Map FilePath [Task])
readTasks = getFiles >>= mapM readTaskFile . filter isTaskFile >>= return . Map.fromList

-- |'writeTasks' @m@ writes the tasks from the map m into the associated files.
writeTasks :: Map FilePath [Task] -> IO ()
writeTasks = mapM_ (\(fp, taskObjs) -> withFile fp WriteMode . flip hPutStr $
                         showTaskLines taskObjs) . Map.assocs
