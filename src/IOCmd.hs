module IODiscrete (displayTasks, getFiles, getNewTask, initIO, isGui) where

import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map

import Numeric.Natural

import System.Directory (doesDirectoryExist, listDirectory)
import System.IO
import System.IO.Error

import TodoData (Days(..), Datetime, Task(..), mkDate, mkDatetime, noDays)

import Utils

-- |'displayTasks' @tasks@ displays the passed tasks in the appropriate format.
-- TODO: implement
displayTasks :: Map FilePath [Task] -> IO ()
displayTasks = const $ return ()

-- |'initIO' is unnecessary for command line, since I/O is provided by the
-- system.
initIO :: IO ()
initIO = return ()

-- |'isGui' describes whether the configuration provides GUI capability.
isGui :: Bool
isGui = False

-- |'expandDir' converts a single file path to a list of all non-directory
-- descendents.
expandDir :: FilePath -> IO [FilePath]
expandDir fp = do
    de <- doesDirectoryExist fp
    if de
    then do
        conts <- listDirectory fp >>= mapM expandDir
        return . map ((fp ++) . ('/':)) $ concat conts
    else return [fp]

-- |'getFiles' provides a list of files to read as task files.
getFiles :: IO [FilePath]
getFiles = do
    filename <- prompt "Enter file or folder name: "
    if filename == ""
    then return []
    else do
        rest <- getFiles
        first <- expandDir filename
        return $ first ++ rest

-- |'getNewTask' provides a new task to be added.
getNewTask :: IO (FilePath, Task)
getNewTask = do
    name <- prompt "Enter task name: "
    maxRep <- promptNatural "Enter number of repeats (0 to repeat forever): "
    repNum <- (if maxRep > 0
               then promptNatural "Enter current repeat: "
               else return 0)
    reps <- (if maxRep /= 1
             then (prompt "Enter days to repeat (sun mon tue wed thu fri sat): "
                    >>= return . readRepDays)
             else return noDays)
    fp <- defaultTo (prompt "Enter file to save task to (default \"tasks.lst\"): ") "tasks.lst"
    dt <- (do
        dateStr <- prompt "Enter date (yyyymmdd): "
        timeStr <- defaultTo (prompt "Enter time (hhmm, default 0000): ") "0000"
        return $ read (dateStr ++ timeStr))
    return $ (fp, Task name maxRep repNum reps dt)
      where
        defaultTo f s = f >>= (\v -> return $ if v == "" then s else v)
        days = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]
        readRepDays s = let [su, mo, tu, we, th, fr, sa] = checkConts days . map toLower $ s
                        in Days su mo tu we th fr sa
          where
            checkConts :: [String] -> String -> [Bool]
            checkConts [] str = []
            checkConts (day:rest) str =
                let sfx = stripPrefix day str
                in case sfx of
                    Nothing -> False:(checkConts rest str)
                    Just tl -> True:(checkConts rest $ drop 1 tl)
        promptNatural p = prompt p >>= return . read :: IO Natural
