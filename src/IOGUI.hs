module IODiscrete (displayTasks, getFiles, getNewTask, initIO, isGui) where

import Data.ByteString.Char8 (pack)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Numeric.Natural

import System.Directory (doesDirectoryExist)
import System.IO
import System.IO.Error

import TodoData (Days(..), Task(..), noDays)

import Utils

-- |'displayTasks' @tasks@ displays the passed tasks in the appropriate format.
displayTasks :: Map FilePath [Task] -> UI ()
displayTasks tasks = do
    w <- askWindow
    return ()

-- |'initIO' initializes the I/O system.
initIO :: IO ()
initIO = startGUI defaultConfig { jsPort = Just 8023
                                , jsStatic = Just "assets"
                                , jsAddr = Just $ pack "0.0.0.0"
                                } setup

-- |'isGui' describes whether the configuration provides GUI capability.
isGui :: Bool
isGui = True

-- |'setup' builds the HTML tree.
setup :: Window -> UI ()
setup w = do
    return w # set UI.title "Todo List"
    taskButton <- set UI.text "Add task" UI.button
    fileButton <- set UI.text "Add task file" UI.button
    getBody w #+ [element taskButton, set (UI.attr "id") "tasks" UI.div, element taskButton]
    on UI.click taskButton $ \_ -> liftIO getNewTask >>= (\(f, t) -> insertTask f t)

-- |'insertTask' @fp t@ inserts the given task into the task list, associated
-- with the given task file.
-- TODO: implement
insertTask :: FilePath -> Task -> UI ()
insertTask fp t = do
    w <- askWindow
    Just tasks <- getElementById w "tasks"
    return ()

-- |'getFiles' provides a list of files to read as task files.
-- TODO: implement
getFiles :: UI [FilePath]
getFiles = liftIO (putStr "TODO: Implement getFiles in GUI mode") >> return []
{-do
    putStr "Enter file or folder name: "
    filename <- getLine
    if filename == ""
    then return []
    else do
        dirExists <- doesDirectoryExist filename
        files <- getFiles
        return $ (if dirExists
                  then (filename ++ "/")
                  else filename):files -}

-- |'getNewTask' provides a new task to be added.
-- TODO: implement
getNewTask :: UI (FilePath, Task)
getNewTask = do
    putStr "TODO: Implement getNewTask for GUI mode"
    return undefined
{-do
    name <- prompt "Enter task name: "
    maxRep <- promptNatural "Enter number of repeats (0 to repeat forever): "
    repNum <- (if maxRep > 0
               then promptNatural "Enter current repeat: "
               else return 0)
    reps <- (if maxRep /= 1
             then (prompt "Enter days to repeat (sun mon tue wed thu fri sat): "
                    >>= return . readRepDays)
             else return noDays)
    fp <- (prompt "Enter file to save task to (default \"tasks.lst\"): " >>=
            (\fp -> return $ if fp == "" then "tasks.lst" else fp))
    return $ (fp, Task name maxRep repNum reps)
      where
        prompt :: String -> IO String
        prompt p = putStr p >> getLine
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
        promptNatural p = prompt p >>= return . read :: IO Natural -}
