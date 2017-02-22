module IODiscrete (getFiles, getNewTask) where
import Data.Char (toLower)
import Data.List (stripPrefix)
import Numeric.Natural
import System.Directory (doesDirectoryExist)
import System.IO
import System.IO.Error
import TodoData (Days(..), Task(..), noDays)
import Utils

getFiles :: IO [FilePath]
getFiles = putStr "TODO: Implement getFiles in GUI mode" {-do
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

getNewTask :: IO (FilePath, Task)
getNewTask = putStr "TODO: Implement getNewTask for GUI mode" {-do
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
