import Control.Monad (forever)
import IOCmd
import System.Directory (doesPathExist)
import System.IO
import TodoData
import Utils

readTasks = do
    files <- getFiles
    sequence . map readTaskFile $ files
      where
        readTaskFile filename = do
            fileExists <- doesFileExist filename
            h <- openFile filename if file then ReadMode else ReadWriteMode
            cont <- hGetContents h
            return (filename, readTaskLines cont)

main = do
    tasks <- readTasks
    return tasks
