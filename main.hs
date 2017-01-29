import Control.Monad (forever)
import IOCmd
import TodoData
import Utils

readTasks = do
    files <- getFiles
    sequence . map readTaskFile $ files
      where
        readTaskFile filename = do
            cont <- readFile filename
            return (filename, readTaskLines cont)

main = do
    tasks <- readTasks
    return tasks
