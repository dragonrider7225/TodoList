import Control.Monad (forever)
import IOAll
import System.Directory (doesFileExist)
import System.IO
import TodoData
import Utils

main = do
    tasks <- readTasks
    return tasks
