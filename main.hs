import Control.Monad (forever)
import IOAll
import System.IO

main = forever $ readTasks >>= print
