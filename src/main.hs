import Control.Monad (forever)

import Data.Map (fromList)

import IOAll

import System.IO

main :: IO ()
main = do
    tasks <- readTasks
    if tasks == fromList [] then return () else print tasks >> main
