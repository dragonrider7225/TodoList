import Control.Monad (forever)

import Data.Map (fromList)

import IOAll

import IODiscrete

import System.IO

-- |The entry point for the program.
main :: IO ()
main = do
    initIO
    tasks <- readTasks
    if tasks == fromList []
    then optExit >>= (\b -> if b then return () else main)
    else print tasks >> main

-- |'optExit' provides a quit prompt to the command line.
optExit :: IO Bool
optExit = do
    response <- prompt "[q]uit? "
    return $ if response == "q" then True else False
