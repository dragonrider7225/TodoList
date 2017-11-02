import Control.Monad (forever)

import Data.Map (fromList)

import IOAll

import IODiscrete

import System.IO

main :: IO ()
main = do
    initIO
    tasks <- readTasks
    if tasks == fromList []
    then optExit >>= (\b -> if b then return () else main)
    else print tasks >> main

optExit :: IO Bool
optExit = do
    putStr "[q]uit? "
    response <- getLine
    return $ if response == "q" then True else False
