module IOCmd (getFiles) where

import System.Directory (listDirectory, doesDirectoryExist)
import IOAll
import Utils

getFiles = do
    putStr "Enter file or folder name: "
    filename <- getLine
    if filename == ""
    then return []
    else do
        dirExists <- doesDirectoryExist filename
        files <- getFiles
        if dirExists
        then addContents filename files
        else return $ filename:files
