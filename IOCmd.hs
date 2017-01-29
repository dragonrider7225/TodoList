module IOCmd (getFiles) where
import System.Directory (listDirectory)
import IOAll
import Utils

getFiles = do
    putStr "Enter file or folder name: "
    filename <- getLine
    if filename == ""
    then return []
    else isDirectory filename >>= \isDir -> getFiles >>= if isDir then addContents filename else return . (filename:)
