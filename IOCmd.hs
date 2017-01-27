module IOCmd (getFiles) where

getFiles = do
    putStr "Enter file or folder name: "
    filename <- getLine
    if filename == ""
    then return []
    else getFiles >>= return . (filename:)
