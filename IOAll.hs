module IOAll (addContents, isDirectory) where
import System.Directory (listDirectory)
import System.IO.Error
import Utils

addContents dir filenames = do
    newNames <- listDirectory dir
    return $ filenames ++ (filter (endsWith ".lst") newNames)

isDirectory :: String -> IO Bool
isDirectory filename = do
    catchIOError (listDirectory filename >>= return . (0<) . length) $ \e -> return False
