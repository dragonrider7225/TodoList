readTasks = do
    files <- getFiles
    taskStrs <- sequence . map readFile $ files

main = do
    tasks <- readTasks
