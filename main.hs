import IOCmd

readTasks = do
    files <- getFiles
    taskStrs <- sequence . map readFile $ files
    return taskStrs -- TODO: read Task data from taskStrs

main = do
    tasks <- readTasks
    return tasks
