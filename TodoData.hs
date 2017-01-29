module TodoData (Task, readTaskLines, showTaskLines) where
import qualified Data.Text as DT

--{- Testing values
t = Task "Test Task" 120 1 $ Days True True True True True False False
---}

data Days = Days { sun :: Bool
                 , mon :: Bool
                 , tue :: Bool
                 , wed :: Bool
                 , thu :: Bool
                 , fri :: Bool
                 , sat :: Bool
                 }

instance Show Days where
    show d = map (head . show . fromEnum . ($ d)) [sun,mon,tue,wed,thu,fri,sat]

instance Read Days where
    readsPrec _ str = [(result, rest)]
      where
        su:mo:tu:we:th:fr:sa:[] = map (toEnum . read . (:[]) :: Char -> Bool) $ take 7 str
        result = Days su mo tu we th fr sa
        rest = drop 7 str

data Task = Task { name :: String
                 , maxRep :: Int
                 , repNum :: Int
                 , reps :: Days
                 }

instance Show Task where
    show t = (name t) ++ " (" ++ (show $ maxRep t) ++ ") " ++ (show $ repNum t) ++ (' ':(show $ reps t))

instance Read Task where
    readsPrec _ str = [(Task name (read maxRep :: Int) (read rep :: Int) (read reps :: Days), afterReps)]
      where
        readName [] = ([], [])
        readName [x] = ([x], [])
        readName (x:(y:xs)) = if y == '(' && x /= '\\' then ([], xs) else (x:result, rest)
          where (result, rest) = readName (y:xs)
        readMax [] = ([], [])
        readMax [x] = if x == ')' then ([], []) else ([x], [])
        readMax (x:(y:xs)) = if x == ')' then ([],xs) else (x:result, rest)
          where (result, rest) = readMax (y:xs)
        readRep [] = ([], [])
        readRep [x] = ([x], [])
        readRep (x:(y:xs)) = if y == ' ' then ([x],xs) else (x:result,rest)
          where (result, rest) = readRep (y:xs)
        readReps xs = (take 7 xs, drop 7 xs)
        (name, afterName) = readName str
        (maxRep, afterMax) = readMax afterName
        (rep, afterRep) = readRep afterMax
        (reps, afterReps) = readReps afterRep

showTaskLines :: [Task] -> String
showTaskLines [] = "\n"
showTaskLines tasks = foldl1 ((++) . (++"\n")) $ map show tasks

readTaskLines :: String -> [Task]
readTaskLines = map (read . DT.unpack) . DT.splitOn (DT.pack "\n") . DT.pack
