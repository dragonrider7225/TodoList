module TodoData (Days(..), Task(..), noDays, allDays, addSun, addMon,
                 addTue, addWed, addThu, addFri, addSat, readTaskLines,
                 showTaskLines) where
import qualified Data.Text as DT
import Numeric.Natural

{- Testing values
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

noDays :: Days
noDays = Days False False False False False False False

allDays :: Days
allDays = Days True True True True True True True

addSun :: Days -> Days
addSun days = Days True (mon days) (tue days) (wed days) (thu days) (fri days) (sat days)

addMon :: Days -> Days
addMon days = Days (sun days) True (tue days) (wed days) (thu days) (fri days) (sat days)

addTue :: Days -> Days
addTue days = Days (sun days) (mon days) True (wed days) (thu days) (fri days) (sat days)

addWed :: Days -> Days
addWed days = Days (sun days) (mon days) (tue days) True (thu days) (fri days) (sat days)

addThu :: Days -> Days
addThu days = Days (sun days) (mon days) (tue days) (wed days) True (fri days) (sat days)

addFri :: Days -> Days
addFri days = Days (sun days) (mon days) (tue days) (wed days) (thu days) True (sat days)

addSat :: Days -> Days
addSat days = Days (sun days) (mon days) (tue days) (wed days) (thu days) (fri days) True

instance Show Days where
    show d = map (head . show . fromEnum . ($ d)) [sun,mon,tue,wed,thu,fri,sat]

instance Read Days where
    readsPrec _ str = [(result, rest)]
      where
        su:mo:tu:we:th:fr:sa:[] = map (toEnum . read . (:[]) :: Char -> Bool) $ take 7 str
        result = Days su mo tu we th fr sa
        rest = drop 7 str

data Task = Task { name :: String
                 , maxRep :: Natural
                 , repNum :: Natural
                 , reps :: Days
                 }

instance Show Task where
    show t = (name t) ++ " (" ++ (show $ maxRep t) ++ ") " ++ (show $ repNum t) ++ (' ':(show $ reps t))

instance Read Task where
    readsPrec _ str = [(Task name (read maxRep :: Natural) (read rep :: Natural) (read reps :: Days), afterReps)]
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
readTaskLines "" = []
readTaskLines "\n" = []
readTaskLines text = map (read . DT.unpack) . DT.splitOn (DT.pack "\n") . DT.pack $ text
