module TodoData (Days(..), noDays, allDays, addSun, addMon, addTue, addWed,
                 addThu, addFri, addSat,
                 Datetime, mkDate, mkDatetime,
                 Task(..), readTaskLines, showTaskLines) where

import Numeric.Natural

import Utils

default(Natural)

{- Testing values
t = Task "Test Task" 120 1 $ Days True True True True True False False
---}

-- |A 'Days' object describes the weekly repetition of a task.
data Days = Days { sun :: Bool
                 , mon :: Bool
                 , tue :: Bool
                 , wed :: Bool
                 , thu :: Bool
                 , fri :: Bool
                 , sat :: Bool
                 } deriving (Eq)

-- |'noDays' describes a non-repeating task.
noDays :: Days
noDays = Days False False False False False False False

-- |'allDays' describes a task that repeats every day.
allDays :: Days
allDays = Days True True True True True True True

-- |'addSun' sets the repetition on Sunday.
addSun :: Days -> Days
addSun days = Days True (mon days) (tue days) (wed days) (thu days) (fri days) (sat days)

-- |'addMon' sets the repetition on Monday.
addMon :: Days -> Days
addMon days = Days (sun days) True (tue days) (wed days) (thu days) (fri days) (sat days)

-- |'addTue' sets the repetition on Tuesday.
addTue :: Days -> Days
addTue days = Days (sun days) (mon days) True (wed days) (thu days) (fri days) (sat days)

-- |'addWed' sets the repetition on Wednesday.
addWed :: Days -> Days
addWed days = Days (sun days) (mon days) (tue days) True (thu days) (fri days) (sat days)

-- |'addThu' sets the repetition on Thursday.
addThu :: Days -> Days
addThu days = Days (sun days) (mon days) (tue days) (wed days) True (fri days) (sat days)

-- |'addFri' sets the repetition on Friday.
addFri :: Days -> Days
addFri days = Days (sun days) (mon days) (tue days) (wed days) (thu days) True (sat days)

-- |'addSat' sets the repetition on Saturday.
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

-- |A 'Datetime' object describes the datetime at which a task occurs.
data Datetime = Datetime { year :: Natural
                         , month :: Natural
                         , day :: Natural
                         , hour :: Natural
                         , minute :: Natural
                         } deriving (Eq)

-- |'mkDatetime' normalizes the values passed to it and converts them into a
-- 'Datetime' object.
mkDatetime :: Natural -> Natural -> Natural -> Natural -> Natural -> Datetime
mkDatetime yr mnt dy hr min = if min >= 60
                              then fixMin
                              else if hr >= 24
                                   then fixHr
                                   else if mnt > 12
                                        then fixMnt
                                        else if dy > getDays yr mnt
                                             then fixDay
                                             else genDatetime
  where
    monthNums = cycle [31, 30] :: [Natural]
    isLeapyear yr = mod yr 400 == 0 || mod yr 100 /= 0 && mod yr 4 == 0
    getDays yr mnt = cycle (31:(if isLeapyear yr then 29 else 28):(take 5 monthNums ++ take 5 monthNums)) !! (fromIntegral mnt)
    fixMin = mkDatetime yr mnt dy (hr + div min 60) (mod min 60)
    fixHr = mkDatetime yr mnt (dy + div hr 24) (mod hr 24) min
    fixDay = mkDatetime yr (mnt + 1) (dy - getDays yr mnt) hr min
    fixMnt = mkDatetime (yr + 1) (mnt - 12) dy hr min
    genDatetime = Datetime yr mnt dy hr min

-- |'mkDate' @yr mnt day@ provides an equal 'Datetime' object to
-- 'mkDatetime' @yr mnt day 0 0@.
mkDate :: Natural -> Natural -> Natural -> Datetime
mkDate = curry . curry $ (flip . (flip . uncurry . uncurry) mkDatetime) 0 0

instance Show Datetime where
    show = concat . map (leftPad . show) . zipWith ($) fields . repeat
      where
        fields = [year, month, day, hour, minute]
        leftPad n = if length n > 1 then n else '0':n

instance Read Datetime where
    readsPrec _ str = [(result, rest)]
      where
        fixFst (start, rest) = (read $ reverse start :: Natural, rest)
        (dt, rest) = span (/= ' ') str
        (min, afterMin) = fixFst $ splitAt 2 dt
        (hr, afterHour) = fixFst $ splitAt 2 afterMin
        (day, afterDay) = fixFst $ splitAt 2 afterHour
        (mnt, yr) = fixFst $ splitAt 2 afterDay
        result = mkDatetime (read $ reverse yr) mnt day hr min

-- |A 'Task' object describes an (optionally repeating) item on the todo list.
data Task = Task { name :: String
                 , maxRep :: Natural
                 , repNum :: Natural
                 , reps :: Days
                 , date :: Datetime
                 } deriving (Eq)

instance Show Task where
    show t = (name t) ++ " (" ++ (show $ maxRep t) ++ ") " ++ (show $ repNum t) ++ (' ':(show $ date t)) ++ (' ':(show $ reps t))

instance Read Task where
    readsPrec _ str = [(Task name maxRep rep reps datetime, afterReps)]
      where
        readFirst :: Read a => (String, b) -> (a, b)
        readFirst (a, b) = (read a, b)
        tailSecond :: (a, [b]) -> (a, [b])
        tailSecond (a, b) = (a, tail b)
        readAndTail :: (Read a, Integral n) => n -> (String, [b]) -> (a, [b])
        readAndTail n = readFirst . repFunc n tailSecond
        readName :: String -> (String, String)
        readName [] = ([], [])
        readName [x] = ([x], [])
        readName (x:y:xs) = if y == '(' && x /= '\\' then ([], xs) else (x:result, rest)
          where (result, rest) = readName (y:xs)
        readMax :: String -> (Natural, String)
        readMax = readAndTail 2 . span (/= ')')
        readRep :: String -> (Natural, String)
        readRep = readAndTail 1 . span (/= ' ')
        readDatetime :: String -> (Datetime, String)
        readDatetime = readAndTail 1 . span (/= ' ')
        readReps :: String -> (Days, String)
        readReps = readFirst . splitAt 7
        (name, afterName) = readName str
        (maxRep, afterMax) = readMax afterName
        (rep, afterRep) = readRep afterMax
        (datetime, afterDatetime) = readDatetime afterRep
        (reps, afterReps) = readReps afterDatetime

-- |'showTaskLines' converts a list of tasks to the string representation that
-- will be written to the task file.
showTaskLines :: [Task] -> String
showTaskLines [] = "\n"
showTaskLines tasks = foldl1 ((++) . (++"\n")) $ map show tasks

-- |'readTaskLines' converts a task file string into a list of tasks.
readTaskLines :: String -> [Task]
readTaskLines "" = []
readTaskLines "\n" = []
readTaskLines ('v':'0':'.':'1':'\n':rest) = map read . lines $ rest
readTaskLines text = error "Can't read tasks from list file."
