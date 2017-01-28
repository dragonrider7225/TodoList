module TodoData (Task) where
import Text.ParserCombinators.Parsec

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

data Task = Task { name :: String
                 , repNum :: Int
                 , reps :: Days
                 }

instance Show Task where
    show t = (name t) ++ " (" ++ (show $ repNum t) ++ ") " ++ (show $ reps t)

taskP :: Parser Task
taskP = do
    name <- taskNameP
    num <- repNumP
    reps <- space >> daysP
    return $ Task name num reps

taskNameP :: Parser String
taskNameP = do
    nameSp <- many $ noneOf "("
    return $ init nameSp

repNumP :: Parser Int
repNumP = do
    char '('
    numStr <- many (oneOf "0123456789")
    char ')'
    return (read numStr :: Int)

-- A Days object consists of seven digits, either '0' or '1'.
daysP :: Parser Days
daysP = do
    daysStr <- sequence . take 7 . repeat $ char '0' <|> char '1'
    let [su, mo, tu, we, th, fr, sa] = map (toEnum . read . (:[]) :: Char -> Bool) daysStr
    return $ Days su mo tu we th fr sa

testParser :: String -> Either ParseError Task
testParser = parse taskP "(unknown)"
