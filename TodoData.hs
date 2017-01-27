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
    show d = map (head . show . fromEnum . ($ d)) [sat,fri,thu,wed,tue,mon,sun]

-- A Days object consists of seven digits, either '0' or '1'.
days :: Parser Days
days = do
    daysStr <- sequence . take 7 . repeat $ char '0' <|> char '1'
    let [su, mo, tu, we, th, fr, sa] = map (toEnum . read . (:[]) :: Char -> Bool) daysStr
    return $ Days su mo tu we th fr sa

data Task = Task { name :: String
                 , repNum :: Maybe Int
                 , reps :: Maybe Days
                 } deriving (Show)
