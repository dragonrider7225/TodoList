module Utils (endsWith, prompt, removeAt, split, startsWith) where

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith str = startsWith (reverse str) . reverse

prompt :: String -> IO String
prompt p = putStr p >> getLine

removeAt :: Integral a => a -> [b] -> [b]
removeAt _ [] = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x : removeAt (n - 1) xs

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split chr (s:str) = if s == chr then []:rem else (s:first):rest
  where rem@(first:rest) = split chr str

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (s:str) (c:chars) = s == c && startsWith str chars
