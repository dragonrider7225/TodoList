module Utils (removeAt, split, endsWith, startsWith) where

removeAt _ [] = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x : removeAt (n - 1) xs

split _ [] = []
split chr (s:str) = if s == chr then []:(split chr str) else (s:first):rest
  where (first:rest) = split chr str

endsWith str = startsWith (reverse str) . reverse

startsWith [] _ = True
startsWith _ [] = False
startsWith (s:str) (c:chars) = s == c && startsWith str chars
