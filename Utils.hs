module Utils (
    startsWith, endsWith,
    hPrompt, prompt,
    removeAt,
    splitOn, takeAndDrop) where

import System.IO (Handle, hFlush, hGetLine, hPutStr, stdin, stdout)

-- |'startsWith' @str seq@ returns True iff the first n elements of str
-- are equal to and in the same order as the elements of seq.
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = helper True
  where
    helper False _ _ = False
    helper _ _ [] = True
    helper _ [] _ = False
    helper _ (l:long) (s:short) = helper (l == s) long short

-- |'endsWith' @str seq@ returns True iff the last n elements of str
-- are equal to and in the same order as the elements of seq.
endsWith :: Eq a => [a] -> [a] -> Bool
endsWith long = startsWith (reverse long) . reverse

-- |'hPrompt' @hIn hOut s@ writes s to hOut then reads a line from hIn.
hPrompt :: Handle -> Handle -> String -> IO String
hPrompt hIn hOut p = hPutStr hOut p >> hFlush hOut >> hGetLine hIn

-- |'prompt' writes a string to 'stdout' then reads a line from stdout
-- (same as 'hPrompt' stdout)
prompt :: String -> IO String
prompt = hPrompt stdin stdout

-- |'removeAt' n arr will remove the nth element of arr if @n > length arr@.
-- Otherwise, removeAt will just walk through arr.
removeAt :: Integral a => a -> [b] -> [b]
removeAt _ [] = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x:removeAt (n - 1) xs

-- |'splitOn' @es e@ will split es at each occurrence of e.
splitOn :: Eq a => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn (s:str) chr = if s == chr then []:rem else (s:first):rest
  where rem@(first:rest) = splitOn str chr

-- |'takeAndDrop' @n xs@ returns a tuple @(start, end)@ where start is the
-- equivalent to @take n xs@ and end is equivalent to @drop n xs@.
takeAndDrop :: Int -> [a] -> ([a], [a])
takeAndDrop _ [] = ([], [])
takeAndDrop 0 xs = ([], xs)
takeAndDrop n (x:xs) = (x:start, end)
  where
    (start, end) = takeAndDrop (n - 1) xs
