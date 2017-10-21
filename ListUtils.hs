module ListUtils (
    startsWith, endsWith, removeAt
) where

import Data.List (isPrefixOf, isSuffixOf)

-- |'startsWith' @str seq@ returns True iff the first n elements of str
-- are equal to and in the same order as the elements of seq.
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = flip isPrefixOf

-- |'endsWith' @str seq@ returns True iff the last n elements of str
-- are equal to and in the same order as the elements of seq.
endsWith :: Eq a => [a] -> [a] -> Bool
endsWith = flip isSuffixOf

-- |'removeAt' n arr will remove the nth element of arr if @n > length arr@.
-- Otherwise, removeAt will just walk through arr.
removeAt :: Integral a => a -> [b] -> [b]
removeAt _ [] = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x:(removeAt (n - 1) xs)
