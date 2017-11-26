module Utils (
    startsWith, endsWith,
    hPrompt, prompt, removeAt,
    repFunc
) where

import ListUtils
import IOUtils

-- |'repFunc' @n f@ builds a function that applies @f@ @n@ times.
repFunc :: Integral a => a -> (b -> b) -> (b -> b)
repFunc 0 _ = id
repFunc n f = f . repFunc (n - 1) f
