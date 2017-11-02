module Utils (
    startsWith, endsWith,
    hPrompt, prompt, removeAt,
    repFunc
) where

import ListUtils
import IOUtils

repFunc :: Integral a => a -> (b -> b) -> (b -> b)
repFunc 0 _ = id
repFunc x f = f . repFunc (x - 1) f
