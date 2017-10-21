module IOUtils (
    hPrompt, prompt
) where

import GHC.Conc (pseq)

import System.IO (Handle, hFlush, hGetLine, hPutStr, stdin, stdout)

-- |'hPrompt' @hIn hOut s@ writes s to hOut then reads a line from hIn.
hPrompt :: Handle -> Handle -> String -> IO String
hPrompt hIn hOut p = hPutStr hOut p >> hFlush hOut >> hGetLine hIn

-- |'prompt' writes a string to 'stdout' then reads a line from stdout
-- (same as 'hPrompt' stdout)
prompt :: String -> IO String
prompt = hPrompt stdin stdout
