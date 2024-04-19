{-# LANGUAGE LambdaCase #-}

-- | Main library for the app, defines most of the function that interacts with IO.
module Lib
  ( convertText,
  )
where

import Data.Maybe (fromJust)
import Messages (standAloneCombing, unrecognizedUnicode)
import Output (outputAbsChars)
import Parser (Warning (..), parseAbsStr)

-- | print the warning message as human readable strings
showWarn :: Warning -> String
showWarn = \case
  UnrecognizedUnicode char -> unrecognizedUnicode char
  StandaloneComb char -> standAloneCombing char

-- | Convert and output a single line
--
-- Printing the warning in the process
convertLine :: String -> IO String
convertLine line =
  let (absStr, warns) = fromJust $ parseAbsStr line
      outputLine = outputAbsChars absStr
   in do
        mapM_ (putStrLn . showWarn) warns
        return outputLine

-- | convert a text with unicode to lines without unicode
--
-- returns the output per-line
convertText :: String -> IO [String]
convertText text =
  mapM convertLine (lines text)