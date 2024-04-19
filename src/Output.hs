{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module produces the output string from a list of abstract chars
module Output (outputAbsChars) where

import Fonts (Font, convertFont)
import qualified Parser as C
import Symbols (convertComb, convertSymbol)

-- | We group the abstract char of the same type together,
-- so that it is easier to convert
data MergedToken
  = Preserved String
  | UniFont Font String
  | UniSymbol Char
  | WithComb Char C.AbsChar

-- | group neighboring abstract char with the same type into a string.
merge :: [C.AbsChar] -> [MergedToken]
merge absStr = case absStr of
  [] -> []
  C.Preserved c : rest ->
    case merge rest of
      Preserved s : restRes -> Preserved (c : s) : restRes
      restRes -> Preserved [c] : restRes
  C.UniFont font c : rest ->
    case merge rest of
      UniFont font' s : restRes | font == font' -> UniFont font' (c : s) : restRes
      restRes -> UniFont font [c] : restRes
  (C.UniSymbol c) : rest -> UniSymbol c : merge rest
  (C.WithComb comb absChar) : rest -> WithComb comb absChar : merge rest

-- | output a abstract char `C.AbsChar` as a string
outputAbsChar :: C.AbsChar -> String
outputAbsChar absChar = case absChar of
  C.Preserved c -> [c]
  C.UniFont font c -> convertFont font [c]
  C.UniSymbol c -> convertSymbol c
  C.WithComb comb innerAbsChar ->
    convertComb comb $ outputAbsChar innerAbsChar

-- | output a merged token `MergedToken` as string
outputToken :: MergedToken -> String
outputToken = \case
  Preserved s -> s
  UniFont font s -> convertFont font s
  UniSymbol c -> convertSymbol c
  WithComb comb innerAbsChar ->
    convertComb comb $ outputAbsChar innerAbsChar

-- | output a list of of merged token
outputTokens :: [MergedToken] -> String
outputTokens = concatMap outputToken

-- | output a list of abstract char
--
-- Note that it is crucial for us to merge first and then output
-- for example, `A₁₂` will be outputted as `A_{12}`;
-- but if we directly output all the abstract char without merging, 
-- we will have `A_{1}_{2}`.
outputAbsChars :: [C.AbsChar] -> String
outputAbsChars = outputTokens . merge