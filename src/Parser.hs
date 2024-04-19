{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (parseAbsStr, AbsChar(..), Warning(..)) where

import Control.Applicative (Alternative (many, some, (<|>)))
import Control.Monad (MonadPlus, guard)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (MonadState (get, put), StateT (StateT, runStateT))
import Control.Monad.Writer (MonadWriter (tell), WriterT (WriterT, runWriterT))
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Fonts
  ( Font (..),
    bbMap,
    boldMap,
    calMap,
    frakMap,
    italicMap,
    scrMap,
    sfMap,
    subMap,
    supMap,
    ttMap,
  )
import Symbols (combMap, symbolMap)

-- | An abstract representation of a char
--
-- This is the structured version of a char.
-- everything will keep its original unicode symbol
-- and will be converted in the next step
data AbsChar
  = -- | a character that will be preserved for this transformation.
    -- these includes ascii character, unrecognized unicode character
    -- and standalone combining character.
    Preserved Char
  | -- | a simple unicode symbol
    UniSymbol Char
  | -- | character with a font (like bold) represented as unicode
    UniFont Font Char
  | -- | unicode combination character with a abstract char
    WithComb Char AbsChar
  deriving (Show, Eq)

data Warning
  = -- | unicode that is not in the dictionary
    UnrecognizedUnicode Char
  | -- | a combining character without a previous character
    StandaloneComb Char
  deriving (Show)

-- | The parser type.
--
-- This parser is not expected to produce any error, will only write warnings
-- The `Maybe` is only here for for `Alternative`,
-- but we can handle error in the future if we want to.
--
-- We intentionally make the constructor of Parser unreadable,
-- since people should not use it.
newtype Parser res = P {runP :: WriterT (Seq.Seq Warning) (StateT String Maybe) res}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadPlus,
      MonadState String,
      MonadWriter (Seq.Seq Warning),
      MonadError ()
    )

-- | run the parser
runParser :: Parser res -> String -> Maybe ((res, Seq.Seq Warning), String)
runParser = runStateT . runWriterT . runP

-- | parse a string from start to end
parseWith :: Parser res -> String -> Maybe (res, Seq Warning)
parseWith parser str = case runParser parser str of
  Nothing -> Nothing
  Just (res, []) -> Just res
  Just (_, _) -> Nothing

-- | type of unicode used in the text
--
-- Note that this doesn't include combination characters
data UnicodeType = Symbol | Font Font

-- | maps all the known unicode to their type
--
-- Note that this map doesn't include combining characters
unicodeTypeMap :: Map.Map Char UnicodeType
unicodeTypeMap =
  (Symbol <$ symbolMap)
    `Map.union` (Font Sup <$ supMap)
    `Map.union` (Font Sub <$ subMap)
    `Map.union` (Font Bold <$ boldMap)
    `Map.union` (Font Italic <$ italicMap)
    `Map.union` (Font Cal <$ calMap)
    `Map.union` (Font Frak <$ frakMap)
    `Map.union` (Font BB <$ bbMap)
    `Map.union` (Font SF <$ sfMap)
    `Map.union` (Font TT <$ ttMap)
    `Map.union` (Font Scr <$ scrMap)

-- | the set of combining characters
allCombs :: Set.Set Char
allCombs = Set.fromList $ Map.keys combMap

-- | parse a single char
charParser :: Parser Char
charParser = do
  str <- get
  case str of
    [] -> throwError ()
    char : rest -> do
      put rest
      return char

-- | Parse a ascii character to abstract characters
asciiParser :: Parser AbsChar
asciiParser = do
  char <- charParser
  guard (Char.isAscii char)
  return $ Preserved char

-- | parse a single ascii or unicode char
--
-- try to parse this as a ascii first then try to parse as unicode.
--
-- if both failed see if the char is a combining character:
-- if so, this combination character doesn't have anything in the front to absorb it;
-- otherwise this character is an unrecognized unicode character.
charToAbsParser :: Parser AbsChar
charToAbsParser =
  asciiParser <|> do
    char <- charParser
    case unicodeTypeMap Map.!? char of
      Just Symbol -> return $ UniSymbol char
      Just (Font font) -> return $ UniFont font char
      Nothing ->
        if Set.member char allCombs
          then do
            tell $ Seq.singleton $ StandaloneComb char
            return $ Preserved char
          else do
            tell $ Seq.singleton $ UnrecognizedUnicode char
            return $ Preserved char

-- | get a combining character, fail if it is not a combining character
combParser :: Parser Char
combParser = do
  char <- charParser
  guard (Set.member char allCombs)
  return char

-- | Parse a abstract char with all the combining character applied on it
--
-- TODO: make it more efficient by not repeating `parseAChar` in `parseAbsChar`
withCombsParser :: Parser AbsChar
withCombsParser = do
  absChar <- charToAbsParser
  combs <- some combParser
  return $ applyCombs combs absChar
  where
    applyCombs combs absChar = foldl (flip WithComb) absChar combs

-- | parse a abstract char
absCharParser :: Parser AbsChar
absCharParser = withCombsParser <|> charToAbsParser

-- | parse a abstract string
parseAbsStr :: String -> Maybe ([AbsChar], Seq Warning)
parseAbsStr = parseWith (many absCharParser) 

-- | temporary testing case, will put in test file later
testCase :: String
testCase = "ÿ c—天σ̇"