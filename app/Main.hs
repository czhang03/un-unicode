{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Lib (convertText)
import Messages (warningPrefix)
import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment (getArgs)

data OptInput = ReadIn | TextIn String | FileIn FilePath deriving (Show)

data OptOutput = OptOut
  { showOut :: Bool,
    -- | the second component represents whether the output file is forced
    fileOut :: Maybe (FilePath, Bool)
  }

-- | a plain parsed options
--
-- These options do not check for option conflicts
-- for example `inputFileOpt` and `inputReadOpt` cannot be specified at the same time.
data Flag
  = VerbTextOpt
  | ShowVersionOpt
  | ShowHelpOpt
  | InputFileOpt FilePath
  | InputReadOpt
  | InputTextOpt String
  | OutputFileOpt FilePath
  | OutputShowOpt
  | ForceReplaceOutFile
  deriving (Show, Eq)

data ConvertOpt = Conv {showVersion :: Bool, inpOpt :: OptInput, outOpt :: OptOutput, verbOpt :: Bool}

data Options = ShowVersion | ShowHelp | ConvertOpt ConvertOpt

options :: [OptDescr Flag]
options =
  [ Option
      []
      ["verbose", "debug"]
      (NoArg VerbTextOpt)
      "Output information for debugging purpose.",
    Option
      ['h', '?']
      ["help"]
      (NoArg ShowHelpOpt)
      "Show this help message",
    Option
      ['v']
      ["version"]
      (NoArg ShowVersionOpt)
      "Show version number",
    Option
      ['i']
      ["input"]
      ( ReqArg
          InputFileOpt
          "FILE"
      )
      "Input file name. Incompatible with other input options.",
    Option
      ['o']
      ["output"]
      ( ReqArg
          OutputFileOpt
          "FILE"
      )
      "Output file name. This will overwrite the content of the file",
    Option
      ['f']
      ["force", "replace"]
      (NoArg ForceReplaceOutFile)
      "Force replace the output file, even if it already exists",
    Option
      ['r']
      ["read"]
      (NoArg InputReadOpt)
      "Read the input form stdin. Incompatible with other input options.",
    Option
      ['t']
      ["text"]
      ( ReqArg
          InputTextOpt
          "TEXT"
      )
      "Provide the text to convert as input. Incompatible with other input options.",
    Option
      ['s']
      ["show", "print"]
      (NoArg OutputShowOpt)
      "Show the output directly in STDOUT"
  ]

showUsageInfo :: String
showUsageInfo = usageInfo header options
  where
    header = "Usage: un-unicode [OPTION...] files..."

version :: String
version = "0.1.0"

atLeastOne :: (Eq a) => a -> [a] -> Bool
atLeastOne _e [] = True
atLeastOne e (e' : rest) = if e == e' then e `notElem` rest else atLeastOne e rest

getInpOptions :: [Flag] -> IO (Maybe OptInput)
getInpOptions flags =
  let -- the input file as maybe, if not specified, then it is Nothing
      inpFile =
        listToMaybe $
          mapMaybe (\case InputFileOpt file -> Just file; _ -> Nothing) flags
      -- the input file as maybe, if not specified, then it is Nothing
      inpText =
        listToMaybe $
          mapMaybe (\case InputTextOpt text -> Just text; _ -> Nothing) flags
      inpReadEnabled = InputReadOpt `elem` flags
      enabledInpOptions =
        [ isJust inpFile,
          isJust inpText,
          inpReadEnabled
        ]
   in if not $ atLeastOne True enabledInpOptions
        then
          ioError (userError ("more than one input options enabled.\n" ++ showUsageInfo))
        else case inpFile of
          Just file -> return . Just $ FileIn file
          Nothing -> case inpText of
            Just text -> return . Just $ TextIn text
            Nothing ->
              if inpReadEnabled
                then return $ Just ReadIn
                else return Nothing

-- | convert a plain `[Flag]` to a structured `Options`
interpretOption :: [Flag] -> IO Options
interpretOption flags = do
  if ShowHelpOpt `elem` flags || null flags
    then return ShowHelp
    else do
      inpOption <- getInpOptions flags
      let fileWithForced =
            ( do
                file <-
                  listToMaybe $
                    mapMaybe (\case OutputFileOpt file -> Just file; _ -> Nothing) flags
                let forced = ForceReplaceOutFile `elem` flags
                return (file, forced)
            )
      let outputShow = OutputShowOpt `elem` flags
      let outputEnabled = isJust fileWithForced || outputShow
      let showVerEnabled = ShowVersionOpt `elem` flags
      case inpOption of
        Nothing ->
          if showVerEnabled && not outputEnabled
            -- no input or output options enabled
            then return ShowVersion
            -- some output option enabled, but not input
            else ioError (userError ("no input option specified.\n" ++ showUsageInfo))
        Just opt ->
          if not outputEnabled
            then
              return . ConvertOpt $
                Conv
                  { showVersion = showVerEnabled,
                    inpOpt = opt,
                    outOpt =
                      OptOut
                        { -- when no output option is enabled,
                          -- output to stdout
                          showOut = True,
                          fileOut = Nothing
                        },
                    verbOpt = VerbTextOpt `elem` flags
                  }
            else
              return . ConvertOpt $
                Conv
                  { showVersion = showVerEnabled,
                    inpOpt = opt,
                    outOpt =
                      OptOut
                        { showOut = outputShow,
                          fileOut = fileWithForced
                        },
                    verbOpt = VerbTextOpt `elem` flags
                  }

compilerOpts :: [String] -> IO Options
compilerOpts argv =
  case getOpt Permute options argv of
    (flags, [], []) -> interpretOption flags
    (_, n, []) -> ioError (userError ("unrecognized options: " ++ unwords n ++ "\n" ++ showUsageInfo))
    (_, _, errs) -> ioError (userError (concat errs ++ "\n" ++ showUsageInfo))

outputText :: [String] -> OptOutput -> IO ()
outputText outText outOption = do
  when (showOut outOption) $ mapM_ putStrLn outText
  case fileOut outOption of
    Nothing -> return ()
    Just (file, forced) -> do
      exists <- doesFileExist file
      if exists && not forced
        then do
          putStrLn $ warningPrefix ++ "File " ++ file ++ " already exists, do you want to overwrite it? [type \"Y\" to confirm]"
          response <- getLine
          if (toLower <$> response) == "y"
            then
              writeFile file (unlines outText)
            else putStrLn $ "input is " ++ response ++ ", but not Y, exit."
        else writeFile file (unlines outText)

execute :: Options -> IO ()
execute ShowVersion = putStrLn version
execute ShowHelp = putStrLn showUsageInfo
execute (ConvertOpt opt) = do
  when (showVersion opt) $ putStrLn version
  -- get the input text
  inText <- case inpOpt opt of
    FileIn file -> readFile file
    TextIn text -> return text
    ReadIn -> readLn
  -- compute the output text
  outText <- convertText inText
  -- output text based on the output
  outputText outText $ outOpt opt

main :: IO ()
main = do
  argv <- getArgs
  opts <- compilerOpts argv
  execute opts
