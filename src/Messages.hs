{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- | the collection of messages to output
module Messages where

warningPrefix :: String
warningPrefix = "WARNING: "

unrecognizedUnicode :: Char -> String 
unrecognizedUnicode uniChar = warningPrefix ++ "Unrecognized Unicode: \"" 
    ++ uniChar : "\", preserving it in the final output."

standAloneCombing :: Char -> String
standAloneCombing uniChar = warningPrefix ++ "Standalone Combining Character \"" 
    ++ uniChar : "\", preserving it in the final output."

internalErrorPrefix :: String 
internalErrorPrefix = "Internal error encountered. This is a bug; please consider reporting it."