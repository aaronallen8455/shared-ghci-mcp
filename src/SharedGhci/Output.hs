-- | Pure helpers: finding GHCi's prompts in raw terminal output, turning that
-- output into plain text, preparing input to send, and shortening long output.
module SharedGhci.Output
  ( -- * Prompts
    Prompt (..)
  , prompts
  , promptSetup
  , lastBanner
    -- * Cleaning terminal output
  , decodeLenient
  , clean
  , stripAnsi
  , renderLine
    -- * Input
  , prepareInput
    -- * Long output
  , maxLines
  , headLines
  , tailLines
  , trimBlankLines
  , summarizeOutput
    -- * Utilities
  , splitOn
  , trim
  , lastN
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

------------------------------------------------------------------------------
-- Prompts

-- | A @[ghci#N]> @ prompt found in the log.
data Prompt = Prompt
  { promptNumber :: !Int
  , promptStart :: !Int -- ^ byte index of the @[@
  , promptEnd :: !Int -- ^ byte index just after the trailing space
  }
  deriving (Eq, Show)

-- | Every main prompt @[ghci#N]> @ in the data, in order. Continuation
-- prompts (@[ghci#N]| @) and incomplete prompts are ignored.
prompts :: BS.ByteString -> [Prompt]
prompts = go 0
  where
    marker = BC.pack "[ghci#"
    close = BC.pack "]> "
    go off bs =
      let (pre, rest) = BS.breakSubstring marker bs
          i = off + BS.length pre
          afterMarker = BS.drop (BS.length marker) rest
          digits = BC.takeWhile isDigit afterMarker
          afterDigits = BS.drop (BS.length digits) afterMarker
       in if BS.null rest
            then []
            else
              if not (BS.null digits) && close `BS.isPrefixOf` afterDigits
                then
                  let e = i + BS.length marker + BS.length digits + BS.length close
                   in Prompt (read (BC.unpack digits)) i e : go e (BS.drop (e - off) bs)
                else go (i + 1) (BS.drop (i + 1 - off) bs)

-- | GHCi commands that install the numbered prompts 'prompts' looks for.
-- The same text is in the init.ghci that @ghci-session@ writes.
promptSetup :: [String]
promptSetup =
  [ ":set prompt-function \\_ n -> return (\"[ghci#\" ++ show n ++ \"]> \")"
  , ":set prompt-cont-function \\_ n -> return (\"[ghci#\" ++ show n ++ \"]| \")"
  ]

-- | Byte index of the last @GHCi, version@ startup banner in the data. Once it
-- has been printed, GHCi owns the terminal and typed input reaches it.
lastBanner :: BS.ByteString -> Maybe Int
lastBanner = go Nothing 0
  where
    banner = BC.pack "GHCi, version "
    go found off bs =
      let (pre, rest) = BS.breakSubstring banner bs
          i = off + BS.length pre
       in if BS.null rest
            then found
            else go (Just i) (i + 1) (BS.drop (BS.length pre + 1) bs)

------------------------------------------------------------------------------
-- Cleaning terminal output

-- | Decode UTF-8, replacing invalid bytes with U+FFFD.
decodeLenient :: BS.ByteString -> String
decodeLenient = T.unpack . decodeUtf8With lenientDecode

-- | Turn raw terminal output into lines of plain text.
clean :: BS.ByteString -> [String]
clean = map (renderLine . dropWhileEnd (== '\r')) . splitOn '\n' . stripAnsi . decodeLenient

-- | Remove terminal escape sequences (colours, cursor movement, titles) and
-- bell characters.
stripAnsi :: String -> String
stripAnsi s = case s of
  '\ESC' : '[' : r -> stripAnsi (drop 1 (dropWhile (not . csiFinal) r))
  '\ESC' : ']' : r -> stripAnsi (dropOsc r)
  '\ESC' : c : _ : r | c `elem` "()" -> stripAnsi r
  '\ESC' : c : r | (c >= '@' && c <= '_') || c `elem` "=>" -> stripAnsi r
  '\ESC' : r -> stripAnsi r
  '\a' : r -> stripAnsi r
  c : r -> c : stripAnsi r
  [] -> []
  where
    csiFinal c = c >= '@' && c <= '~'
    dropOsc ('\a' : r) = r
    dropOsc ('\ESC' : '\\' : r) = r
    dropOsc (_ : r) = dropOsc r
    dropOsc [] = []

-- | Apply carriage returns and backspaces within one line the way a terminal
-- would, and drop trailing whitespace.
renderLine :: String -> String
renderLine l
  | all (\c -> c /= '\r' && c /= '\b') l = dropWhileEnd isSpace l
  | otherwise = dropWhileEnd isSpace (toList (go Seq.empty 0 l))
  where
    go buf _ [] = buf
    go buf _ ('\r' : r) = go buf 0 r
    go buf p ('\b' : r) = go buf (max 0 (p - 1)) r
    go buf p (c : r)
      | p < Seq.length buf = go (Seq.update p c buf) (p + 1) r
      | otherwise = go (buf Seq.|> c) (p + 1) r

------------------------------------------------------------------------------
-- Input

-- | Split input into the lines to type into GHCi. Tabs become spaces (a tab
-- would trigger completion), trailing blank lines are dropped, and multi-line
-- input is wrapped in @:{@ / @:}@ unless it already is. Empty input gives @[]@.
prepareInput :: String -> [String]
prepareInput command
  | length ls > 1 && map trim (take 1 ls) /= [":{"] = [":{"] ++ ls ++ [":}"]
  | otherwise = ls
  where
    expand '\t' = "    "
    expand c = [c]
    ls = dropWhileEnd (all isSpace) (splitOn '\n' (concatMap expand (filter (/= '\r') command)))

------------------------------------------------------------------------------
-- Long output

-- | Output longer than this is shortened.
maxLines, headLines, tailLines :: Int
maxLines = 400
headLines = 300
tailLines = 60

trimBlankLines :: [String] -> [String]
trimBlankLines = dropWhile null . dropWhileEnd null

-- | Join already-trimmed lines for the agent. More than 'maxLines' lines are
-- cut to the first 'headLines' and last 'tailLines', with a note saying the
-- full output is at the given path (the caller writes that file).
summarizeOutput :: FilePath -> [String] -> String
summarizeOutput savedTo ls
  | null ls = "(no output)"
  | n <= maxLines = intercalate "\n" ls
  | otherwise =
      intercalate "\n" $
        take headLines ls
          ++ [ ""
             , "... [" ++ show (n - headLines - tailLines) ++ " lines omitted; full output ("
                 ++ show n ++ " lines) saved to " ++ savedTo ++ "] ..."
             , ""
             ]
          ++ lastN tailLines ls
  where
    n = length ls

------------------------------------------------------------------------------
-- Utilities

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : r) -> a : splitOn c r

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

lastN :: Int -> [a] -> [a]
lastN k xs = drop (length xs - k) xs
