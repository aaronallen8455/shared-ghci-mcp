-- | Pure helpers: marking the end of a command's output, turning raw terminal
-- output into plain text, preparing input to send, and shortening long output.
--
-- After each command the server types a /sentinel/ line,
--
-- > System.IO.putStrLn "<<shared-ghci:\&17>>"
--
-- which GHCi runs once the command has finished, printing the /marker/
-- @<<shared-ghci:17>>@. The @\\&@ (an empty escape) means the echoed input
-- never looks like the marker, and nothing depends on what GHCi's prompt is.
module SharedGhci.Output
  ( -- * Sentinels
    sentinelCommand
  , sentinelMarker
  , isSentinelLine
  , Completion (..)
  , findCompletion
  , partialOutput
  , trailingLine
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
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

------------------------------------------------------------------------------
-- Sentinels

sentinelTag :: String
sentinelTag = "<<shared-ghci:"

-- | What the typed sentinel line looks like up to its number, as echoed.
sentinelEchoPrefix :: String
sentinelEchoPrefix = "System.IO.putStrLn \"" ++ sentinelTag ++ "\\&"

-- | The line to type after a command.
sentinelCommand :: Integer -> String
sentinelCommand n = sentinelEchoPrefix ++ show n ++ ">>\""

-- | What GHCi prints when it runs @sentinelCommand n@.
sentinelMarker :: Integer -> BS.ByteString
sentinelMarker n = BC.pack (sentinelTag ++ show n ++ ">>")

-- | Is this cleaned line an echo of any sentinel, or any sentinel's output?
-- (Used to hide them; markers of earlier, interrupted commands can show up.)
isSentinelLine :: String -> Bool
isSentinelLine l = sentinelEchoPrefix `isInfixOf` l || isMarker (trim l)
  where
    isMarker s = sentinelTag `isPrefixOf` s && ">>" `isSuffixOf` s

-- | A finished command.
data Completion = Completion
  { cOutput :: [String]
  -- ^ the command's output, without the echoed input, sentinel or prompt
  , cPrompt :: [String]
  -- ^ lines GHCi printed after the marker: its next prompt (possibly
  -- incomplete if read too soon), and any lines such as @it :: ()@ before it
  }
  deriving (Eq, Show)

-- | Look for sentinel @n@'s marker in the bytes logged since a command was
-- sent. @echoed@ is how many input lines were typed before the sentinel;
-- GHCi echoes each on its own line, and they are dropped.
findCompletion :: Integer -> Int -> BS.ByteString -> Maybe Completion
findCompletion n echoed dat
  | BS.null rest = Nothing
  | otherwise = Just (Completion (removeEcho (drop echoed (clean pre))) promptLs)
  where
    marker = sentinelMarker n
    (pre, rest) = BS.breakSubstring marker dat
    -- the first cleaned line is the remainder of the marker's own line
    promptLs = drop 1 (clean (BS.drop (BS.length marker) rest))

    -- @pre@ ends with GHCi's prompt followed by the echoed sentinel. Text
    -- before the prompt on that line is output without a trailing newline.
    removeEcho ls = case break (sentinelEchoPrefix `isInfixOf`) (reverse ls) of
      (_, []) -> filter (not . isSentinelLine) ls
      (_, echoLine : revBefore) ->
        let before = filter (not . isSentinelLine) (reverse revBefore)
            x0 = dropWhileEnd isSpace (takeBefore sentinelEchoPrefix echoLine)
         in stripPrompt before x0 promptLs

-- | Remove the prompt that precedes the echoed sentinel. It is matched
-- against the prompt GHCi printed after the marker, with each run of digits
-- matching any run of digits (prompts often show a line number or the time).
-- A prompt can span several lines; the longest tail of 'cPrompt' that
-- matches is removed.
stripPrompt :: [String] -> String -> [String] -> [String]
stripPrompt before x0 promptLs =
  fromMaybe plain (listToMaybe [r | k <- [length promptLs, length promptLs - 1 .. 1], Just r <- [tryK k]])
  where
    plain = before ++ [x0 | not (null x0)]
    tryK k =
      let pk = map (dropWhileEnd isSpace) (lastN k promptLs)
          (pInit, pLast) = (init pk, last pk)
          m = length pInit
       in case promptSuffixLength pLast x0 of
            Just len
              | length before >= m
              , and (zipWith matchesWhole pInit (lastN m before)) ->
                  let x = dropWhileEnd isSpace (take (length x0 - len) x0)
                   in Just (take (length before - m) before ++ [x | not (null x)])
            _ -> Nothing
    matchesWhole p l = promptSuffixLength p l == Just (length l)

-- | If @s@ ends with text matching prompt @p@, the length of that text. Each
-- run of digits in @p@ matches any non-empty run of digits.
promptSuffixLength :: String -> String -> Maybe Int
promptSuffixLength p s = go (reverse p) (reverse s) 0
  where
    go [] _ n = Just n
    go pr@(c : _) sr n
      | isDigit c =
          let (ds, sr') = span isDigit sr
           in if null ds then Nothing else go (dropWhile isDigit pr) sr' (n + length ds)
    go (c : pr) (d : sr) n | c == d = go pr sr (n + 1)
    go _ _ _ = Nothing

-- | Output so far of a command that hasn't finished, with input echo and
-- sentinel lines removed.
partialOutput :: Int -> BS.ByteString -> [String]
partialOutput echoed = filter (not . isSentinelLine) . drop echoed . clean

-- | The last (unterminated) line of terminal output: at an idle GHCi, its
-- prompt plus anything typed but not yet entered.
trailingLine :: BS.ByteString -> String
trailingLine dat = case clean dat of
  [] -> ""
  ls -> last ls

takeBefore :: String -> String -> String
takeBefore needle = go []
  where
    go acc s@(c : r)
      | needle `isPrefixOf` s = reverse acc
      | otherwise = go (c : acc) r
    go acc [] = reverse acc

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
