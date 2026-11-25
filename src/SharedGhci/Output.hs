{-# LANGUAGE OverloadedStrings #-}

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
  , lastN
  ) where

import qualified Data.ByteString as BS
import Data.Char (isDigit, isSpace)
import Data.Foldable (toList)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)

------------------------------------------------------------------------------
-- Sentinels

sentinelTag :: Text
sentinelTag = "<<shared-ghci:"

-- | What the typed sentinel line looks like up to its number, as echoed.
sentinelEchoPrefix :: Text
sentinelEchoPrefix = "System.IO.putStrLn \"" <> sentinelTag <> "\\&"

-- | The line to type after a command.
sentinelCommand :: Integer -> Text
sentinelCommand n = sentinelEchoPrefix <> tshow n <> ">>\""

-- | What GHCi prints when it runs @sentinelCommand n@.
sentinelMarker :: Integer -> BS.ByteString
sentinelMarker n = encodeUtf8 (sentinelTag <> tshow n <> ">>")

-- | Is this cleaned line an echo of any sentinel, or any sentinel's output?
-- (Used to hide them; markers of earlier, interrupted commands can show up.)
isSentinelLine :: Text -> Bool
isSentinelLine l = sentinelEchoPrefix `T.isInfixOf` l || isMarker (T.strip l)
  where
    isMarker s = sentinelTag `T.isPrefixOf` s && ">>" `T.isSuffixOf` s

-- | A finished command.
data Completion = Completion
  { cOutput :: [Text]
  -- ^ the command's output, without the echoed input, sentinel or prompt
  , cPrompt :: [Text]
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
    removeEcho ls = case break (sentinelEchoPrefix `T.isInfixOf`) (reverse ls) of
      (_, []) -> filter (not . isSentinelLine) ls
      (_, echoLine : revBefore) ->
        let before = filter (not . isSentinelLine) (reverse revBefore)
            x0 = T.stripEnd (fst (T.breakOn sentinelEchoPrefix echoLine))
         in stripPrompt before x0 promptLs

-- | Remove the prompt that precedes the echoed sentinel. It is matched
-- against the prompt GHCi printed after the marker, with each run of digits
-- matching any run of digits (prompts often show a line number or the time).
-- A prompt can span several lines; the longest tail of 'cPrompt' that
-- matches is removed.
stripPrompt :: [Text] -> Text -> [Text] -> [Text]
stripPrompt before x0 promptLs =
  fromMaybe plain (listToMaybe [r | k <- [length promptLs, length promptLs - 1 .. 1], Just r <- [tryK k]])
  where
    plain = before ++ [x0 | not (T.null x0)]
    tryK k =
      let pk = map T.stripEnd (lastN k promptLs)
          (pInit, pLast) = (init pk, last pk)
          m = length pInit
       in case promptSuffixLength pLast x0 of
            Just len
              | length before >= m
              , and (zipWith matchesWhole pInit (lastN m before)) ->
                  let x = T.stripEnd (T.dropEnd len x0)
                   in Just (take (length before - m) before ++ [x | not (T.null x)])
            _ -> Nothing
    matchesWhole p l = promptSuffixLength p l == Just (T.length l)

-- | If @s@ ends with text matching prompt @p@, the length of that text. Each
-- run of digits in @p@ matches any non-empty run of digits.
promptSuffixLength :: Text -> Text -> Maybe Int
promptSuffixLength p s = go (T.reverse p) (T.reverse s) 0
  where
    go pr sr n = case T.uncons pr of
      Nothing -> Just n
      Just (c, pr')
        | isDigit c ->
            let (ds, sr') = T.span isDigit sr
             in if T.null ds then Nothing else go (T.dropWhile isDigit pr) sr' (n + T.length ds)
        | otherwise -> case T.uncons sr of
            Just (d, sr') | c == d -> go pr' sr' (n + 1)
            _ -> Nothing

-- | Output so far of a command that hasn't finished, with input echo and
-- sentinel lines removed.
partialOutput :: Int -> BS.ByteString -> [Text]
partialOutput echoed = filter (not . isSentinelLine) . drop echoed . clean

-- | The last (unterminated) line of terminal output: at an idle GHCi, its
-- prompt plus anything typed but not yet entered.
trailingLine :: BS.ByteString -> Text
trailingLine dat = case clean dat of
  [] -> ""
  ls -> last ls

------------------------------------------------------------------------------
-- Cleaning terminal output

-- | Decode UTF-8, replacing invalid bytes with U+FFFD.
decodeLenient :: BS.ByteString -> Text
decodeLenient = decodeUtf8With lenientDecode

-- | Turn raw terminal output into lines of plain text.
clean :: BS.ByteString -> [Text]
clean = map (renderLine . T.dropWhileEnd (== '\r')) . T.splitOn "\n" . stripAnsi . decodeLenient

-- | Remove terminal escape sequences (colours, cursor movement, titles) and
-- bell characters.
stripAnsi :: Text -> Text
stripAnsi = T.concat . go
  where
    go t =
      let (plain, rest) = T.break (\c -> c == '\ESC' || c == '\a') t
       in plain : case T.uncons rest of
            Nothing -> []
            Just ('\a', r) -> go r
            Just (_, r) -> go (skipEscape r)
    -- the text after an ESC, with the rest of the escape sequence removed
    skipEscape r = case T.uncons r of
      Just ('[', r') -> T.drop 1 (T.dropWhile (not . csiFinal) r') -- CSI
      Just (']', r') -> dropOsc r' -- OSC
      Just (c, r')
        | c == '(' || c == ')' -> T.drop 1 r' -- charset selection
        | (c >= '@' && c <= '_') || c == '=' || c == '>' -> r' -- 2-byte escapes
      _ -> r
    csiFinal c = c >= '@' && c <= '~'
    -- OSC ends with BEL or ESC \
    dropOsc r =
      let rest = T.dropWhile (\c -> c /= '\a' && c /= '\ESC') r
       in case T.uncons rest of
            Just ('\a', r') -> r'
            Just (_, r') -> case T.uncons r' of
              Just ('\\', r'') -> r''
              _ -> dropOsc r'
            Nothing -> T.empty

-- | Apply carriage returns and backspaces within one line the way a terminal
-- would, and drop trailing whitespace.
renderLine :: Text -> Text
renderLine l
  | not (T.any (\c -> c == '\r' || c == '\b') l) = T.stripEnd l
  | otherwise = T.stripEnd (T.pack (toList (fst (T.foldl' step (Seq.empty, 0) l))))
  where
    step (buf, _) '\r' = (buf, 0)
    step (buf, p) '\b' = (buf, max 0 (p - 1))
    step (buf, p) c
      | p < Seq.length buf = (Seq.update p c buf, p + 1)
      | otherwise = (buf Seq.|> c, p + 1 :: Int)

------------------------------------------------------------------------------
-- Input

-- | Split input into the lines to type into GHCi. Tabs become spaces (a tab
-- would trigger completion), trailing blank lines are dropped, and multi-line
-- input is wrapped in @:{@ / @:}@ unless it already is. Empty input gives @[]@.
prepareInput :: Text -> [Text]
prepareInput command
  | length ls > 1 && map T.strip (take 1 ls) /= [":{"] = [":{"] ++ ls ++ [":}"]
  | otherwise = ls
  where
    ls =
      dropWhileEnd (T.all isSpace) . T.splitOn "\n" . T.replace "\t" "    " $
        T.filter (/= '\r') command

------------------------------------------------------------------------------
-- Long output

-- | Output longer than this is shortened.
maxLines, headLines, tailLines :: Int
maxLines = 400
headLines = 300
tailLines = 60

trimBlankLines :: [Text] -> [Text]
trimBlankLines = dropWhile T.null . dropWhileEnd T.null

-- | Join already-trimmed lines for the agent. More than 'maxLines' lines are
-- cut to the first 'headLines' and last 'tailLines', with a note saying the
-- full output is at the given path (the caller writes that file).
summarizeOutput :: FilePath -> [Text] -> Text
summarizeOutput savedTo ls
  | null ls = "(no output)"
  | n <= maxLines = T.intercalate "\n" ls
  | otherwise =
      T.intercalate "\n" $
        take headLines ls
          ++ [ ""
             , "... [" <> tshow (n - headLines - tailLines) <> " lines omitted; full output ("
                 <> tshow n <> " lines) saved to " <> T.pack savedTo <> "] ..."
             , ""
             ]
          ++ lastN tailLines ls
  where
    n = length ls

------------------------------------------------------------------------------
-- Utilities

lastN :: Int -> [a] -> [a]
lastN k xs = drop (length xs - k) xs

tshow :: Show a => a -> Text
tshow = T.pack . show
