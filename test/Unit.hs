module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as BS
import SharedGhci.Output
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "unit"
      [ promptTests
      , stripAnsiTests
      , renderLineTests
      , cleanTests
      , prepareInputTests
      , summarizeTests
      ]

promptTests :: TestTree
promptTests =
  testGroup
    "prompts"
    [ testCase "finds a prompt with its position" $
        prompts (BC.pack "ab[ghci#12]> ") @?= [Prompt 12 2 13]
    , testCase "finds several prompts in order" $
        map promptNumber (prompts (BC.pack "[ghci#1]> 1+1\r\n2\r\n[ghci#2]> ")) @?= [1, 2]
    , testCase "ignores continuation prompts" $
        map promptNumber (prompts (BC.pack "[ghci#3]| x\r\n[ghci#4]> ")) @?= [4]
    , testCase "ignores incomplete or malformed prompts" $
        prompts (BC.pack "[ghci#]> [ghci#5] [ghci#x]> [ghci#6") @?= []
    , testCase "finds a prompt right after output with no newline" $
        map promptNumber (prompts (BC.pack "no newline[ghci#3]> ")) @?= [3]
    , testCase "finds a prompt after a false start" $
        map promptNumber (prompts (BC.pack "[ghci#[ghci#7]> ")) @?= [7]
    , testCase "typing promptSetup doesn't itself look like a prompt" $
        prompts (BC.pack (concat promptSetup)) @?= []
    , testCase "lastBanner finds the most recent GHCi banner" $
        lastBanner (BC.pack "GHCi, version 9.4\r\n...\r\nGHCi, version 9.4\r\n") @?= Just 24
    , testCase "lastBanner is Nothing before GHCi starts" $
        lastBanner (BC.pack "Resolving dependencies...\r\nBuilding library...") @?= Nothing
    ]

stripAnsiTests :: TestTree
stripAnsiTests =
  testGroup
    "stripAnsi"
    [ testCase "leaves plain text alone" $
        stripAnsi "Ok, one module loaded." @?= "Ok, one module loaded."
    , testCase "removes colour codes" $
        stripAnsi "\ESC[1m\ESC[31merror:\ESC[0m bad" @?= "error: bad"
    , testCase "removes cursor movement and line clearing" $
        stripAnsi "a\ESC[2K\ESC[10Db\ESC[?25h" @?= "ab"
    , testCase "removes OSC sequences ended by BEL or ST" $
        stripAnsi "\ESC]0;title\aX\ESC]8;;http://x\ESC\\Y" @?= "XY"
    , testCase "removes charset selection and two-byte escapes" $
        stripAnsi "\ESC(Bx\ESC=y\ESC>z" @?= "xyz"
    , testCase "removes bells and a trailing lone ESC" $
        stripAnsi "a\ab\ESC" @?= "ab"
    ]

renderLineTests :: TestTree
renderLineTests =
  testGroup
    "renderLine"
    [ testCase "carriage return overwrites from the start" $
        renderLine "abc\rX" @?= "Xbc"
    , testCase "backspace moves left" $
        renderLine "abc\b\bX" @?= "aXc"
    , testCase "backspace stops at the start of the line" $
        renderLine "a\b\b\bX" @?= "X"
    , testCase "drops trailing whitespace" $
        renderLine "text   \t" @?= "text"
    , testCase "spaces then backspaces (haskeline redraw) leave the text" $
        renderLine "ab   \b\b\b" @?= "ab"
    ]

cleanTests :: TestTree
cleanTests =
  testGroup
    "clean"
    [ testCase "splits lines and drops CR before LF" $
        clean (BC.pack "[ghci#1]> 1+1\r\r\n2\r\n") @?= ["[ghci#1]> 1+1", "2", ""]
    , testCase "removes colours inside lines" $
        clean (BC.pack "\ESC[31mError\ESC[0m\r\n") @?= ["Error", ""]
    , testCase "decodes UTF-8" $
        clean (BS.pack [0xE2, 0x80, 0xA2, 0x20, 0x78]) @?= ["\x2022 x"]
    , testCase "replaces invalid UTF-8 instead of failing" $
        clean (BS.pack [0x61, 0xFF, 0x62]) @?= ["a\xFFFD\&b"]
    ]

prepareInputTests :: TestTree
prepareInputTests =
  testGroup
    "prepareInput"
    [ testCase "single line is sent as is" $
        prepareInput "map (+1) [1,2]" @?= ["map (+1) [1,2]"]
    , testCase "empty input gives no lines" $
        prepareInput "  \n\n" @?= []
    , testCase "multi-line input is wrapped in :{ :}" $
        prepareInput "f :: Int\nf = 1" @?= [":{", "f :: Int", "f = 1", ":}"]
    , testCase "input already wrapped is not wrapped again" $
        prepareInput ":{\nf = 1\n:}" @?= [":{", "f = 1", ":}"]
    , testCase "tabs become spaces" $
        prepareInput "f x =\n\tx" @?= [":{", "f x =", "    x", ":}"]
    , testCase "CRLF and trailing blank lines are handled" $
        prepareInput "1 +\r\n  2\r\n\r\n" @?= [":{", "1 +", "  2", ":}"]
    ]

summarizeTests :: TestTree
summarizeTests =
  testGroup
    "summarizeOutput"
    [ testCase "no lines gives a placeholder" $
        summarizeOutput "out.txt" [] @?= "(no output)"
    , testCase "short output is joined unchanged" $
        summarizeOutput "out.txt" ["a", "", "b"] @?= "a\n\nb"
    , testCase "trimBlankLines drops blank lines at both ends only" $
        trimBlankLines ["", "", "a", "", "b", ""] @?= ["a", "", "b"]
    , testCase "output at the limit is not cut" $
        length (lines (summarizeOutput "out.txt" (numbered maxLines))) @?= maxLines
    , testCase "long output keeps head and tail with a note" $ do
        let out = lines (summarizeOutput "out.txt" (numbered 1000))
        length out @?= headLines + 3 + tailLines
        take 2 out @?= ["1", "2"]
        last out @?= "1000"
        out !! headLines @?= ""
        out !! (headLines + 1)
          @?= "... [640 lines omitted; full output (1000 lines) saved to out.txt] ..."
        out !! (headLines + 3) @?= show (1000 - tailLines + 1)
    ]
  where
    numbered n = map show [1 .. n :: Int]
