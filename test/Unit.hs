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
      [ sentinelTests
      , stripAnsiTests
      , renderLineTests
      , cleanTests
      , prepareInputTests
      , summarizeTests
      ]

sentinelTests :: TestTree
sentinelTests =
  testGroup
    "sentinels"
    [ testCase "the typed sentinel never contains its own marker" $
        findCompletion 7 0 (BC.pack (sentinelCommand 7 ++ "\r\n")) @?= Nothing
    , testCase "finds a command's output and the prompt that follows" $
        findCompletion 7 1 (logOf "ghci> " "double 21" "42\r\n" 7)
          @?= Just (Completion ["42"] ["ghci>"])
    , testCase "keeps output that has no trailing newline" $
        cOutput <$> findCompletion 7 1 (logOf "ghci> " "putStr \"hi\"" "hi" 7) @?= Just ["hi"]
    , testCase "is Nothing until the marker arrives" $
        findCompletion 7 1 (BC.pack "double 21\r\n42\r\nghci> ") @?= Nothing
    , testCase "ignores markers of other sentinels" $
        findCompletion 8 1 (logOf "ghci> " "double 21" "42\r\n" 7) @?= Nothing
    , testCase "removes a multi-line prompt" $
        cOutput <$> findCompletion 7 1 (logOf "\r\nmulti\r\nprompt> " "double 21" "42\r\n" 7) @?= Just ["42"]
    , testCase "removes a prompt whose number changes" $
        cOutput <$> findCompletion 7 1 (BC.pack ("putStr \"hi\"\r\nhi15 Demo $ " ++ sentinelCommand 7 ++ "\r\n<<shared-ghci:7>>\r\n16 Demo $ "))
          @?= Just ["hi"]
    , testCase "copes with extra lines after the marker (:set +t)" $
        cOutput <$> findCompletion 7 1 (BC.pack ("double 21\r\n42\r\nghci> " ++ sentinelCommand 7 ++ "\r\n<<shared-ghci:7>>\r\nit :: ()\r\nghci> "))
          @?= Just ["42"]
    , testCase "drops every echoed input line of multi-line input" $
        cOutput <$> findCompletion 7 4 (logOf "ghci> " ":{\r\nghci| f x =\r\nghci|   x\r\nghci| :}" "" 7) @?= Just []
    , testCase "hides leftovers of an interrupted command" $
        cOutput
          <$> findCompletion 7 1
            ( BC.pack
                ( "length [1..]\r\n^CInterrupted.\r\nghci> " ++ sentinelCommand 6 ++ "\r\n<<shared-ghci:6>>\r\n"
                    ++ "ghci> " ++ sentinelCommand 7 ++ "\r\n<<shared-ghci:7>>\r\nghci> "
                )
            )
          @?= Just ["^CInterrupted."]
    , testCase "partialOutput drops input echo and sentinel lines" $
        partialOutput 1 (BC.pack ("length [1..]\r\n" ++ sentinelCommand 7 ++ "\r\nworking\r\n")) @?= ["working", ""]
    , testCase "isSentinelLine recognises echoes and markers only" $
        map isSentinelLine ["ghci> " ++ sentinelCommand 3, "<<shared-ghci:3>>", "42", "<<other>>"]
          @?= [True, True, False, False]
    , testCase "trailingLine is the prompt plus unsent input" $
        trailingLine (BC.pack "42\r\n\ESC[1mghci> \ESC[0mmap (+1") @?= "ghci> map (+1"
    ]
  where
    -- What the log holds after typing @input@ and a sentinel at an idle GHCi
    -- (whose prompt was printed before the log offset): the echoed input,
    -- the output, the prompt with the echoed sentinel, the marker, the prompt.
    logOf prompt input output n =
      BC.pack
        ( input ++ "\r\n" ++ output ++ prompt ++ sentinelCommand n ++ "\r\n"
            ++ "<<shared-ghci:" ++ show (n :: Integer) ++ ">>\r\n" ++ prompt
        )

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
        clean (BC.pack "ghci> 1+1\r\r\n2\r\n") @?= ["ghci> 1+1", "2", ""]
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
