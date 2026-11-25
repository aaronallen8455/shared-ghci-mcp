{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end tests: start a real GHCi on a small module with
-- bin/ghci-session, then drive it through the shared-ghci-mcp server over
-- stdio, the way an MCP client would.
--
-- The tests share one GHCi session and run in order.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import Control.Monad (filterM, unless, void, when)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitSuccess)
import System.FilePath ((</>))
import System.IO
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process hiding (env)
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  tmuxFound <- findTmux
  when (null tmuxFound) $ do
    hPutStrLn stderr "integration: SKIPPED, tmux not found (set SHARED_GHCI_TMUX to its path)"
    exitSuccess
  defaultMain $
    withResource setup teardown $ \getEnv ->
      dependentTestGroup "integration" AllFinish (map ($ getEnv) tests)

------------------------------------------------------------------------------
-- The tests

tests :: [IO Env -> TestTree]
tests =
  [ test "lists the tools" $ \env -> do
      r <- rpc env "tools/list" (object [])
      let names = parseMaybe (withObject "r" $ \o -> o .: "result" >>= (.: "tools") >>= mapM (withObject "tool" (.: "name"))) r
      fmap sort names
        @?= Just (sort ["ghci_eval", "ghci_reload", "ghci_wait", "ghci_interrupt", "ghci_restart", "ghci_status" :: Text])
  , test "ghci_wait reports when GHCi is ready" $ \env -> do
      out <- ok env "ghci_wait" ["timeout_seconds" .= (120 :: Int)]
      assertContains "GHCi is ready" out
      assertContains "Ok, one module loaded." out
  , test "evaluates an expression" $ \env ->
      (@?= "42") =<< eval env "double 21"
  , test "runs GHCi commands" $ \env ->
      (@?= "double :: Int -> Int") =<< eval env ":t double"
  , test "passes non-ASCII text through" $ \env ->
      (@?= "\"h\233llo \10003\" :: String") =<< eval env ":t \"h\233llo \10003\""
  , test "returns output that has no trailing newline" $ \env ->
      (@?= "no newline") =<< eval env "putStr \"no newline\""
  , test "sends multi-line input containing tabs" $ \env -> do
      (@?= "(no output)") =<< eval env "triple :: Int -> Int\ntriple x =\n\tx * 3"
      (@?= "15") =<< eval env "triple 5"
  , test "reload returns a long error in full" $ \env -> do
      TIO.writeFile (envDir env </> "Demo.hs") (demoSource <> badDefinition)
      out <- ok env "ghci_reload" []
      assertContains "Couldn't match" out
      assertContains "show 59" out -- the end of the long expression
      assertBool ("expected a long error, got " ++ show (length (T.lines out)) ++ " lines")
        (length (T.lines out) > 100)
      assertBool "last line should report failure" ("Failed" `T.isPrefixOf` last (T.lines out))
      TIO.writeFile (envDir env </> "Demo.hs") demoSource
      -- GHC says "loaded" or "reloaded" depending on the version
      assertContains "Ok, one module" =<< ok env "ghci_reload" []
  , test "a timed-out command can be waited for" $ \env -> do
      out <- ok env "ghci_eval" ["command" .= ("Control.Concurrent.threadDelay 3000000 >> putStrLn \"done\"" :: Text), "timeout_seconds" .= (1 :: Int)]
      assertContains "still running after 1s" out
      (@?= "done") =<< ok env "ghci_wait" ["timeout_seconds" .= (30 :: Int)]
  , test "refuses new input while a command runs, and interrupts it" $ \env -> do
      out <- ok env "ghci_eval" ["command" .= ("length [1..]" :: Text), "timeout_seconds" .= (1 :: Int)]
      assertContains "still running" out
      failsWith env "ghci_eval" ["command" .= ("1+1" :: Text)] "An earlier command is still running"
      assertContains "Interrupted." =<< ok env "ghci_interrupt" []
      (@?= "2") =<< eval env "1+1"
  , test "shortens long output and saves all of it" $ \env -> do
      out <- T.lines <$> eval env "mapM_ print [1..1000]"
      length out @?= 363
      take 1 out @?= ["1"]
      last out @?= "1000"
      assertBool "omitted-lines note" (any ("640 lines omitted" `T.isInfixOf`) out)
      saved <- TIO.readFile (envDir env </> ".shared-ghci" </> "last-output.txt")
      length (T.lines saved) @?= 1000
  , test "won't send while the human has typed input at the prompt" $ \env -> do
      envTmux env ["send-keys", "-t", envTarget env, "-l", "map (+1"]
      threadDelay 500000
      failsWith env "ghci_eval" ["command" .= ("1+1" :: Text)] "typed unsent input at the GHCi prompt: \"map (+1\""
      envTmux env ["send-keys", "-t", envTarget env, "C-u"]
      threadDelay 500000
      (@?= "2") =<< eval env "1+1"
  , test "reports status" $ \env -> do
      out <- ok env "ghci_status" ["lines" .= (5 :: Int)]
      assertContains "state:   idle at the prompt" out
  , test "detects that GHCi exited, and restarts it" $ \env -> do
      failsWith env "ghci_eval" ["command" .= (":quit" :: Text)] "GHCi has exited"
      assertContains "GHCi is ready" =<< ok env "ghci_restart" ["timeout_seconds" .= (120 :: Int)]
      (@?= "8") =<< eval env "double 4"
  , test "answers malformed requests with JSON-RPC errors" $ \env -> do
      sendLine env "this is not json"
      r <- readResponse env
      errorCode r @?= Just (-32700)
      r2 <- rpc env "no/such/method" (object [])
      errorCode r2 @?= Just (-32601)
  , test "reports bad tool calls as tool errors" $ \env -> do
      failsWith env "ghci_eval" [] "invalid arguments"
      failsWith env "ghci_eval" ["command" .= (42 :: Int)] "expected String"
      failsWith env "no_such_tool" [] "unknown tool: no_such_tool"
  , test "runs GHCi through a wrapper script" $ \_ ->
      withProject ["./repl.sh"] writeWrapper $ \dir -> do
        withServer dir $ \env -> do
          assertContains "GHCi is ready" =<< ok env "ghci_wait" ["timeout_seconds" .= (120 :: Int)]
          (@?= "42") =<< eval env "double 21"
          -- the setup must be typed again after a restart
          assertContains "GHCi is ready" =<< ok env "ghci_restart" ["timeout_seconds" .= (120 :: Int)]
          (@?= "8") =<< eval env "double 4"
  , test "keeps working when the prompt changes" $ \env -> do
      (@?= "(no output)") =<< eval env ":set prompt \"changed> \""
      (@?= "42") =<< eval env "double 21"
      (@?= "(no output)") =<< eval env ":set prompt \"\\nmulti\\nline> \""
      (@?= "42") =<< eval env "double 21"
      (@?= "no newline") =<< eval env "putStr \"no newline\""
      (@?= "(no output)") =<< eval env ":set prompt-function \\mods n -> return (show n ++ unwords mods ++ \" $ \")"
      (@?= "8") =<< eval env "double 4"
      (@?= "(no output)") =<< eval env ":set prompt \"my-prompt> \""
  , test "explains when there is no session" $ \_ -> do
      tmp <- getCanonicalTemporaryDirectory
      bracket (createTempDirectory tmp "no-session") removeDirectoryRecursive $ \dir ->
        withServer dir $ \env -> do
          (isErr, out) <- callTool env "ghci_status" []
          assertBool "should be an error" isErr
          assertContains "No shared GHCi session found" out
  ]
  where
    test name body = \getEnv -> testCase name (getEnv >>= body)

-- | Stands in for a wrapper such as one that runs GHCi in docker, with some
-- output and a delay before GHCi starts.
writeWrapper :: FilePath -> IO ()
writeWrapper dir = do
  let f = dir </> "repl.sh"
  TIO.writeFile f "#!/bin/sh\necho 'starting container...'\nsleep 1\nexec ghci Demo.hs\n"
  p <- getPermissions f
  setPermissions f (setOwnerExecutable True p)

-- | A project .ghci that sets its own prompt, as many do.
dotGhci :: Text
dotGhci = ":set prompt \"my-prompt> \"\n:set prompt-cont \"my-prompt| \"\n"

demoSource :: Text
demoSource =
  T.unlines
    [ "module Demo where"
    , ""
    , "double :: Int -> Int"
    , "double x = x * 2"
    ]

-- | A type error whose message is well over 100 lines long.
badDefinition :: Text
badDefinition =
  "\nbad :: Int\nbad = \"oops\" ++ "
    <> T.intercalate " ++ " ["show " <> T.pack (show i) | i <- [0 .. 59 :: Int]]
    <> "\n"

------------------------------------------------------------------------------
-- Setting up GHCi and the server

data Env = Env
  { envDir :: FilePath
  , envIn :: Handle
  , envOut :: Handle
  , envServer :: ProcessHandle
  , envNextId :: IORef Int
  , envTmux :: [String] -> IO ()
  , envTarget :: String
  }

setup :: IO Env
setup = do
  script <- makeAbsolute ("bin" </> "ghci-session")
  tmp <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmp "shared-ghci-test"
  TIO.writeFile (dir </> "Demo.hs") demoSource
  TIO.writeFile (dir </> ".ghci") dotGhci
  callProcessIn dir script ["start", "--", "ghci", "Demo.hs"]
  st <- readState dir
  let get k = T.unpack (fromMaybe (error ("state file has no " ++ T.unpack k)) (lookup k st))
  env <- startServer dir
  pure
    env
      { envTmux = \args -> callProcess (get "tmux") (["-L", get "socket"] ++ args)
      , envTarget = "=" ++ get "session" ++ ":"
      }

teardown :: Env -> IO ()
teardown env = do
  stopServer env
  script <- makeAbsolute ("bin" </> "ghci-session")
  void (readCreateProcessWithExitCode ((proc script ["stop"]) {cwd = Just (envDir env)}) "")
  removeDirectoryRecursive (envDir env)

-- | A separate project and GHCi session, started with the given command after
-- @prepare@ has run in the project directory.
withProject :: [String] -> (FilePath -> IO ()) -> (FilePath -> IO a) -> IO a
withProject cmd prepare body = do
  script <- makeAbsolute ("bin" </> "ghci-session")
  tmp <- getCanonicalTemporaryDirectory
  bracket (createTempDirectory tmp "shared-ghci-wrapper") removeDirectoryRecursive $ \dir -> do
    TIO.writeFile (dir </> "Demo.hs") demoSource
    TIO.writeFile (dir </> ".ghci") dotGhci
    prepare dir
    bracket_
      (callProcessIn dir script ("start" : "--" : cmd))
      (void (readCreateProcessWithExitCode ((proc script ["stop"]) {cwd = Just dir}) ""))
      (body dir)

withServer :: FilePath -> (Env -> IO a) -> IO a
withServer dir = bracket (startServer dir) stopServer

-- | Start the server in @dir@ and complete the MCP handshake.
startServer :: FilePath -> IO Env
startServer dir = do
  exe <- findServer
  (Just hin, Just hout, _, ph) <-
    createProcess (proc exe []) {cwd = Just dir, std_in = CreatePipe, std_out = CreatePipe}
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  ref <- newIORef 0
  let env = Env dir hin hout ph ref (\_ -> pure ()) ""
  r <- rpc env "initialize" $
    object
      [ "protocolVersion" .= ("2025-06-18" :: Text)
      , "capabilities" .= object []
      , "clientInfo" .= object ["name" .= ("test" :: Text), "version" .= ("0" :: Text)]
      ]
  unless (isResult r) $ assertFailure ("initialize failed: " ++ show r)
  sendLine env (encode (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)]))
  pure env
  where
    isResult = maybe False (const True) . parseMaybe (withObject "r" (.: "result") :: Value -> Parser Value)

stopServer :: Env -> IO ()
stopServer env = do
  hClose (envIn env)
  r <- timeout 5000000 (waitForProcess (envServer env))
  when (r == Nothing) $ terminateProcess (envServer env)

-- | cabal puts build-tool-depends executables on PATH while running tests.
findServer :: IO FilePath
findServer = do
  override <- lookupEnv "SHARED_GHCI_MCP"
  found <- maybe (findExecutable "shared-ghci-mcp") (pure . Just) override
  maybe (fail "shared-ghci-mcp not found on PATH (set SHARED_GHCI_MCP)") pure found

findTmux :: IO [FilePath]
findTmux = do
  override <- lookupEnv "SHARED_GHCI_TMUX"
  onPath <- findExecutable "tmux"
  home <- getHomeDirectory
  let candidates =
        maybe [] pure override
          ++ maybe [] pure onPath
          ++ [ home </> ".nix-profile/bin/tmux"
             , "/run/current-system/sw/bin/tmux"
             , "/opt/homebrew/bin/tmux"
             , "/usr/local/bin/tmux"
             , "/usr/bin/tmux"
             ]
  filterM doesFileExist candidates

-- | Run a command in a directory, failing the test (with its output) if it fails.
callProcessIn :: FilePath -> FilePath -> [String] -> IO ()
callProcessIn dir exe args = do
  (ec, out, err) <- readCreateProcessWithExitCode (proc exe args) {cwd = Just dir} ""
  when (ec /= ExitSuccess) $
    assertFailure (unwords (exe : args) ++ " failed (" ++ show ec ++ "):\n" ++ out ++ err)

readState :: FilePath -> IO [(Text, Text)]
readState dir = do
  txt <- TIO.readFile (dir </> ".shared-ghci" </> "state")
  pure [(k, T.drop 1 v) | l <- T.lines txt, let (k, v) = T.breakOn "=" l, not (T.null v)]

------------------------------------------------------------------------------
-- Talking to the server

sendLine :: Env -> BL.ByteString -> IO ()
sendLine env bs = BL.hPut (envIn env) (bs <> "\n") >> hFlush (envIn env)

readResponse :: Env -> IO Value
readResponse env = do
  ml <- timeout (300 * 1000000) (BC.hGetLine (envOut env))
  l <- maybe (assertFailure "no response from server within 300s") pure ml
  maybe (assertFailure ("response is not JSON: " ++ BC.unpack l)) pure (decodeStrict l)

rpc :: Env -> Text -> Value -> IO Value
rpc env method params = do
  i <- atomicModifyIORef' (envNextId env) (\n -> (n + 1, n + 1))
  sendLine env . encode $
    object ["jsonrpc" .= ("2.0" :: Text), "id" .= i, "method" .= method, "params" .= params]
  readResponse env

-- | Call a tool; returns (isError, text).
callTool :: Env -> Text -> [Pair] -> IO (Bool, Text)
callTool env name args = do
  r <- rpc env "tools/call" (object ["name" .= name, "arguments" .= object args])
  maybe (assertFailure ("unexpected tools/call response: " ++ show r)) pure (parseMaybe toolResult r)
  where
    toolResult = withObject "response" $ \o -> do
      res <- o .: "result"
      isErr <- res .: "isError"
      content <- res .: "content"
      texts <- mapM (withObject "content" (.: "text")) content
      pure (isErr, T.concat texts)

ok :: Env -> Text -> [Pair] -> IO Text
ok env name args = do
  (isErr, out) <- callTool env name args
  when isErr $ assertFailure (show name ++ " returned an error:\n" ++ T.unpack out)
  pure out

eval :: Env -> Text -> IO Text
eval env cmd = ok env "ghci_eval" ["command" .= cmd]

failsWith :: Env -> Text -> [Pair] -> Text -> IO ()
failsWith env name args needle = do
  (isErr, out) <- callTool env name args
  unless isErr $ assertFailure (show name ++ " should have failed, but returned:\n" ++ T.unpack out)
  assertContains needle out

errorCode :: Value -> Maybe Int
errorCode = parseMaybe (withObject "r" $ \o -> o .: "error" >>= (.: "code"))

assertContains :: Text -> Text -> Assertion
assertContains needle hay =
  unless (needle `T.isInfixOf` hay) $
    assertFailure ("expected to find " ++ show needle ++ " in:\n" ++ T.unpack hay)
