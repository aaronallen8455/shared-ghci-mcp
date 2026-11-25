{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Driving a GHCi session that runs in tmux (started by @ghci-session@).
--
-- Input goes in with @tmux send-keys@. Output is read from the log written by
-- @tmux pipe-pane@, never from the screen, so scrollback limits can't cut it
-- off. Each command is followed by a sentinel line whose output marks where
-- the command's output ends (see "SharedGhci.Output"), so nothing depends on
-- GHCi's prompt.
module SharedGhci.Session
  ( GhciError (..)
  , Session
  , newSession
  , sendCommand
  , waitCmd
  , interrupt
  , restart
  , status
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (dropWhileEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTime)
import SharedGhci.Output
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Process (readProcessWithExitCode)

newtype GhciError = GhciError Text
  deriving (Show)

instance Exception GhciError

throwG :: Text -> IO a
throwG = throwIO . GhciError

-- | Contents of @.shared-ghci/state@, written by @ghci-session start@. Paths
-- and tmux arguments are 'String' because that is what the process and
-- filepath APIs take.
data State = State
  { stSession :: String
  , stSocket :: String
  , stTmux :: FilePath
  , stLog :: FilePath
  , stDir :: FilePath
  , stCmd :: String
  , stStateDir :: FilePath
  }

-- | Something we typed and are waiting for: a command, or just a sentinel to
-- find out when GHCi is ready (a probe).
data Pending = Pending
  { pOffset :: !Integer -- ^ log size when it was sent
  , pSentinel :: !Integer -- ^ the sentinel whose marker ends it
  , pLines :: !Int -- ^ input lines typed before the sentinel
  , pProbe :: !Bool
  , pLog :: FilePath
  }

data Session = Session
  { sPending :: IORef (Maybe Pending)
  , sPrompt :: IORef (Maybe Text)
  -- ^ last line of the prompt GHCi showed after our latest sentinel
  , sNextSentinel :: IORef Integer
  }

-- | Sentinel numbers start from the current time, so markers left in the log
-- by an earlier server process can't be mistaken for ours.
newSession :: IO Session
newSession = do
  t <- getPOSIXTime
  Session <$> newIORef Nothing <*> newIORef Nothing <*> newIORef (floor (t * 1000) * 1000)

pollMicros :: Int
pollMicros = 100000

------------------------------------------------------------------------------
-- State and tmux

stateDirName :: FilePath
stateDirName = ".shared-ghci"

loadState :: IO State
loadState = do
  env <- lookupEnv "SHARED_GHCI_DIR"
  start <- case env of
    Just d | not (null d) -> pure d
    _ -> getCurrentDirectory
  let walk d = do
        let f = d </> stateDirName </> "state"
        exists <- doesFileExist f
        if exists
          then pure (d, f)
          else do
            let p = takeDirectory d
            if p == d
              then
                throwG $
                  "No shared GHCi session found for " <> T.pack start
                    <> ". Ask the user to run `ghci-session start` (or "
                    <> "`ghci-session start -- <repl command>`) in the project root."
              else walk p
  (dir, file) <- walk =<< makeAbsolute start
  txt <- decodeLenient <$> BS.readFile file
  let kvs = [(k, T.drop 1 v) | l <- T.lines txt, let (k, v) = T.breakOn "=" l, not (T.null v)]
      get k = maybe "" T.unpack (lookup k kvs)
  pure
    State
      { stSession = get "session"
      , stSocket = get "socket"
      , stTmux = get "tmux"
      , stLog = get "log"
      , stDir = get "dir"
      , stCmd = get "cmd"
      , stStateDir = dir </> stateDirName
      }

tmux :: State -> [String] -> IO (ExitCode, String, String)
tmux st args = readProcessWithExitCode (stTmux st) (["-L", stSocket st] ++ args) ""

tmuxOk :: State -> [String] -> IO ()
tmuxOk st args = do
  (ec, _, err) <- tmux st args
  case ec of
    ExitSuccess -> pure ()
    _ -> throwG ("tmux " <> T.pack (unwords (take 1 args)) <> " failed: " <> T.strip (T.pack err))

-- | The active pane of the exactly-named session.
paneTarget :: State -> String
paneTarget st = "=" ++ stSession st ++ ":"

-- | Type lines into GHCi, each followed by Enter, in a single write. Sending
-- them together means they arrive while GHCi is reading input, so the
-- terminal doesn't echo the later lines into the middle of the output.
typeLines :: State -> [Text] -> IO ()
typeLines st ls =
  tmuxOk st ["send-keys", "-t", paneTarget st, "-l", "--", T.unpack (T.concat (map (<> "\r") ls))]

data PaneState = Running | Dead | Missing
  deriving (Eq)

paneState :: State -> IO PaneState
paneState st = do
  (ec, out, _) <- tmux st ["display-message", "-p", "-t", paneTarget st, "#{pane_dead}"]
  pure $ case ec of
    ExitSuccess | T.strip (T.pack out) == "1" -> Dead
    ExitSuccess -> Running
    _ -> Missing

requireRunning :: State -> IO ()
requireRunning st = do
  ps <- paneState st
  case ps of
    Running -> pure ()
    Missing ->
      throwG $
        "The GHCi tmux session '" <> T.pack (stSession st) <> "' is not running. Ask the "
          <> "user to run `ghci-session start` in the project root."
    Dead -> do
      t <- logTail st 40
      throwG $
        "GHCi has exited (it was quit, crashed, or failed to start). Last output:\n\n"
          <> t
          <> "\n\nFix any problem shown, then call ghci_restart."

------------------------------------------------------------------------------
-- Reading the log

logSize :: State -> IO Integer
logSize st = either (\(_ :: IOException) -> 0) id <$> try (getFileSize (stLog st))

readLog :: State -> Integer -> Maybe Integer -> IO BS.ByteString
readLog st start mend = withBinaryFile (stLog st) ReadMode $ \h -> do
  hSeek h AbsoluteSeek start
  case mend of
    Nothing -> BS.hGetContents h
    Just e -> BS.hGet h (fromIntegral (e - start))

logTailBytes :: State -> Integer -> IO BS.ByteString
logTailBytes st n = do
  size <- logSize st
  readLog st (max 0 (size - n)) (Just size)

-- | The last @n@ lines of the log as plain text, without sentinel lines.
logTail :: State -> Int -> IO Text
logTail st n = do
  dat <- logTailBytes st (64 * 1024)
  let ls = dropWhileEnd T.null (filter (not . isSentinelLine) (clean dat))
  pure (T.intercalate "\n" (lastN n ls))

-- | Text typed at GHCi's prompt but not yet entered, if we can tell. Only
-- known once we've seen GHCi's prompt after one of our sentinels.
unsentInput :: Session -> State -> IO (Maybe Text)
unsentInput sess st = do
  mp <- readIORef (sPrompt sess)
  t <- trailingLine <$> logTailBytes st 4096
  pure $ case mp of
    Just p | not (T.null p), t /= p, p `T.isPrefixOf` t -> Just (T.strip (T.drop (T.length p) t))
    _ -> Nothing

refuseIfUnsentInput :: Session -> State -> IO ()
refuseIfUnsentInput sess st = do
  u <- unsentInput sess st
  case u of
    Nothing -> pure ()
    Just typed ->
      throwG $
        "Someone has typed unsent input at the GHCi prompt: \"" <> T.takeEnd 200 typed
          <> "\". Ask the user to finish or clear it; ghci_interrupt would discard it."

-- | Shorten output for the agent, saving the full text if it was cut.
formatOutput :: State -> [Text] -> IO Text
formatOutput st ls0 = do
  let ls = trimBlankLines ls0
      path = stStateDir st </> "last-output.txt"
  when (length ls > maxLines) $
    withFile path WriteMode $ \h -> hSetEncoding h utf8 >> TIO.hPutStr h (T.unlines ls)
  pure (summarizeOutput path ls)

------------------------------------------------------------------------------
-- Commands

nextSentinel :: Session -> IO Integer
nextSentinel sess = atomicModifyIORef' (sNextSentinel sess) (\n -> (n + 1, n))

pendingFor :: Session -> State -> IO (Maybe Pending)
pendingFor sess st = do
  mp <- readIORef (sPending sess)
  pure $ case mp of
    Just p | pLog p == stLog st -> Just p
    _ -> Nothing

-- | Type @ls@ followed by a new sentinel, and record it as pending.
startPending :: Session -> State -> [Text] -> Bool -> IO ()
startPending sess st ls probe = do
  n <- nextSentinel sess
  size <- logSize st
  writeIORef (sPending sess) (Just (Pending size n (length ls) probe (stLog st)))
  typeLines st (ls ++ [sentinelCommand n])

-- | Send GHCi input and wait up to @timeout@ seconds for its output.
sendCommand :: Session -> Text -> Double -> IO Text
sendCommand sess command timeout = do
  st <- loadState
  requireRunning st
  pend <- pendingFor sess st
  case pend of
    Just p
      | pProbe p -> throwG "GHCi isn't ready yet. Call ghci_wait until it is."
      | otherwise ->
          throwG
            "An earlier command is still running. Use ghci_wait to keep waiting \
            \for it, or ghci_interrupt to cancel it."
    Nothing -> pure ()
  refuseIfUnsentInput sess st
  let ls = prepareInput command
  when (null ls) $ throwG "empty command"
  startPending sess st ls False
  snd <$> waitPending sess st timeout

-- | Wait for the pending sentinel. Returns whether it arrived, and the output.
waitPending :: Session -> State -> Double -> IO (Bool, Text)
waitPending sess st timeout = do
  p <- maybe (throwG "nothing is pending") pure =<< readIORef (sPending sess)
  deadline <- (+ timeout) <$> getMonotonicTime
  let readSince = do
        size <- logSize st
        when (size < pOffset p) $ do
          writeIORef (sPending sess) Nothing
          throwG "The GHCi session was restarted while waiting."
        readLog st (pOffset p) Nothing
      loop = do
        dat <- readSince
        case findCompletion (pSentinel p) (pLines p) dat of
          Just _ -> do
            c <- awaitPrompt (10 :: Int)
            writeIORef (sPending sess) Nothing
            case filter (not . T.null) (cPrompt c) of
              [] -> pure ()
              ps -> writeIORef (sPrompt sess) (Just (last ps))
            out <- formatOutput st (cOutput c)
            if pProbe p
              then (,) True . ("GHCi is ready. Recent output:\n\n" <>) <$> logTail st 40
              else pure (True, out)
          Nothing -> do
            now <- getMonotonicTime
            if now >= deadline
              then
                if pProbe p
                  then do
                    t <- logTail st 20
                    pure
                      ( False
                      , "GHCi isn't ready after " <> showSecs timeout <> "s (still starting, "
                          <> "or busy). Call ghci_wait again to keep waiting. Recent output:\n\n"
                          <> t
                      )
                  else do
                    partial <- formatOutput st (partialOutput (pLines p) dat)
                    pure
                      ( False
                      , partial <> "\n\n[still running after " <> showSecs timeout
                          <> "s: call ghci_wait to keep waiting, or ghci_interrupt to cancel]"
                      )
              else do
                ps <- paneState st
                when (ps /= Running) $ do
                  writeIORef (sPending sess) Nothing
                  requireRunning st
                threadDelay pollMicros
                loop
      -- GHCi prints its prompt just after the marker; give it a moment so we
      -- can learn the prompt (used to tidy output and spot unsent input).
      awaitPrompt tries = do
        dat <- readSince
        case findCompletion (pSentinel p) (pLines p) dat of
          Just c
            | tries <= 0 || any (not . T.null) (cPrompt c) -> pure c
          _ -> threadDelay 50000 >> awaitPrompt (tries - 1)
  loop

-- | Wait for a pending command, or else for GHCi to be ready for input.
waitCmd :: Session -> Double -> IO Text
waitCmd sess timeout = do
  st <- loadState
  requireRunning st
  pend <- pendingFor sess st
  case pend of
    Just _ -> snd <$> waitPending sess st timeout
    Nothing -> do
      refuseIfUnsentInput sess st
      -- GHCi reads this only once it has started and loaded its modules.
      startPending sess st [] True
      snd <$> waitPending sess st timeout

interrupt :: Session -> IO Text
interrupt sess = do
  st <- loadState
  requireRunning st
  pend <- pendingFor sess st
  tmuxOk st ["send-keys", "-t", paneTarget st, "C-c"]
  threadDelay 200000
  -- Ctrl-C usually discards input not yet read, including our sentinel, so
  -- type a new one. Output since the original command is still included.
  n <- nextSentinel sess
  size <- logSize st
  let p = case pend of
        Just old -> old {pSentinel = n}
        Nothing -> Pending size n 0 False (stLog st)
  writeIORef (sPending sess) (Just p)
  typeLines st [sentinelCommand n]
  snd <$> waitPending sess st 10

restart :: Session -> Double -> IO Text
restart sess timeout = do
  st <- loadState
  ps <- paneState st
  when (ps == Missing) $ requireRunning st
  now <- getZonedTime
  withFile (stLog st) AppendMode $ \h ->
    hPutStr h $
      "\n===== shared-ghci: restarted by agent at "
        ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
        ++ " =====\n"
  writeIORef (sPending sess) Nothing
  writeIORef (sPrompt sess) Nothing
  tmuxOk st ["respawn-pane", "-k", "-t", paneTarget st, "-c", stDir st, stCmd st]
  threadDelay 200000
  waitCmd sess timeout

status :: Session -> Int -> IO Text
status sess n = do
  st <- loadState
  ps <- paneState st
  stateLine <- case ps of
    Dead -> pure "GHCi has EXITED"
    Missing -> pure "tmux session not running"
    Running -> do
      pend <- pendingFor sess st
      u <- unsentInput sess st
      mp <- readIORef (sPrompt sess)
      trailing <- trailingLine <$> logTailBytes st 4096
      pure $ case (pend, u) of
        (Just p, _)
          | pProbe p -> "starting up, or busy (ghci_wait to keep waiting)"
          | otherwise -> "running a command sent by the agent (ghci_wait to collect it)"
        (_, Just typed) -> "someone has typed unsent input at the prompt: \"" <> typed <> "\""
        _
          | Just trailing == mp -> "idle at the prompt"
          | otherwise -> "unknown: may be starting or running something (ghci_wait checks)"
  recent <- logTail st n
  pure $
    T.intercalate
      "\n"
      [ "session: " <> T.pack (stSession st) <> " (tmux -L " <> T.pack (stSocket st) <> ")"
      , "command: " <> T.pack (stCmd st)
      , "log:     " <> T.pack (stLog st)
      , "state:   " <> stateLine
      ]
      <> "\n\nRecent output:\n\n"
      <> recent

showSecs :: Double -> Text
showSecs d
  | d == fromIntegral r = T.pack (show r)
  | otherwise = T.pack (show d)
  where
    r = round d :: Integer
