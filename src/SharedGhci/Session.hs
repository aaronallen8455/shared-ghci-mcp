{-# LANGUAGE ScopedTypeVariables #-}

-- | Driving a GHCi session that runs in tmux (started by @ghci-session@).
--
-- Input goes in with @tmux send-keys@. Output is read from the log written by
-- @tmux pipe-pane@, never from the screen, so scrollback limits can't cut it
-- off. A command's output ends where GHCi shows a numbered prompt
-- @[ghci#N]> @ whose N is higher than the one shown before the command.
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
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (dropWhileEnd, intercalate)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import GHC.Clock (getMonotonicTime)
import SharedGhci.Output
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Process (readProcessWithExitCode)

newtype GhciError = GhciError String
  deriving (Show)

instance Exception GhciError

throwG :: String -> IO a
throwG = throwIO . GhciError

-- | Contents of @.shared-ghci/state@, written by @ghci-session start@.
data State = State
  { stSession :: String
  , stSocket :: String
  , stTmux :: FilePath
  , stLog :: FilePath
  , stDir :: FilePath
  , stCmd :: String
  , stInit :: String -- ^ "script" or "typed": how the prompt gets set up
  , stStateDir :: FilePath
  }

-- | A command we sent and are still waiting on.
data Pending = Pending
  { pOffset :: !Integer -- ^ log size when it was sent
  , pPrev :: !Int -- ^ prompt number shown before it
  , pLines :: !Int -- ^ number of echoed input lines to drop
  , pLog :: FilePath
  }

data Session = Session
  { sPending :: IORef (Maybe Pending)
  , sSetupSent :: IORef (Maybe (FilePath, Integer))
  -- ^ log and banner offset we last typed the prompt setup for
  }

newSession :: IO Session
newSession = Session <$> newIORef Nothing <*> newIORef Nothing

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
                  "No shared GHCi session found for " ++ start
                    ++ ". Ask the user to run `ghci-session start` (or "
                    ++ "`ghci-session start -- <repl command>`) in the project root."
              else walk p
  (dir, file) <- walk =<< makeAbsolute start
  txt <- decodeLenient <$> BS.readFile file
  let kvs = [(k, drop 1 v) | l <- lines txt, let (k, v) = break (== '=') l, not (null v)]
      get k = maybe "" id (lookup k kvs)
  pure
    State
      { stSession = get "session"
      , stSocket = get "socket"
      , stTmux = get "tmux"
      , stLog = get "log"
      , stDir = get "dir"
      , stCmd = get "cmd"
      , stInit = get "init"
      , stStateDir = dir </> stateDirName
      }

tmux :: State -> [String] -> IO (ExitCode, String, String)
tmux st args = readProcessWithExitCode (stTmux st) (["-L", stSocket st] ++ args) ""

tmuxOk :: State -> [String] -> IO ()
tmuxOk st args = do
  (ec, _, err) <- tmux st args
  case ec of
    ExitSuccess -> pure ()
    _ -> throwG ("tmux " ++ unwords (take 1 args) ++ " failed: " ++ trim err)

-- | The active pane of the exactly-named session.
paneTarget :: State -> String
paneTarget st = "=" ++ stSession st ++ ":"

data PaneState = Running | Dead | Missing
  deriving (Eq)

paneState :: State -> IO PaneState
paneState st = do
  (ec, out, _) <- tmux st ["display-message", "-p", "-t", paneTarget st, "#{pane_dead}"]
  pure $ case ec of
    ExitSuccess | trim out == "1" -> Dead
    ExitSuccess -> Running
    _ -> Missing

requireRunning :: State -> IO ()
requireRunning st = do
  ps <- paneState st
  case ps of
    Running -> pure ()
    Missing ->
      throwG $
        "The GHCi tmux session '" ++ stSession st ++ "' is not running. Ask the "
          ++ "user to run `ghci-session start` in the project root."
    Dead -> do
      t <- logTail st 40
      throwG $
        "GHCi has exited (it was quit, crashed, or failed to start). Last output:\n\n"
          ++ t
          ++ "\n\nFix any problem shown, then call ghci_restart."

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

-- | Number of the last prompt in the log, and the bytes after it.
lastPrompt :: State -> IO (Maybe (Int, BS.ByteString))
lastPrompt st = do
  size <- logSize st
  dat <- readLog st (max 0 (size - 256 * 1024)) (Just size)
  pure $ case prompts dat of
    [] -> Nothing
    ps -> let p = last ps in Just (promptNumber p, BS.drop (promptEnd p) dat)

logTail :: State -> Int -> IO String
logTail st n = do
  size <- logSize st
  dat <- readLog st (max 0 (size - 64 * 1024)) (Just size)
  let ls = dropWhileEnd null (clean dat)
  pure (intercalate "\n" (lastN n ls))

-- | (idle, reason if not idle)
idleStatus :: State -> IO (Bool, String)
idleStatus st = do
  lp <- lastPrompt st
  pure $ case lp of
    Nothing -> (False, "GHCi has not shown a prompt yet (still building or starting)")
    Just (_, rest)
      | null typed -> (True, "")
      | otherwise ->
          ( False
          , "GHCi is busy, or someone has typed unsent input at the prompt: \""
              ++ lastN 200 typed
              ++ "\""
          )
      where
        typed = trim (concat (clean rest))

-- | For sessions whose command couldn't be given -ghci-script (wrapper
-- scripts, docker, ...), type the prompt setup into GHCi once its startup
-- banner has appeared and no numbered prompt has followed it. Done once per
-- banner, so it happens again after every restart.
ensurePrompt :: Session -> State -> IO ()
ensurePrompt sess st = when (stInit st == "typed") $ do
  size <- logSize st
  let start = max 0 (size - 256 * 1024)
  dat <- readLog st start (Just size)
  case lastBanner dat of
    Nothing -> pure () -- GHCi hasn't started yet
    Just b -> do
      let key = (stLog st, start + fromIntegral b)
          numberedPromptSince = any ((> b) . promptStart) (prompts dat)
      sent <- readIORef (sSetupSent sess)
      unless (numberedPromptSince || sent == Just key) $ do
        writeIORef (sSetupSent sess) (Just key)
        forM_ promptSetup $ \l -> do
          tmuxOk st ["send-keys", "-t", paneTarget st, "-l", "--", l]
          tmuxOk st ["send-keys", "-t", paneTarget st, "Enter"]

-- | Shorten output for the agent, saving the full text if it was cut.
formatOutput :: State -> [String] -> IO String
formatOutput st ls0 = do
  let ls = trimBlankLines ls0
      path = stStateDir st </> "last-output.txt"
  when (length ls > maxLines) $
    withFile path WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h (unlines ls)
  pure (summarizeOutput path ls)

------------------------------------------------------------------------------
-- Commands

-- | Send GHCi input and wait up to @timeout@ seconds for its output.
sendCommand :: Session -> String -> Double -> IO String
sendCommand sess command timeout = do
  let ref = sPending sess
  st <- loadState
  requireRunning st
  pend <- readIORef ref
  case pend of
    Just p
      | pLog p == stLog st ->
          throwG
            "An earlier command is still running. Use ghci_wait to keep waiting \
            \for it, or ghci_interrupt to cancel it."
    _ -> writeIORef ref Nothing
  ensurePrompt sess st
  (idle, why) <- idleStatus st
  unless idle $ do
    t <- logTail st 15
    throwG $
      why ++ ".\n\nUse ghci_wait to wait for it, or ghci_interrupt to cancel. "
        ++ "Recent output:\n\n" ++ t
  let ls = prepareInput command
  when (null ls) $ throwG "empty command"
  prev <- maybe (throwG "no GHCi prompt found") (pure . fst) =<< lastPrompt st
  size <- logSize st
  writeIORef ref (Just (Pending size prev (length ls) (stLog st)))
  forM_ ls $ \l -> do
    unless (null l) $ tmuxOk st ["send-keys", "-t", paneTarget st, "-l", "--", l]
    tmuxOk st ["send-keys", "-t", paneTarget st, "Enter"]
  waitPending sess st timeout

waitPending :: Session -> State -> Double -> IO String
waitPending sess st timeout = do
  let ref = sPending sess
  p <- maybe (throwG "no command is pending") pure =<< readIORef ref
  deadline <- (+ timeout) <$> getMonotonicTime
  let loop = do
        size <- logSize st
        when (size < pOffset p) $ do
          writeIORef ref Nothing
          throwG "The GHCi session was restarted while waiting."
        dat <- readLog st (pOffset p) Nothing
        case [pr | pr <- prompts dat, promptNumber pr > pPrev p] of
          pr : _ -> do
            writeIORef ref Nothing
            formatOutput st (drop (pLines p) (clean (BS.take (promptStart pr) dat)))
          [] -> do
            now <- getMonotonicTime
            if now >= deadline
              then do
                partial <- formatOutput st (drop (pLines p) (clean dat))
                pure $
                  partial ++ "\n\n[still running after " ++ showSecs timeout
                    ++ "s: call ghci_wait to keep waiting, or ghci_interrupt to cancel]"
              else do
                ps <- paneState st
                when (ps /= Running) $ do
                  writeIORef ref Nothing
                  requireRunning st
                threadDelay pollMicros
                loop
  loop

-- | Wait for a pending command, or else for GHCi to become ready.
waitCmd :: Session -> Double -> IO String
waitCmd sess timeout = do
  let ref = sPending sess
  st <- loadState
  requireRunning st
  pend <- readIORef ref
  case pend of
    Just p | pLog p == stLog st -> waitPending sess st timeout
    _ -> do
      writeIORef ref Nothing
      deadline <- (+ timeout) <$> getMonotonicTime
      let loop = do
            ensurePrompt sess st
            (idle, why) <- idleStatus st
            if idle
              then ("GHCi is ready. Recent output:\n\n" ++) <$> logTail st 40
              else do
                now <- getMonotonicTime
                if now >= deadline
                  then do
                    t <- logTail st 20
                    pure $
                      "Still not ready after " ++ showSecs timeout ++ "s (" ++ why
                        ++ "). Recent output:\n\n" ++ t
                  else requireRunning st >> threadDelay pollMicros >> loop
      loop

interrupt :: Session -> IO String
interrupt sess = do
  let ref = sPending sess
  st <- loadState
  requireRunning st
  pend <- readIORef ref
  case pend of
    Just p | pLog p == stLog st -> pure ()
    _ -> do
      lp <- lastPrompt st
      size <- logSize st
      writeIORef ref (Just (Pending size (maybe (-1) fst lp) 0 (stLog st)))
  tmuxOk st ["send-keys", "-t", paneTarget st, "C-c"]
  waitPending sess st 10

restart :: Session -> Double -> IO String
restart sess timeout = do
  let ref = sPending sess
  st <- loadState
  ps <- paneState st
  when (ps == Missing) $ requireRunning st
  now <- getZonedTime
  -- The banner also ensures the old prompt is no longer the last thing in the
  -- log, so we don't mistake it for the new GHCi being ready.
  withFile (stLog st) AppendMode $ \h ->
    hPutStr h $
      "\n===== shared-ghci: restarted by agent at "
        ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
        ++ " =====\n"
  writeIORef ref Nothing
  tmuxOk st ["respawn-pane", "-k", "-t", paneTarget st, "-c", stDir st, stCmd st]
  threadDelay 500000
  waitCmd sess timeout

status :: Session -> Int -> IO String
status sess n = do
  let ref = sPending sess
  st <- loadState
  ps <- paneState st
  stateLine <- case ps of
    Running -> do
      ensurePrompt sess st
      (idle, why) <- idleStatus st
      pure (if idle then "idle, ready for commands" else why)
    Dead -> pure "GHCi has EXITED"
    Missing -> pure "tmux session not running"
  pend <- readIORef ref
  t <- logTail st n
  pure $
    intercalate
      "\n"
      ( [ "session: " ++ stSession st ++ " (tmux -L " ++ stSocket st ++ ")"
        , "command: " ++ stCmd st
        , "log:     " ++ stLog st
        , "state:   " ++ stateLine
        ]
          ++ ["note:    a command sent by this agent is still running (ghci_wait)" | Just _ <- [pend]]
      )
      ++ "\n\nRecent output:\n\n"
      ++ t

showSecs :: Double -> String
showSecs d
  | d == fromIntegral r = show r
  | otherwise = show d
  where
    r = round d :: Integer
