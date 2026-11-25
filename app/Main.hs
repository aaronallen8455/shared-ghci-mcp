{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | shared-ghci MCP server: lets an AI agent use a GHCi session running in
-- tmux (started with @ghci-session start@) that a human can attach to.
-- Speaks MCP over stdio: one JSON-RPC message per line.
module Main (main) where

import Control.Exception
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import SharedGhci.Session
import System.IO

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetEncoding stderr utf8
  sess <- newSession
  let loop = do
        eof <- isEOF
        unless eof $ do
          line <- BC.hGetLine stdin
          unless (BC.all (`elem` (" \t\r" :: String)) line) $ do
            resp <- case eitherDecodeStrict line of
              Left _ -> pure (Just (errorResp Null (-32700) "parse error"))
              Right req -> handleRequest sess req
            mapM_ (\r -> BL.hPut stdout (encode r <> "\n") >> hFlush stdout) resp
          loop
  loop

-- | id (absent for notifications), method, params
data Request = Request (Maybe Value) Text Object

instance FromJSON Request where
  parseJSON = withObject "JSON-RPC request" $ \o ->
    Request <$> o .:? "id" <*> o .: "method" <*> o .:? "params" .!= mempty

handleRequest :: Session -> Request -> IO (Maybe Value)
handleRequest _ (Request Nothing _ _) = pure Nothing
handleRequest sess (Request (Just mid) method params) = Just <$> case method of
  "initialize" ->
    pure . ok mid $
      object
        [ "protocolVersion" .= either (const defaultVersion) id (parseIn (params .:? "protocolVersion" .!= defaultVersion))
        , "capabilities" .= object ["tools" .= object []]
        , "serverInfo" .= object ["name" .= ("shared-ghci" :: Text), "version" .= ("0.1.0" :: Text)]
        , "instructions"
            .= ( "Tools for a GHCi session shared with the user in tmux. After editing \
                 \Haskell sources, call ghci_reload for compiler feedback. Use ghci_eval \
                 \for :t / :i / evaluating expressions." :: Text
               )
        ]
  "ping" -> pure (ok mid (object []))
  "tools/list" -> pure (ok mid (object ["tools" .= tools]))
  "tools/call" -> do
    r <- try . try $ do
      (name, args) <- either (throwIO . GhciError) pure . parseIn $
        (,) <$> params .: "name" <*> params .:? "arguments" .!= mempty
      t <- callTool sess name args
      _ <- evaluate (length t)
      pure t
    let (txt, isErr) = case r of
          Right (Right t) -> (t, False)
          Right (Left (GhciError e)) -> (e, True)
          Left (e :: SomeException) -> ("internal error: " ++ show e, True)
    pure . ok mid $
      object
        [ "content" .= [object ["type" .= ("text" :: Text), "text" .= txt]]
        , "isError" .= isErr
        ]
  _ -> pure (errorResp mid (-32601) ("method not found: " <> method))

defaultVersion :: Text
defaultVersion = "2025-06-18"

-- | Run a parser built from already-decoded fields.
parseIn :: Parser a -> Either String a
parseIn p = parseEither (const p) ()

ok :: Value -> Value -> Value
ok mid result = object ["jsonrpc" .= ("2.0" :: Text), "id" .= mid, "result" .= result]

errorResp :: Value -> Int -> Text -> Value
errorResp mid code msg =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= mid
    , "error" .= object ["code" .= code, "message" .= msg]
    ]

callTool :: Session -> Text -> Object -> IO String
callTool sess name args = case name of
  "ghci_eval" -> do
    cmd <- arg (args .: "command")
    t <- timeoutArg 60
    sendCommand sess cmd t
  "ghci_reload" -> sendCommand sess ":reload" =<< timeoutArg 300
  "ghci_wait" -> waitCmd sess =<< timeoutArg 120
  "ghci_interrupt" -> interrupt sess
  "ghci_restart" -> restart sess =<< timeoutArg 300
  "ghci_status" -> status sess . max 1 =<< arg (args .:? "lines" .!= 40)
  _ -> throwIO (GhciError ("unknown tool: " ++ T.unpack name))
  where
    arg :: Parser a -> IO a
    arg p = either (throwIO . GhciError . ("invalid arguments: " ++)) pure (parseIn p)
    timeoutArg :: Double -> IO Double
    timeoutArg d = do
      t <- arg (args .:? "timeout_seconds" .!= d)
      pure (if t > 0 then t else d)

------------------------------------------------------------------------------
-- Tool definitions

tools :: [Value]
tools =
  [ tool
      "ghci_eval"
      "Send input to the shared GHCi session and return its output. Accepts \
      \anything you could type at the GHCi prompt: expressions, `let` bindings, \
      \and commands such as `:t expr`, `:i Name`, `:k Type`, `:browse Mod`, \
      \`:load File.hs`, `:set -XFoo`. Multi-line input is wrapped in :{ :} \
      \automatically. A human may be watching or using the same session, so don't \
      \run commands that take over the terminal for a long time without reason. \
      \Long output is cut down and saved in full to a file whose path is given."
      [ "command" .= object ["type" .= ("string" :: Text), "description" .= ("GHCi input." :: Text)]
      , "timeout_seconds" .= timeoutProp 60
      ]
      ["command"]
  , tool
      "ghci_reload"
      "Run :reload in the shared GHCi session and return the compiler output \
      \(errors and warnings, then 'Ok, N modules loaded.' or 'Failed, ...'). Use \
      \this after editing Haskell source files for fast compiler feedback, instead \
      \of a full cabal/stack build."
      ["timeout_seconds" .= timeoutProp 300]
      []
  , tool
      "ghci_wait"
      "Wait for a command that timed out to finish and return the rest of its \
      \output. If nothing is pending, wait until GHCi is ready for input (for \
      \example while it is still starting up) and show recent output."
      ["timeout_seconds" .= timeoutProp 120]
      []
  , tool
      "ghci_interrupt"
      "Send Ctrl-C to GHCi to cancel a running evaluation or reload."
      []
      []
  , tool
      "ghci_restart"
      "Restart GHCi (same command as originally started) and wait until it is \
      \ready. Use after changing .cabal/package.yaml/cabal.project, when GHCi has \
      \exited, or when its state is broken. Discards all GHCi bindings, so ask the \
      \user first if they may be using the session interactively."
      ["timeout_seconds" .= timeoutProp 300]
      []
  , tool
      "ghci_status"
      "Show whether the shared GHCi session is running and ready, plus its most \
      \recent output (which may include commands typed by the human)."
      [ "lines"
          .= object
            [ "type" .= ("integer" :: Text)
            , "minimum" .= (1 :: Int)
            , "maximum" .= (500 :: Int)
            , "default" .= (40 :: Int)
            , "description" .= ("How many recent lines to show." :: Text)
            ]
      ]
      []
  ]
  where
    tool :: Text -> Text -> [(Key, Value)] -> [Text] -> Value
    tool name desc props required =
      object
        [ "name" .= name
        , "description" .= desc
        , "inputSchema"
            .= object
              ( ["type" .= ("object" :: Text), "properties" .= object props]
                  ++ ["required" .= required | not (null required)]
              )
        ]
    timeoutProp :: Double -> Value
    timeoutProp d =
      object
        [ "type" .= ("number" :: Text)
        , "minimum" .= (1 :: Int)
        , "maximum" .= (3600 :: Int)
        , "default" .= d
        , "description"
            .= ( "Seconds to wait for output before returning what has arrived so \
                 \far (the command keeps running)." :: Text
               )
        ]
