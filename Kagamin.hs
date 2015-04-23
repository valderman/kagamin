module Main where
import Web.Slack (Event (..), SlackConfig (..), SlackBot, runBot)
import System.Posix.Signals (installHandler, sigINT, Handler (..))
import System.Exit (ExitCode (..))
import Control.Exception (throwTo)
import Control.Concurrent (myThreadId)
import Kagamin.State (StateRef, readState, writeState)
import Kagamin.TextUtils (toKagamin)
import Kagamin.Handlers (handleMsg, handleKagaMsg, handleOtherMsg)

import KagaInfo (kagaToken, kagaID) -- Slack API token + bot ID

kagaConfig :: SlackConfig
kagaConfig = SlackConfig {
    _slackApiToken = kagaToken
  }

main :: IO ()
main = do
    stref <- readState "kagamin"
    t <- myThreadId
    _ <- installHandler sigINT (Catch $ intHandler t stref) Nothing
    runBot kagaConfig kagamin stref
  where
    intHandler t r = do
      writeState "kagamin" r
      putStrLn "Bye!"
      throwTo t ExitSuccess

-- | Kagamin's personality entry point. See @Kagamin.Handlers@ if you're
--   looking to add/change behaviours.
kagamin :: SlackBot StateRef
kagamin (Message cid from msg _ _ _) = do
  if (toKagamin kagaID msg)
    then handleKagaMsg kagaID kagaToken cid from msg
    else handleOtherMsg kagaID kagaToken cid from msg
  handleMsg kagaID kagaToken cid from msg
kagamin _ = do
  return ()
