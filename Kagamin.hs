module Main where
import Control.Monad (void)
import Web.Slack (Event (..), SlackConfig (..), SlackBot, runBot)
import System.Posix.Signals (installHandler, sigINT, Handler (..))
import System.Exit (ExitCode (..))
import Kagamin.TextUtils (toKagamin)
import Data.Text (strip)
import Control.Concurrent

import KagaInfo (kagaToken, kagaID) -- Slack API token + bot ID

import Kagamin.Modules
import Kagamin.Modules.Markov
import Kagamin.Modules.Links
import Kagamin.Modules.Dice
import Kagamin.Modules.Misc


allModules :: [IO KagaModule]
allModules = [kagaMarkov, kagaLinks, kagaDice, kagaMisc]

kagaConfig :: SlackConfig
kagaConfig = SlackConfig {
    _slackApiToken = kagaToken
  }

main :: IO ()
main = do
    saveLock <- newMVar ()
    mods <- sequence allModules
    mapM_ (flip kagaLoadHook ".") mods
    t <- myThreadId
    void $ installHandler sigINT (Catch $ intHandler t mods saveLock) Nothing
    void $ forkIO $ stateBackup mods saveLock
    runBot kagaConfig (kagamin mods) ()
  where
    stateBackup mods saveLock = do
      delayMins (24*60)
      withMVar saveLock $ \_ -> mapM_ (flip kagaSaveHook ".") mods
      stateBackup mods saveLock

    delayMins :: Int -> IO ()
    delayMins 0   = return ()
    delayMins n
      | n >= 10   = threadDelay (600*1000000) >> delayMins (n-10)
      | otherwise = threadDelay (60*1000000) >> delayMins (n-1)

    intHandler t mods saveLock = do
      putStrLn "Saving state..."
      _ <- takeMVar saveLock
      mapM_ (flip kagaSaveHook ".") mods
      putStrLn "Bye!"
      throwTo t ExitSuccess

-- | Kagamin's personality entry point.
kagamin :: [KagaModule] -> SlackBot ()
kagamin mods (Message cid from msg _ _ _) = do
  let msg' = strip msg
  void $ if (toKagamin kagaID msg')
           then forAll kagaMsgHook mods cid from msg'
           else forAll kagaOtherHook mods cid from msg'
  void $ forAll kagaAllHook mods cid from msg'
kagamin _ _ = do
  return ()

-- | Run the given hook for all modules in a list.
forAll :: (KagaModule -> MsgHook) -> [KagaModule] -> MsgHook
forAll hook mods cid from message = go message mods
  where
    go msg (m:ms) = do
      res <- hook m cid from msg
      case res of
        Next        -> go msg ms
        Modify msg' -> go msg' ms
        Stop        -> return Stop
    go msg _ = do
      return (Modify msg)
