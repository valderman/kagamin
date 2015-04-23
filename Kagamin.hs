module Main where
import Web.Slack
import Control.Monad.State

-- Markov chain stuff
import Data.Text.Binary ()
import Data.IORef
import DissociatedPress
import System.Posix.Signals
import System.Exit
import Control.Exception
import Control.Concurrent (myThreadId)

-- Link log
import qualified Data.Binary as B
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS

import Kagamin.State
import Kagamin.TextUtils (toKagamin)
import Kagamin.Handlers

import KagaInfo (kagaToken, kagaID) -- Slack API token + bot ID

kagaConfig :: SlackConfig
kagaConfig = SlackConfig {
    _slackApiToken = kagaToken
  }

main :: IO ()
main = do
    d <- load "kagamin.dict"
    elinks <- try $! BS.readFile "kagamin.links" >>= evaluate . B.decode
    let links = case elinks of
                  Right l -> l
                  Left e  -> (e :: SomeException) `seq` S.empty
    stref <- newIORef $! KagaState {
        stateDict  = maybe defDict id d,
        stateLinks = links
      }
    t <- myThreadId
    void $ installHandler sigINT (Catch $ intHandler t stref) Nothing
    runBot kagaConfig kagamin stref
  where
    intHandler t r = do
      st <- readIORef r
      store "kagamin.dict" (stateDict st)
      BS.writeFile "kagamin.links" (B.encode $ stateLinks st)
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
