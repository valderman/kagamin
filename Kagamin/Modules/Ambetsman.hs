{-# LANGUAGE OverloadedStrings #-}
-- | Finds your way in the Ambetsman jungle 
module Kagamin.Modules.Ambetsman where

import Kagamin.Modules
import qualified Data.Text as T
import Web.Slack.Message
import System.Random (randomRIO)
import Control.Monad.State (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Default 
import qualified Data.ByteString.Lazy as BL (readFile)
import qualified Data.Vector as V
import Control.Concurrent.MVar

-- | Ambetsman type with fromJSON instance
newtype Ambetsman = Ambetsman (V.Vector T.Text, V.Vector T.Text)

instance FromJSON Ambetsman where
  parseJSON (Object v) = Ambetsman <$> do
    job    <- v .: "job"
    surjob <- v .: "surjob"
    return (job, surjob)
  parseJSON _ = fail "Badly formatted json :("

instance Default Ambetsman where
  def = Ambetsman (V.fromList ["idiot"], V.fromList [""])

-- | Loads the ambetsman
loadAmbetsman :: MVar Ambetsman -> FilePath -> IO () 
loadAmbetsman ambetsman dir = do
  oldAmbetsman <- takeMVar ambetsman
  ambets <- decode <$> BL.readFile (dir ++ "/assets/ambetsman.json")
  putMVar ambetsman $ fromMaybe oldAmbetsman ambets

-- | Generate a job 
jobname :: MVar Ambetsman -> IO T.Text
jobname ambetsman = do
  Ambetsman (job, surjob) <- readMVar ambetsman 
  prefix <- (job V.!)    <$> randomRIO (0, length job - 1)
  suffix <- (surjob V.!) <$> randomRIO (0, length surjob - 1)
  return $ prefix `T.append` suffix

-- | Kagamin Ambetsman Module 
kagaAmbetsman :: IO KagaModule
kagaAmbetsman = do
  ambetsman <- newMVar def
  return $ defaultModule {kagaMsgHook = handleKagaMsg ambetsman,
                          kagaLoadHook = loadAmbetsman ambetsman}

-- | Handle messages 
handleKagaMsg :: MVar Ambetsman -> MsgHook
handleKagaMsg ambetsman cid _from msg
  | "yrke" `T.isInfixOf` msg = do
      jn <- liftIO $ jobname ambetsman
      sendMessage cid $ T.concat ["Du borde jobba som ", jn]
      return Next
  | otherwise = return Next

