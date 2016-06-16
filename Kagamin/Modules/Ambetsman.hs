{-# LANGUAGE OverloadedStrings #-}
-- | Finds your way in the Ambetsman jungle 
module Kagamin.Modules.Ambetsman where

import Kagamin.Modules
import qualified Data.Text as T
import Kagamin.TextUtils
import Web.Slack.Message
import Text.Read
import System.Random
import Control.Monad.State (MonadIO (..))
import Data.Maybe (catMaybes)
import KagaInfo (kagaID)
import Data.Aeson
import Data.Vector
import System.Random 

newtype Ambetsman = Ambetsman (Vector Text, Vector Text)

instance FromJSON Ambetsman where
  parseJSON (Object v) = Ambetsman <$> do
    job    <- v .: "job"
    surjob <- v .: "surjob"
    return (job, surjob)

loadAmbetsman = undefined 

jobname :: Ambetsman -> IO Text
jobname (Ambetsman (job, surjob)) = do
  prefix <- (job !)    <$> randomRIO (0, length job)
  suffix <- (surjob !) <$> randomRIO (0, length surjob)
  return $ prefix `T.append` suffix

ambetsman :: IO Kagamodule
ambetsman = do
  ambetsmanDB <- loadAmbetsman
  return $ defaultModule {kagaMsgHook = handleKagaMsg ambetsmanDB }

handleKagaMsg :: Ambetsman -> MsgHook
handleKagaMsg ambetsman cid _from msg
  | "yrke" `T.isInfixOf` msg = do
      jn <- jobname ambetsman
      sendMessage cid $ T.concat ["Du borde jobba som ", jn]
  | otherwise = return Next
      
