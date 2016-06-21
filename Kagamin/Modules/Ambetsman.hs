{-# LANGUAGE OverloadedStrings #-}
-- | Finds your way in the Ambetsman jungle 
module Kagamin.Modules.Ambetsman where

import System.Directory (doesFileExist)
import Kagamin.Modules
import qualified Data.Text as T
import Web.Slack.Message
import Web.Slack (Slack)
import System.Random (randomIO, randomRIO)
import Control.Monad.State (MonadIO (..))
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Default 
import qualified Data.ByteString.Lazy as BL (readFile)
import qualified Data.Vector as V
import Control.Concurrent.MVar

-- | Ambetsman type with fromJSON instance
data Ambetsman = Ambetsman (V.Vector T.Text) (V.Vector T.Text)

instance FromJSON Ambetsman where
  parseJSON (Object v) = Ambetsman <$> v .: "job" <*> v .: "surjob"
  parseJSON _          = fail "Badly formatted json :("

instance Default Ambetsman where
  def = Ambetsman (V.fromList ["idiot"]) (V.fromList [""])

-- | Loads the ambetsman
loadAmbetsman :: MVar Ambetsman -> FilePath -> IO () 
loadAmbetsman ambetsman dir = do
  let path = (dir ++ "/assets/ambetsman.json")
  oldAmbetsman <- takeMVar ambetsman
  ambets <- doesFileExist path >>= \existing -> if not existing
                                                then return Nothing
                                                else decode <$> BL.readFile path
  putMVar ambetsman $ fromMaybe oldAmbetsman ambets

-- | Generate a job 
jobname :: MVar Ambetsman -> Slack a T.Text
jobname ambetsman = liftIO (readMVar ambetsman) >>= randomJobFrom

randomJobFrom :: Ambetsman -> Slack a T.Text
randomJobFrom (Ambetsman job surjob) = liftIO $ do
  prefix <- (job    V.!) <$> randomRIO (0, length job - 1)
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
  | "vill jobba med" `T.isInfixOf` msg = do
      (Ambetsman job surjob) <- liftIO $ readMVar ambetsman
      let (who, _)      = T.breakOn "vill jobba med" msg
          (_, what)     = T.breakOnEnd "vill jobba med" msg
          (who', what') = (T.strip who, T.strip what)
          job'          = V.filter (what' `T.isInfixOf`) job
          surjob'       = V.filter (what' `T.isInfixOf`) surjob
          to            = case T.toLower who' of
                            "jag" -> "Du"
                            _     -> who'
          yourJob x     = sendMessage cid $ T.concat [to," borde jobba som ",x]
      preferSurjob <- liftIO randomIO
      case () of
        _ | all null [job', surjob'] ->
            sendMessage cid $ "Det finns inga såna jobb!"
          | null job' || preferSurjob ->
            yourJob =<< randomJobFrom (Ambetsman job surjob')
          | otherwise ->
            yourJob =<< randomJobFrom (Ambetsman job' surjob)
      return Next
  | "yrke" `T.isInfixOf` msg = do
      jn <- jobname ambetsman
      sendMessage cid $ T.concat ["Du borde jobba som ", jn]
      return Next
  | otherwise = return Next

