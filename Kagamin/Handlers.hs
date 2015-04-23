{-# LANGUAGE OverloadedStrings #-}
module Kagamin.Handlers where
import Control.Applicative
import qualified Data.Text as T
import Web.Slack
import Web.Slack.Message
import Control.Monad
import Control.Monad.State (liftIO)

-- Markov chain stuff
import DissociatedPress
import System.Random

-- Link log
import qualified Data.Set as S

import Kagamin.State
import Kagamin.SlackUtils
import Kagamin.TextUtils

-- | Handler called for *all* messages, targeted or not.
handleMsg :: KagaID -> Token -> ChannelId -> Submitter -> T.Text -> Slack s ()
handleMsg _kid _tok cid _from msg
  | "dricka te" `T.isInfixOf` (T.toLower msg) = do
    sendMessage cid "MOTHERFUCKING TEA!"
    sendMessage cid "https://www.youtube.com/watch?v=pdEcqSBx4J8"
  | otherwise = do
    return ()

-- | Handle mssages NOT specifically directed at Kagamin.
handleOtherMsg :: KagaID
               -> Token
               -> ChannelId
               -> Submitter
               -> T.Text
               -> Slack StateRef ()
handleOtherMsg _kid _tok _cid _from msg
  | Just url <- extractUrl msg = do
    updState $ \s ->
      s {stateLinks = S.insert (mkLinkMsg url msg) (stateLinks s)}
  | otherwise = do
    updState $ \s -> s {stateDict = updateDict (T.words msg) $ stateDict s}
    return ()

-- | Handle a message directed at Kagamin.
handleKagaMsg :: KagaID
              -> Token
              -> ChannelId
              -> Submitter
              -> T.Text
              -> Slack StateRef ()
handleKagaMsg kid tok cid from msg = do
  case stripLeadingTrailingMention kid msg of
    "suki" -> do
        from' <- submitterName from
        sendMessage cid $ stutter (T.concat [from', " no baka!!"])
    "citat" -> do
        quote <- randomSentence <$> getState stateDict <*> liftIO newStdGen
        sendMessage cid quote
    "skärp dig" -> do
        postImage tok cid "[Sad Kagamin]" "http://ekblad.cc/i/kagasad.jpg"
    "länktips" -> do
        links <- getState stateLinks
        when (S.size links > 0) $ do
          ix <- liftIO $ randomRIO (0, S.size links-1)
          sendMessage cid . linkMessage $ S.elemAt ix links
    msg'
      | "vad är" `T.isPrefixOf` msg' -> do
        let q = T.strip $ dropPrefix "vad är" $ dropSuffix "?" msg'
        quote <- ask q <$> getState stateDict <*> liftIO newStdGen
        sendMessage cid quote
    msg' -> do
      liftIO $ print msg'
      return ()
