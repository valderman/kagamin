{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where
import Web.Slack hiding (lines)
import Web.Slack.Message
import qualified Data.Text as T
import Data.Char
import Control.Applicative
import Control.Monad.State
import Network.Curl (curlGet)
import Data.List
import Network.HTTP (urlEncode)
import Unsafe.Coerce -- OH NOES

-- Markov chain stuff
import Data.Text.Binary ()
import Data.IORef
import DissociatedPress
import System.Random
import System.Posix.Signals
import System.Exit
import Control.Exception
import Control.Concurrent (myThreadId)

-- Link log
import qualified Data.Binary as B
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS

import Kagamin.State
import Kagamin.TextUtils

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

-- | Kagamin's personality entry point.
kagamin :: SlackBot StateRef
kagamin (Message cid from msg _ _ _) = do
  if (toKagamin kagaID msg)
    then handleKagaMsg cid from msg
    else handleOtherMsg cid from msg
  handleMsg cid from msg
kagamin _ = do
  return ()

-- | Handle mssages NOT specifically directed at Kagamin.
handleMsg :: ChannelId -> Submitter -> T.Text -> Slack s ()
handleMsg cid _from msg
  | "dricka te" `T.isInfixOf` (T.map toLower msg) = do
    sendMessage cid "MOTHERFUCKING TEA!"
    sendMessage cid "https://www.youtube.com/watch?v=pdEcqSBx4J8"
  | otherwise = do
    return ()

-- | Handle mssages NOT specifically directed at Kagamin.
handleOtherMsg :: ChannelId -> Submitter -> T.Text -> Slack StateRef ()
handleOtherMsg _cid _from msg
  | Just url <- extractUrl msg = do
    updState $ \s ->
      s {stateLinks = S.insert (mkLinkMsg url msg) (stateLinks s)}
  | otherwise = do
    updState $ \s -> s {stateDict = updateDict (T.words msg) $ stateDict s}
    return ()

-- | Handle a message directed at Kagamin.
handleKagaMsg :: ChannelId -> Submitter -> T.Text -> Slack StateRef ()
handleKagaMsg cid from msg = do
  case stripLeadingTrailingMention kagaID msg of
    "suki" -> do
        from' <- submitterName from
        sendMessage cid $ stutter (T.concat [from', " no baka!!"])
    "citat" -> do
        quote <- randomSentence <$> getState stateDict <*> liftIO newStdGen
        sendMessage cid quote
    "skärp dig" -> do
        postImage cid "[Sad Kagamin]" "http://ekblad.cc/i/kagasad.jpg"
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

-- | Get the name of the submitter, as seen at the start of the session.
--   System messages come from "system", messages with no discernible sender
--   come from "anon".
submitterName :: Submitter -> Slack s T.Text
submitterName (UserComment uid) = do
  us <- _slackUsers . _session <$> get
  case filter ((uid ==) . _userId) us of
    [u] -> return $ _userName u
    _   -> return "anon"
submitterName (BotComment bid)  = do
  us <- _slackBots . _session <$> get
  case filter ((bid ==) . _botId) us of
    [u] -> return $ _botName u
    _   -> return "anon"
submitterName System = do
  return "system"

-- | Post an image to a channel. Since the RTM API doesn't support attachments,
--   this is done using the REST API via Curl.
postImage :: ChannelId -> String -> String -> Slack s ()
postImage cid fallback img = liftIO $ do
    curlGet ("https://slack.com/api/chat.postMessage?" ++ url) [] 
  where
    url = intercalate "&" $ [
              "token=" ++ kagaToken,
              "channel=" ++ cidString cid,
              "username=kagamin",
              "as_user=true",
              "attachments=" ++ att
            ]
    att = urlEncode $ concat [
        "[{\"fallback\":\"",  fallback, "\",",
          "\"image_url\":\"", img, "\"}]"
      ]

--  | Horribly evil, unsafe way to extract channel IDs, since slack-api doesn't
--    appear to let you do that.
cidString :: ChannelId -> String
cidString = T.unpack . unsafeCoerce
