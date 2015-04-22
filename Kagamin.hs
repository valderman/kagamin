{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Slack
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

import KagaInfo (kagaToken, kagaID) -- Slack API token + bot ID

type DictRef = IORef (Dictionary T.Text)

kagaConfig :: SlackConfig
kagaConfig = SlackConfig {
    _slackApiToken = kagaToken
  }

main :: IO ()
main = do
    d <- load "kagamin.dict"
    dictref <- newIORef $! maybe defDict id d
    t <- myThreadId
    void $ installHandler sigINT (Catch $ intHandler t dictref) Nothing
    runBot kagaConfig kagamin dictref
  where
    intHandler t r = do
      readIORef r >>= store "kagamin.dict"
      putStrLn "Bye!"
      throwTo t ExitSuccess

-- | Kagamin's personality entry point.
kagamin :: SlackBot DictRef
kagamin (Message cid from msg _ _ _) = do
  if (toKagamin msg)
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
handleOtherMsg :: ChannelId -> Submitter -> T.Text -> Slack DictRef ()
handleOtherMsg _cid _from msg
  | otherwise = do
    r <- _userState <$> get
    liftIO $ atomicModifyIORef' r $ \d -> (updateDict (T.words msg) d, ())
    return ()

-- | Handle a message directed at Kagamin.
handleKagaMsg :: ChannelId -> Submitter -> T.Text -> Slack DictRef ()
handleKagaMsg cid from msg = do
  case stripLeadingTrailingMention msg of
    "suki" -> do
        from' <- submitterName from
        sendMessage cid $ stutter (T.concat [from', " no baka!!"])
    "citat" -> do
        r <- _userState <$> get
        quote <- liftIO $ randomSentence <$> readIORef r <*> newStdGen
        sendMessage cid quote
    "skärp dig" -> do
        postImage cid "[Sad Kagamin]" "http://ekblad.cc/i/kagasad.jpg"
    msg'
      | "vad är" `T.isPrefixOf` msg' -> do
        r <- _userState <$> get
        let noPrefix = maybe msg' id $ T.stripPrefix "vad är" msg'
            noSuffix = maybe noPrefix id $ T.stripSuffix "?" noPrefix
            q        = T.strip noSuffix
        quote <- liftIO $ ask q <$> readIORef r <*> newStdGen
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

-- | Was the given message intended for Kagamin?
toKagamin :: T.Text -> Bool
toKagamin s =
    or [ any (`T.isPrefixOf` s') ["kagamin,", "kagamin:"]
       , kagaID `T.isInfixOf` s]
  where s' = T.map toLower s

-- | "baka" -> "b- baka"
stutter :: T.Text -> T.Text
stutter t = T.concat [T.head t `T.cons` "- ",
                      T.singleton (toLower $ T.head t),
                      T.tail t]

-- | Replace all occurrences of @from@ with @to@ in @s@.
replace :: T.Text -> T.Text -> T.Text -> T.Text
replace from to s =
  case T.breakOn from s of
    (_, "") -> s
    (pre, rest) -> T.concat [pre, to, replace from to (T.drop len rest)]
  where
    len = T.length from

-- | Remove any leading or trailing mentions of Kagamin, including whitespace.
stripLeadingTrailingMention :: T.Text -> T.Text
stripLeadingTrailingMention s
  | any (`T.isPrefixOf` s') ["kagamin,", "kagamin:"] =
    T.dropWhile isSpace $ T.drop 8 s
  | otherwise =
    dropPrefix kagaID $ dropSuffix kagaID $ s
  where s' = T.map toLower s

dropPrefix :: T.Text -> T.Text -> T.Text
dropPrefix p s = maybe s T.strip $ T.stripPrefix p s

dropSuffix :: T.Text -> T.Text -> T.Text
dropSuffix p s = maybe s T.strip $ T.stripSuffix p s

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
