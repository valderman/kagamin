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

import KagaInfo (kagaToken, kagaID) -- Slack API token + bot ID

kagaConfig :: SlackConfig
kagaConfig = SlackConfig {
    _slackApiToken = kagaToken
  }

main :: IO ()
main = runBot kagaConfig kagamin ()

-- | Kagamin's personality entry point.
kagamin :: SlackBot ()
kagamin (Message cid from msg _ _ _) = do
  liftIO $ print cid
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
handleOtherMsg :: ChannelId -> Submitter -> T.Text -> Slack s ()
handleOtherMsg _cid _from _msg
  | otherwise = do
    return ()

-- | Handle a message directed at Kagamin.
handleKagaMsg :: ChannelId -> Submitter -> T.Text -> Slack s ()
handleKagaMsg cid from msg = do
  case stripLeadingMention msg of
    "suki" -> do
      from' <- submitterName from
      sendMessage cid $ stutter (T.concat [from', " no baka!!"])
    "skärp dig" -> do
      postImage cid "[Sad Kagamin]" "http://ekblad.cc/i/kagasad.jpg"
    _ -> do
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

-- | Remove any leading mentions of Kagamin, including whitespace.
stripLeadingMention :: T.Text -> T.Text
stripLeadingMention s
  | any (`T.isPrefixOf` s') ["kagamin,", "kagamin:"] =
    T.dropWhile isSpace $ T.drop 8 s
  | kagaID `T.isPrefixOf` s =
    T.dropWhile isSpace
    . T.dropWhile (== ':')
    . T.dropWhile isSpace
    $ T.drop 12 s
  | otherwise =
    s
  where s' = T.map toLower s

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
