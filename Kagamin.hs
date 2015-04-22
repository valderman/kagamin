{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Slack
import Web.Slack.Message
import qualified Data.Text as T
import Data.Char
import Control.Applicative
import Control.Monad.State

import KagaInfo -- Slack API token + bot ID

kagaConfig :: SlackConfig
kagaConfig = SlackConfig {
    _slackApiToken = kagaToken
  }

main = runBot kagaConfig kagamin ()

-- | Kagamin's personality entry point.
kagamin :: SlackBot ()
kagamin (Message cid from msg _ _ _) | toKagamin msg = do
  handleMsg cid from msg
kagamin _ = do
  return ()

-- | Handle a message directed at Kagamin.
handleMsg :: ChannelId -> Submitter -> T.Text -> Slack () ()
handleMsg cid from msg = do
  case stripLeadingMention msg of
    "suki" -> do
      from' <- submitterName from
      sendMessage cid $ stutter (T.concat [from', " no baka!!"])
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
