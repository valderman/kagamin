{-# LANGUAGE CPP, OverloadedStrings #-}
-- | Misc. Slack-related utilities.
module Kagamin.SlackUtils where
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Text as T
import Network.Curl (curlGet)
import Network.HTTP (urlEncode)
import Unsafe.Coerce -- OH NOES
import Web.Slack

type Token = String
type KagaID = T.Text

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
postImage :: Token -> ChannelId -> String -> String -> Slack s ()
postImage token cid fallback img = liftIO $ do
    curlGet ("https://slack.com/api/chat.postMessage?" ++ url) [] 
  where
    url = intercalate "&" $ [
              "token=" ++ token,
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
