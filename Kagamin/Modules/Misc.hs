{-# LANGUAGE OverloadedStrings #-}
-- | Misc. responses.
module Kagamin.Modules.Misc (kagaMisc) where
import Kagamin.Modules
import qualified Data.Text as T
import Kagamin.TextUtils
import Kagamin.SlackUtils
import Web.Slack.Message
import KagaInfo

kagaMisc :: IO KagaModule
kagaMisc = return $ defaultModule {
    kagaMsgHook   = handleKagaMsg,
    kagaAllHook   = handleMsg
  }

handleKagaMsg :: MsgHook
handleKagaMsg cid from msg = do
  case stripLeadingTrailingMention kagaID msg of
    "suki" -> do
        from' <- submitterName from
        sendMessage cid $ stutter (T.concat [from', " no baka!!"])
    "skärp dig" -> do
        postImage kagaToken cid "[Sad Kagamin]" "http://ekblad.cc/i/kagasad.jpg"
    _ -> do
        return ()
  return Next

handleMsg :: MsgHook
handleMsg cid _from msg
  | any (`T.isInfixOf` (T.toLower msg)) teatriggers = do
    sendMessage cid "MOTHERFUCKING TEA!"
    sendMessage cid "https://www.youtube.com/watch?v=pdEcqSBx4J8"
    return Next
  | otherwise = do
    return Next
  where
    teaverbs :: [T.Text]
    teaverbs = ["dricka", "drack", "dricker", "druckit", "gillar"]
    teatriggers = map (`T.append` " te") teaverbs
