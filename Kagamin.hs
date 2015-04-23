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
import Data.Hashable
import qualified Data.ByteString.Lazy as BS

import KagaInfo (kagaToken, kagaID) -- Slack API token + bot ID

type StateRef = IORef KagaState
data LinkMessage = LinkMessage {
    linkHash    :: !Int,
    linkMessage :: !T.Text
  }

instance B.Binary LinkMessage where
  put (LinkMessage h m) = B.put h >> B.put m
  get = LinkMessage <$> B.get <*> B.get

mkLinkMsg :: T.Text -> T.Text -> LinkMessage
mkLinkMsg url s = LinkMessage (hash url) (unCrocodileUrls s)

instance Eq LinkMessage where
  (LinkMessage h1 _) == (LinkMessage h2 _) = h1 == h2

instance Ord LinkMessage where
  compare (LinkMessage h1 _) (LinkMessage h2 _) = compare h1 h2

data KagaState = KagaState {
    stateDict  :: !(Dictionary T.Text),
    stateLinks :: !(S.Set LinkMessage)
  }

getState :: (st -> a) -> Slack (IORef st) a
getState f = get >>= fmap f . liftIO . readIORef . _userState

updState :: (st -> st) -> Slack (IORef st) ()
updState f = get >>= liftIO . flip atomicModifyIORef' ((,()) . f) . _userState

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
handleOtherMsg :: ChannelId -> Submitter -> T.Text -> Slack StateRef ()
handleOtherMsg _cid _from msg
  | Just url <- extractUrl msg = do
    updState $ \s ->
      s {stateLinks = S.insert (mkLinkMsg url msg) (stateLinks s)}
  | otherwise = do
    updState $ \s -> s {stateDict = updateDict (T.words msg) $ stateDict s}
    return ()

-- | Remove the @<>@ surrounding URLs.
unCrocodileUrls :: T.Text -> T.Text
unCrocodileUrls s = maybe s T.concat $ go s
  where
    go str = do
      (pre, tmp) <- breakOnEither ["<http://", "<https://"] str
      (url, suf) <- breakOnEither [">"] (T.drop 1 tmp)
      let suf' = T.drop 1 suf
      return $ maybe [pre,url,suf'] ([pre,url]++) $ go suf'

-- | Extract the first URL from a message, if any.
extractUrl :: T.Text -> Maybe T.Text
extractUrl s = do
  (_, suf) <- breakOnEither ["<http://", "<https://"] s
  case T.breakOn ">" (T.drop 1 suf) of
    (_, "")  -> Nothing
    (url, _) -> Just url

breakOnEither :: [T.Text] -> T.Text -> Maybe (T.Text, T.Text)
breakOnEither (needle:needles) str =
  case T.breakOn needle str of
    (_, "") -> breakOnEither needles str
    match   -> Just match
breakOnEither [] _ =
  Nothing

-- | Handle a message directed at Kagamin.
handleKagaMsg :: ChannelId -> Submitter -> T.Text -> Slack StateRef ()
handleKagaMsg cid from msg = do
  case stripLeadingTrailingMention msg of
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
