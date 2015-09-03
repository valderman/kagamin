{-# LANGUAGE OverloadedStrings #-}
-- | Keeps track of all links posted to channels where Kagamin is active,
--   and responds to requests for link tips.
module Kagamin.Modules.Links (kagaLinks) where
import Kagamin.Modules
import qualified Data.Set as S
import Web.Slack.Message
import Kagamin.TextUtils
import Control.Monad.State
import System.Random
import KagaInfo (kagaID)
import Control.Concurrent.MVar
import qualified Data.Binary as B
import qualified Data.Text as T
import Data.Hashable
import Data.Text.Binary ()
import Control.Exception
import qualified Data.ByteString.Lazy as BS

kagaLinks :: IO KagaModule
kagaLinks = do
  links <- newMVar S.empty
  return $ defaultModule {
      kagaMsgHook   = handleKagaMsg links,
      kagaOtherHook = handleOtherMsg links,
      kagaSaveHook  = save links,
      kagaLoadHook  = load links
    }

load :: MVar (S.Set LinkMessage) -> FilePath -> IO ()
load ls dir = do
  oldlinks <- takeMVar ls
  elinks <- try $! BS.readFile (dir ++ "/kagamin.links") >>= evaluate . B.decode
  let links = case elinks of
                Right l -> l
                Left e  -> (e :: SomeException) `seq` oldlinks
  putMVar ls links

save :: MVar (S.Set LinkMessage) -> FilePath -> IO ()
save ls dir = withMVar ls $ \links -> do
  BS.writeFile (dir ++ "/kagamin.links") (B.encode $ links)

handleKagaMsg :: MVar (S.Set LinkMessage) -> MsgHook
handleKagaMsg ls cid _from msg
  | msg' == "länktips" = do
    links <- liftIO $ withMVar ls return
    when (S.size links > 0) $ do
      ix <- liftIO $ randomRIO (0, S.size links-1)
      sendMessage cid . linkMessage $ S.elemAt ix links
    return Next
  | otherwise = do
    return Next
  where
    msg' = stripLeadingTrailingMention kagaID msg

handleOtherMsg :: MVar (S.Set LinkMessage) -> MsgHook
handleOtherMsg links _cid _from msg = do
  case extractUrl msg of
    Just url -> liftIO $ modifyMVar links $ \ls ->
                  return (S.insert (mkLinkMsg url msg) ls, Next)
    _        -> return Next

mkLinkMsg :: T.Text -> T.Text -> LinkMessage
mkLinkMsg url s = LinkMessage {
    linkHash = hash url,
    linkMessage = unCrocodileUrls s
  }

instance B.Binary LinkMessage where
  put (LinkMessage h m) = B.put h >> B.put m
  get = LinkMessage <$> B.get <*> B.get

instance Eq LinkMessage where
  (LinkMessage h1 _) == (LinkMessage h2 _) = h1 == h2

instance Ord LinkMessage where
  compare (LinkMessage h1 _) (LinkMessage h2 _) = compare h1 h2

data LinkMessage = LinkMessage {
    linkHash    :: !Int,
    linkMessage :: !T.Text
  }
