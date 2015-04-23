{-# LANGUAGE TupleSections #-}
-- | Types and functions for working with Kagamin's state.
module Kagamin.State where
import Web.Slack
import Control.Applicative
import Control.Monad.State
import Control.Exception (try, evaluate, SomeException)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import Data.Text.Binary ()
import Data.IORef
import Data.Hashable
import DissociatedPress (Dictionary, load, store, defDict)
import Kagamin.TextUtils

type StateRef = IORef KagaState

data LinkMessage = LinkMessage {
    linkHash    :: !Int,
    linkMessage :: !T.Text
  }

data KagaState = KagaState {
    stateDict  :: !(Dictionary T.Text),
    stateLinks :: !(S.Set LinkMessage)
  }

instance B.Binary LinkMessage where
  put (LinkMessage h m) = B.put h >> B.put m
  get = LinkMessage <$> B.get <*> B.get

instance Eq LinkMessage where
  (LinkMessage h1 _) == (LinkMessage h2 _) = h1 == h2

instance Ord LinkMessage where
  compare (LinkMessage h1 _) (LinkMessage h2 _) = compare h1 h2

mkLinkMsg :: T.Text -> T.Text -> LinkMessage
mkLinkMsg url s = LinkMessage (hash url) (unCrocodileUrls s)

getState :: (st -> a) -> Slack (IORef st) a
getState f = get >>= fmap f . liftIO . readIORef . _userState

updState :: (st -> st) -> Slack (IORef st) ()
updState f = get >>= liftIO . flip atomicModifyIORef' ((,()) . f) . _userState

-- | Load Kagamin state from @prefix.{dict,links}@
readState :: FilePath -> IO StateRef
readState prefix = do
  d <- load $ prefix ++ ".dict"
  elinks <- try $! BS.readFile (prefix ++ ".links") >>= evaluate . B.decode
  let links = case elinks of
                Right l -> l
                Left e  -> (e :: SomeException) `seq` S.empty
  newIORef $! KagaState {
      stateDict  = maybe defDict id d,
      stateLinks = links
    }

-- | Write Kagamin state to @prefix.{dict,links}@.
writeState :: FilePath -> StateRef -> IO ()
writeState prefix r = do
  st <- readIORef r
  store (prefix ++ ".dict") (stateDict st)
  BS.writeFile (prefix ++ ".links") (B.encode $ stateLinks st)
