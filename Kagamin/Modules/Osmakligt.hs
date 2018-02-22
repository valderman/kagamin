{-# LANGUAGE OverloadedStrings, LambdaCase #-}
-- | Keeps track of the tastelessness ranking of the Slack team.
module Kagamin.Modules.Osmakligt (kagaTaste) where
import Kagamin.Modules
import qualified Data.Text as T
import Kagamin.TextUtils
import Web.Slack.Message
import Web.Slack.Types.Event (Submitter (..))
import Web.Slack.Types.Id (Id (..))
import Control.Monad.State (MonadIO (..))
import KagaInfo (kagaID)
import qualified Data.Map as Map
import Data.IORef
import Data.Function
import Data.List (sortBy)
import Data.Monoid

kagaTaste :: IO KagaModule
kagaTaste = do
  lastref <- newIORef ""
  rankingref <- newIORef Map.empty
  return $ defaultModule
    { kagaOtherHook = handleOtherMsg lastref rankingref
    , kagaMsgHook = handleKagaMsg lastref rankingref
    }

handleOtherMsg :: IORef T.Text -> IORef (Map.Map T.Text Int) -> MsgHook
handleOtherMsg lastref rankingref _cid (UserComment (Id from)) msg
  | "osmakligt" `T.isInfixOf` msg' = liftIO $ do
      lastuser <- readIORef lastref
      when (not $ T.null lastuser) $ do
        atomicModifyIORef rankingref $ \r -> pair () $ alter' r lastuser $ \case
          Just n -> Just (n+1)
          _      -> Just 1
      return Next
  | "nsfw" `T.isInfixOf` msg' = liftIO $ do
      atomicModifyIORef rankingref $ \r -> pair () $ alter' r from $ \case
        Just n -> Just (n+1)
        _      -> Just 1
      return Next
  | otherwise = liftIO $ do
      writeIORef lastref from
      return Next
  where
    msg' = T.toLower msg
    pair b a = (a, b)
    alter' m k f = Map.alter f k m
handleOtherMsg _ _ _ _ _ = return Next

handleKagaMsg :: IORef T.Text -> IORef (Map.Map T.Text Int) -> MsgHook
handleKagaMsg _lastref rankingref cid _from msg
  | "osmakligast" `T.isInfixOf` msg' = do
      rankings <- liftIO $ readIORef rankingref
      liftIO $ putStrLn $ "Osmakligast: " ++ show (mkRankings rankings)
      sendMessage cid (mkRankings rankings)
      return Next
  | otherwise =
      return Next
  where
    mkRankings
      = T.intercalate "\n"
      . zipWith mkRankingEntry [(1 :: Int) ..]
      . filter (not . T.null)
      . sortByTaste
    mkRankingEntry n user = T.pack (show n) <> ". <@" <> user <> ">"
    sortByTaste
      = map fst
      . reverse
      . sortBy (compare `on` snd)
      . Map.toList
    msg' = stripLeadingTrailingMention kagaID msg
