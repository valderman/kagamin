{-# LANGUAGE OverloadedStrings #-}
-- | Performs die rolls on request.
module Kagamin.Modules.Dice (kagaDice) where
import Kagamin.Modules
import qualified Data.Text as T
import Kagamin.TextUtils
import Web.Slack.Message
import Text.Read
import System.Random
import Control.Monad.State (MonadIO (..))
import Data.Maybe (catMaybes)
import KagaInfo (kagaID)

kagaDice :: IO KagaModule
kagaDice = return $ defaultModule {kagaMsgHook = handleKagaMsg}

rolls :: [T.Text]
rolls = ["rulla", "slå"]

handleKagaMsg :: MsgHook
handleKagaMsg cid _from msg
  | any (`T.isPrefixOf` msg') rolls = do
    let die = T.strip $ dropPrefix "rulla" msg'
    case catMaybes $ map (readMaybe . T.unpack) $ T.splitOn "d" die of
       [a,b] -> do
         num <- liftIO $ sum <$> sequence (replicate a $ randomRIO (1, b))
         sendMessage cid $ T.concat ["Du rullade ", T.pack $ show (num :: Int)]
       _     -> do
         sendMessage cid (stutter "Det där är ingen tärning, baka!!")
    return Next
  | otherwise = do
    return Next
  where
    msg' = stripLeadingTrailingMention kagaID msg
