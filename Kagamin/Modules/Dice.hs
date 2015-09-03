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

handleKagaMsg :: MsgHook
handleKagaMsg cid _from msg
  | "rulla" `T.isPrefixOf` msg' = do
    let die = T.strip $ dropPrefix "rulla" msg'
        rm = readMaybe :: String -> Maybe Integer 
    case catMaybes $ map (rm . T.unpack) $ T.splitOn (T.pack "d") die of
       [a,b] -> do
         num <- liftIO $ randomRIO (1, a*b)
         sendMessage cid $ T.concat ["Du rullade ", T.pack $ show num]
       _     -> do
         sendMessage cid (stutter "Det där är ingen tärning, baka!!")
    return Next
  | otherwise = do
    return Next
  where
    msg' = stripLeadingTrailingMention kagaID msg
