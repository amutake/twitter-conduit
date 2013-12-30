module Stream where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import qualified Data.Text as T

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    runTwitter oauth token $ do
        source <- sample Nothing Nothing
        source $$+- awaitForever (liftIO . pp)
  where
    pp (UserStreamFriends uids) = putStrLn $ "friends: " ++ show uids
    pp (UserStreamEvent ev) = putStrLn $ "event: " ++ show (eventEvent ev)
    pp (UserStreamStatusDeletion sd) = putStrLn $ "status deletion: " ++ show (statusDeletionId sd)
    pp (UserStreamDirectMessage dm) = do
        putStrLn "direct message:"
        putStrLn $ "  from: @" ++ show (directMessageSenderScreenName dm)
        putStrLn $ "  to: @" ++ show (directMessageRecipientScreeName dm)
        putStrLn $ "  text: " ++ T.unpack (directMessageText dm)
    pp (UserStreamStatus st) = do
        putStrLn "tweet:"
        putStrLn $ "  from: @" ++ T.unpack (userScreenName $ statusUser st)
        putStrLn $ "  text: " ++ T.unpack (statusText st)
