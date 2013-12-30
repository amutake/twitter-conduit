module AllDelete where

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    runTwitter oauth token $ do
        me <- verifyCredentials Nothing Nothing
        yn <- liftIO $ do
            putStrLn $ "delete @" ++ T.unpack (userScreenName me) ++ "'s all tweets and direct messages. ok? [y/n]"
            getLine
        case yn of
            "y" -> liftIO (putStrLn "start.") >>
                   delStatus >> delDM >>
                   liftIO (putStrLn "done.")
            _ -> liftIO $ putStrLn "abort."
  where
    delStatus = do
        tl <- userTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        when (not . null $ tl) $ do
            forM_ tl $ \status -> destroy (statusId status) Nothing
            delStatus
    delDM = do
        dms <- sent Nothing Nothing Nothing Nothing Nothing
        when (not . null $ dms) $ do
            forM_ dms $ flip destroyDirectMessage Nothing . directMessageId
            delDM
