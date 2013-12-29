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
            putStrLn $ "delete @" ++ T.unpack (userName me) ++ "'s all tweets. ok? [y/n]"
            getLine
        case yn of
            "y" -> liftIO (putStrLn "start.") >> loop >> liftIO (putStrLn "done.")
            _ -> liftIO $ putStrLn "abort."
  where
    loop = do
        tl <- userTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        when (not . null $ tl) $
            forM_ tl $ \status -> destroy (statusId status) Nothing