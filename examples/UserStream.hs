module UserStream where

import Control.Monad.IO.Class (liftIO)
import Data.Conduit

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    runTwitter oauth token $ do
        source <- user
        source $$+- awaitForever (liftIO . print)
