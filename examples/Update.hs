module Update where

import qualified Data.Text.IO as T

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    putStr "Tweet: "
    text <- T.getLine
    status <- runTwitter oauth token $
        update text Nothing Nothing Nothing Nothing Nothing Nothing
    print status
