module Timelines where

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    statuses <- runTwitter oauth token $
        userTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    print statuses
