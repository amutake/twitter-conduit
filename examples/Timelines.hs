module Timelines where

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    cred <- readCredentialFromJsonFile "access_token.json"
    statuses <- runTwitter oauth cred $
        mentionsTimeline (Just 20) Nothing Nothing Nothing Nothing Nothing
    print statuses
