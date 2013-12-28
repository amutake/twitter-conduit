module Timelines where

import Web.Twitter

main :: IO ()
main = do
    oauth <- getOAuthFromJsonFile "oauth_consumer.json"
    cred <- getCredentialFromJsonFile "access_token.json"
    statuses <- runTwitter oauth cred $
        mentionsTimeline (Just 20) Nothing Nothing Nothing Nothing Nothing
    print statuses
