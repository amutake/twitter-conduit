module Update where

import qualified Data.Text.IO as T

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    cred <- readCredentialFromJsonFile "access_token.json"
    text <- T.getLine
    status <- runTwitter oauth cred $
        update text Nothing Nothing Nothing Nothing Nothing Nothing
    print status
