module Update where

import qualified Data.Text.IO as T

import Web.Twitter

main :: IO ()
main = do
    oauth <- getOAuthFromJsonFile "oauth_consumer.json"
    cred <- getCredentialFromJsonFile "access_token.json"
    text <- T.getLine
    status <- runTwitter oauth cred $
        update text Nothing Nothing Nothing Nothing Nothing Nothing
    print status
