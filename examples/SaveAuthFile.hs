module SaveAuthFile where

import qualified Data.ByteString as BS

import Web.Twitter

main :: IO ()
main = do
    putStr "Input consumer key: "
    key <- BS.getLine
    putStr "Input consumer secret: "
    secret <- BS.getLine
    let oauth = newOAuth key secret
    token <- authorizeIO oauth $ \url -> do
        putStr "Authorize URL: "
        putStrLn url
        putStr "Input PIN: "
        fmap read getLine
    putStrLn "Save to oauth_consumer.json and access_token.json"
    saveOAuthToJsonFile "oauth_consumer.json" oauth
    saveAccessTokenToJsonFile "access_token.json" token
