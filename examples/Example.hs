{-# LANGUAGE OverloadedStrings #-}

module Example where

import qualified Data.ByteString as BS
import qualified Data.Text.IO as T

import Web.Twitter

main :: IO ()
main = do
    putStr "Input consumer key: "
    key <- BS.getLine
    putStr "Input consumer secret: "
    secret <- BS.getLine
    let oauth = newOAuth key secret
    cred <- getCredentialIO oauth $ \url -> do
        putStr "Access this URL: "
        putStrLn url
        putStr "Input PIN: "
        fmap read getLine
    putStr "Tweet: "
    text <- T.getLine
    status <- runTwitter oauth cred $ do
        update text Nothing Nothing Nothing Nothing Nothing Nothing
    print $ statusId status
