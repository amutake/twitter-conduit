{-# LANGUAGE OverloadedStrings #-}

module Example where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.IO as T
import Network.HTTP.Types

import Web.Twitter

main :: IO ()
main = do
    let query = [("hoge", Just "fuga"), ("none", Nothing)]
    BC.putStrLn $ renderQuery True query
    BC.putStrLn $ renderQuery False query
    putStr "Input consumer key: "
    key <- BS.getLine
    putStr "Input consumer secret: "
    secret <- BS.getLine
    let oauth = newOAuth key secret
    cred <- authorizeIO oauth $ \url -> do
        putStr "Access this URL: "
        putStrLn url
        putStr "Input PIN: "
        fmap read getLine
    putStr "Tweet: "
    text <- T.getLine
    status <- runTwitter oauth cred $ do
        update text Nothing Nothing Nothing Nothing Nothing Nothing
    print $ statusId status
