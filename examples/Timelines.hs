module Timelines where

import Control.Monad.IO.Class (liftIO)

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    runTwitter oauth token $ do
        liftIO $ putStrLn "---- mentions_timeline ----"
        mtl <- mentionsTimeline Nothing Nothing Nothing Nothing Nothing Nothing
        liftIO $ print mtl

        liftIO $ putStrLn "---- user_timeline ----"
        utl <- userTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        liftIO $ print utl

        liftIO $ putStrLn "---- home_timeline ----"
        htl <- homeTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        liftIO $ print htl

        liftIO $ putStrLn "---- retweets_of_me ----"
        rtl <- retweetsOfMe Nothing Nothing Nothing Nothing Nothing Nothing
        liftIO $ print rtl
