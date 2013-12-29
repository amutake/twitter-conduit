module Tweets where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    runTwitter oauth token $ do
        liftIO $ putStrLn "---- update ----"
        liftIO $ putStr "Tweet: "
        text <- liftIO $ T.getLine
        ures <- update text Nothing Nothing Nothing Nothing Nothing Nothing
        liftIO $ print ures

        let sid = statusId ures

        liftIO $ putStrLn "---- show ----"
        sres <- showStatus sid Nothing Nothing Nothing
        liftIO $ print sres

        liftIO $ putStrLn "---- retweet ----"
        rres <- retweet sid Nothing
        liftIO $ print rres

        liftIO $ putStrLn "---- retweets ----"
        rsres <- retweets sid Nothing Nothing
        liftIO $ print rsres

        liftIO $ putStrLn "---- destroy ----"
        dres <- destroy sid Nothing
        liftIO $ print dres