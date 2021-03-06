{-# LANGUAGE OverloadedStrings #-}

module Timelines where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Test.Hspec
import Web.Twitter

import Resources
import Util

runTimelinesTests :: OAuth -> AccessToken -> AccessToken -> Spec
runTimelinesTests oauth token1 token2 = do
    describe "statuses/mentions_timeline" $ do
        it "mentions_timeline" $ do
            text <- getRandomText
            test2 <- runTwitter oauth token2 $ verifyCredentials Nothing Nothing
            let text' = "@" <> userScreenName test2 <> " " <> text
            withTweetFrom oauth token1 text' $ \status -> do
                mentions <- runTwitter oauth token2 $
                    mentionsTimeline Nothing Nothing Nothing Nothing Nothing Nothing
                map statusId mentions `shouldContain` [statusId status]

    describe "statuses/user_timeline" $ do
        it "user_timeline" $ do
            text <- getRandomText
            runTwitter oauth token1 $ withTweet text $ \status -> do
                tl <- userTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                liftIO $ map statusId tl `shouldContain` [statusId status]

    describe "statuses/home_timeline" $ do
        it "home_timeline" $ do
            text <- getRandomText
            withTweetFrom oauth token2 text $ \status -> do
                tl <- runTwitter oauth token1 $
                    homeTimeline Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                map statusId tl `shouldContain` [statusId status]

    describe "statuses/retweets_of_me" $ do
        it "retweets_of_me" $ do
            text <- getRandomText
            withTweetFrom oauth token1 text $ \status -> do
                withRetweetFrom oauth token2 (statusId status) $ const $ do
                    rts <- runTwitter oauth token1 $
                        retweetsOfMe Nothing Nothing Nothing Nothing Nothing Nothing
                    map statusId rts `shouldContain` [statusId status]
