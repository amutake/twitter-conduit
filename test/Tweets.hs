module Tweets where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Test.Hspec
import Web.Twitter

import Resources
import Util

runTweetsTests :: OAuth -> AccessToken -> AccessToken -> Spec
runTweetsTests oauth token1 token2 = do
    describe "statuses/update" $ do
        it "update" $ do
            text <- getRandomText
            status <- runTwitter oauth token1 $ withTweet text return
            statusText status `shouldBe` text

    describe "statuses/show" $ do
        it "show" $ do
            text <- getRandomText
            runTwitter oauth token1 $ withTweet text $ \status -> do
                status' <- showStatus (statusId status) Nothing Nothing Nothing
                liftIO $ status' `shouldBe` status

    describe "statuses/retweet" $ do
        it "retweet" $ do
            text <- getRandomText
            withTweetFrom oauth token1 text $ \status -> do
                status' <- runTwitter oauth token2 $ retweet (statusId status) Nothing
                statusId <$> statusRetweetedStatus status' `shouldBe` Just (statusId status)

    describe "statuses/retweets" $ do
        it "retweets" $ do
            text <- getRandomText
            withTweetFrom oauth token1 text $ \status -> do
                withRetweetFrom oauth token2 (statusId status) $ \status' -> do
                    statuses <- runTwitter oauth token1 $ retweets (statusId status) Nothing Nothing
                    map statusId statuses `shouldBe` [statusId status']

    describe "statuses/retweeters/ids" $ do
        it "retweeters" $ do
            text <- getRandomText
            withTweetFrom oauth token1 text $ \status -> do
                withRetweetFrom oauth token2 (statusId status) $ const $ do
                    user2 <- runTwitter oauth token2 $ verifyCredentials Nothing Nothing
                    rts <- runTwitter oauth token1 $ retweeters (statusId status) Nothing Nothing
                    idsIds rts `shouldBe` [userId user2]
