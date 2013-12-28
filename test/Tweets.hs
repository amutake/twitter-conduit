module Tweets where

import Test.Hspec

import Control.Applicative ((<$>))
import Control.Exception.Lifted (bracket)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.Random (randomRIO)

import Web.Twitter

runTweetsTests :: OAuth -> AccessToken -> AccessToken -> Spec
runTweetsTests oauth test1 test2 = do
    describe "statuses/update" $ do
        it "tweet" $ do
            text <- getRandomText
            status <- runTwitter oauth test1 $ withTweet text return
            statusText status `shouldBe` text

    describe "statuses/show" $ do
        it "show" $ do
            text <- getRandomText
            runTwitter oauth test1 $ withTweet text $ \status -> do
                status' <- showStatus (statusId status) Nothing Nothing Nothing
                liftIO $ status' `shouldBe` status

    describe "statuses/retweet" $ do
        it "retweet" $ do
            text <- getRandomText
            withTweetFrom oauth test1 text $ \status -> do
                status' <- runTwitter oauth test2 $ retweet (statusId status) Nothing
                statusId <$> statusRetweetedStatus status' `shouldBe` Just (statusId status)

    describe "statuses/retweets" $ do
        it "retweets" $ do
            text <- getRandomText
            withTweetFrom oauth test1 text $ \status -> do
                withRetweetFrom oauth test2 (statusId status) $ \status' -> do
                    statuses <- runTwitter oauth test1 $ retweets (statusId status) Nothing Nothing
                    map statusId statuses `shouldBe` [statusId status']

getRandomText :: IO T.Text
getRandomText = T.pack <$> replicateM 8 (randomRIO ('!', '~'))

withTweet :: T.Text -> (Status -> Twitter a) -> Twitter a
withTweet text = bracket
    (update text Nothing Nothing Nothing Nothing Nothing Nothing)
    (flip destroy Nothing . statusId)

withTweetFrom :: OAuth -> AccessToken -> T.Text -> (Status -> IO a) -> IO a
withTweetFrom oauth token text = bracket
    (runTwitter oauth token $ update text Nothing Nothing Nothing Nothing Nothing Nothing)
    (runTwitter oauth token . flip destroy Nothing . statusId)

withRetweetFrom :: OAuth -> AccessToken -> StatusId -> (Status -> IO a) -> IO a
withRetweetFrom oauth token sid = bracket
    (runTwitter oauth token $ retweet sid Nothing)
    (runTwitter oauth token . flip destroy Nothing . statusId)
