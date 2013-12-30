module DirectMessages where

import Control.Monad.IO.Class (liftIO)
import Test.Hspec
import Web.Twitter

import Resources
import Util

runDirectMessagesTests :: OAuth -> AccessToken -> AccessToken -> Spec
runDirectMessagesTests oauth token1 token2 = do
    describe "direct_message/{new,destroy}" $ it "" $ do
        text <- getRandomText
        test2 <- getMe oauth token2
        dm <- runTwitter oauth token1 $ withDirectMessage (userId test2) text return
        dms <- runTwitter oauth token2 $ directMessages Nothing Nothing Nothing Nothing Nothing
        map directMessageId dms `shouldSatisfy` all (/= directMessageId dm)

    describe "direct_messages" $ it "" $ do
        text <- getRandomText
        test1 <- getMe oauth token1
        withDirectMessageFrom oauth token2 (userId test1) text $ \dm -> do
            dms <- runTwitter oauth token1 $ directMessages Nothing Nothing Nothing Nothing Nothing
            map directMessageId dms `shouldContain` [directMessageId dm]

    describe "direct_messages/sent" $ it "" $ do
        text <- getRandomText
        test2 <- getMe oauth token2
        runTwitter oauth token1 $ withDirectMessage (userId test2) text $ \dm -> do
            dms <- sent Nothing Nothing Nothing Nothing Nothing
            liftIO $ dms `shouldContain` [dm]

    describe "direct_message/show" $ it "" $ do
        text <- getRandomText
        test2 <- getMe oauth token2
        runTwitter oauth token1 $ withDirectMessage (userId test2) text $ \dm -> do
            dm' <- showDirectMessage (directMessageId dm)
            liftIO $ dm' `shouldBe` dm
