import Test.Hspec

import Web.Twitter

import Tweets

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    test1 <- readAccessTokenFromJsonFile "haskell_test_1.json"
    test2 <- readAccessTokenFromJsonFile "haskell_test_2.json"

    hspec $ do
        runTweetsTests oauth test1 test2
