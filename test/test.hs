import Test.Hspec
import Web.Twitter

import Tweets
import Timelines

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token1 <- readAccessTokenFromJsonFile "haskell_test_1.json"
    token2 <- readAccessTokenFromJsonFile "haskell_test_2.json"

    hspec $ do
        runTweetsTests oauth token1 token2
        runTimelinesTests oauth token1 token2
