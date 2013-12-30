module DirectMessages where

import Control.Exception.Lifted
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as T

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token1 <- readAccessTokenFromJsonFile "haskell_test_1.json"
    token2 <- readAccessTokenFromJsonFile "haskell_test_2.json"
    test2 <- runTwitter oauth token2 $ verifyCredentials Nothing Nothing
    runTwitter oauth token1 $ do
        liftIO $ putStrLn "---- new ----"
        liftIO $ putStr "Text: "
        text <- liftIO $ T.getLine
        dm <- newDirectMessage (Left $ userId test2) text
        liftIO $ print dm

        let did = directMessageId dm

        liftIO $ putStrLn "---- show ----"
        sres <- showDirectMessage did
        liftIO $ print sres

        liftIO $ putStrLn "---- sent ----"
        dms <- sent Nothing Nothing Nothing Nothing Nothing
        liftIO $ print dms

        liftIO $ putStrLn "---- direct_messages ----"
        dms' <- directMessages Nothing Nothing Nothing Nothing Nothing
        liftIO $ print dms'

        liftIO $ putStrLn "---- destroy ----"
        dm' <- destroyDirectMessage did Nothing
        liftIO $ print dm'
