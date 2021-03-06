module Users where

import Control.Monad.IO.Class (liftIO)

import Web.Twitter

main :: IO ()
main = do
    oauth <- readOAuthFromJsonFile "oauth_consumer.json"
    token <- readAccessTokenFromJsonFile "access_token.json"
    runTwitter oauth token $ do
        liftIO $ putStrLn "---- verify_credentials ----"
        me <- verifyCredentials Nothing Nothing
        liftIO $ print me

        liftIO $ putStrLn "---- GET settings ----"
        ss <- getAccountSettings
        liftIO $ print ss

        liftIO $ putStrLn "---- POST settings ---"
        ss' <- updateAccountSettings (Just 1) (Just True) (Just 22) (Just 6) (Just "Tokyo") (Just "ja")
        liftIO $ print ss'
