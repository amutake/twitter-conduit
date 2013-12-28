module SaveAuthFile where

import Control.Monad (unless)
import qualified Data.ByteString as BS

import Web.Twitter

main :: IO ()
main = do
    putStr "Do you have oauth_consumer.json file? [y/n] "
    yn <- getLine
    oauth <- case yn of
        "y" -> do
            putStr "Input the path to oauth_consumer.json: "
            path <- getLine
            readOAuthFromJsonFile path
        _ -> do
            putStr "Input consumer key: "
            key <- BS.getLine
            putStr "Input consumer secret: "
            secret <- BS.getLine
            return $ newOAuth key secret
    token <- authorizeIO oauth $ \url -> do
        putStr "Authorize URL: "
        putStrLn url
        putStr "Input PIN: "
        fmap read getLine
    unless (yn == "y") $ do
        putStr "Input save oauth_consumer path: "
        opath <- getLine
        saveOAuthToJsonFile opath oauth
    putStr "Input save access_token path: "
    apath <- getLine
    saveAccessTokenToJsonFile apath token
