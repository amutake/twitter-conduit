{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Web.Twitter.Auth
    ( OAuth
    , Credential
    , newOAuth
    , newCredential
    , authorize
    , authorizeIO
    , readOAuthFromJsonFile
    , readCredentialFromJsonFile
    , saveOAuthToJsonFile
    , saveCredentialToJsonFile
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (lift)
import Data.Aeson (Value (..), (.:), toJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (MonadResource, MonadBaseControl, runResourceT)
import Data.Map (fromList)
import Network.HTTP.Conduit (withManager)
import System.IO (withBinaryFile, hPutStrLn, IOMode (..))
import Web.Authenticate.OAuth hiding (newOAuth)

import Web.Twitter.Util

newOAuth :: ByteString -> ByteString -> OAuth
newOAuth key secret = def
    { oauthServerName = "api.twitter.com"
    , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthConsumerKey = key
    , oauthConsumerSecret = secret
    }

authorize :: (MonadResource m, MonadBaseControl IO m)
          => OAuth
          -> (String -> m Int)
          -> m Credential
authorize oauth getPIN = withManager $ \man -> do
    tmp <- getTemporaryCredential oauth man
    let url = authorizeUrl oauth tmp
    pin <- lift $ getPIN url
    let tmp' = injectVerifier (showBS pin) tmp
    getAccessToken oauth tmp' man

authorizeIO :: OAuth
            -> (String -> IO Int)
            -> IO Credential
authorizeIO oauth getPIN =
    runResourceT $ authorize oauth $ lift . getPIN

readOAuthFromJsonFile :: FilePath -> IO OAuth
readOAuthFromJsonFile path = do
    bs <- BL.readFile path
    either error return $ eitherDecodeWith parser bs
  where
    parser (Object o) = newOAuth
        <$> o .: "consumer_key"
        <*> o .: "consumer_secret"
    parser v = fail $ show v

readCredentialFromJsonFile :: FilePath -> IO Credential
readCredentialFromJsonFile path = do
    bs <- BL.readFile path
    either error return $ eitherDecodeWith parser bs
  where
    parser (Object o) = newCredential
        <$> o .: "access_token"
        <*> o .: "access_token_secret"
    parser v = fail $ show v

saveOAuthToJsonFile :: FilePath -> OAuth -> IO ()
saveOAuthToJsonFile path oauth = withBinaryFile path WriteMode $ \handle -> do
    BL.hPutStr handle $ encodeWith encoder oauth
    hPutStrLn handle ""
  where
    encoder oa = toJSON . fromList $ (
        [ ("consumer_key", oauthConsumerKey oa)
        , ("consumer_secret", oauthConsumerSecret oa)
        ] :: [(ByteString, ByteString)])

saveCredentialToJsonFile :: FilePath -> Credential -> IO ()
saveCredentialToJsonFile path cred = withBinaryFile path WriteMode $ \handle -> do
    BL.hPutStr handle $ encodeWith encoder cred
    hPutStrLn handle ""
  where
    encoder = toJSON . fromList . unCredential
