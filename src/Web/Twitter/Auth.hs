{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Web.Twitter.Auth
    ( Auth (..)
    , OAuth
    , Credential
    , newOAuth
    , newCredential
    , authorize
    , authorizeIO
    , getOAuthFromJsonFile
    , getCredentialFromJsonFile
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (lift)
import Data.Aeson (Value (..), (.:))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (MonadResource, MonadBaseControl, runResourceT)
import Network.HTTP.Conduit (withManager)
import Web.Authenticate.OAuth hiding (newOAuth)

import Web.Twitter.Util

data Auth = Auth
    { twitterOAuth :: OAuth
    , twitterCredential :: Credential
    }

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

getOAuthFromJsonFile :: FilePath -> IO OAuth
getOAuthFromJsonFile path = do
    bs <- BL.readFile path
    either error return $ eitherDecodeWith parser bs
  where
    parser (Object o) = newOAuth
        <$> o .: "consumer_key"
        <*> o .: "consumer_secret"
    parser v = fail $ show v

getCredentialFromJsonFile :: FilePath -> IO Credential
getCredentialFromJsonFile path = do
    bs <- BL.readFile path
    either error return $ eitherDecodeWith parser bs
  where
    parser (Object o) = newCredential
        <$> o .: "access_token"
        <*> o .: "access_token_secret"
    parser v = fail $ show v
