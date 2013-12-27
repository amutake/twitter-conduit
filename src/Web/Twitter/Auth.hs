{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.Auth
    ( Auth (..)
    , OAuth
    , Credential
    , newOAuth
    , newCredential
    , getCredential
    , getCredentialIO
    ) where

import Control.Monad.Trans (lift)
import Data.ByteString
import Data.Conduit
import Network.HTTP.Conduit
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

getCredential :: (MonadResource m, MonadBaseControl IO m)
              => OAuth
              -> (String -> m Int)
              -> m Credential
getCredential oauth getPIN = withManager $ \man -> do
    tmp <- getTemporaryCredential oauth man
    let url = authorizeUrl oauth tmp
    pin <- lift $ getPIN url
    let tmp' = injectVerifier (showBS pin) tmp
    getAccessToken oauth tmp' man

getCredentialIO :: OAuth
                -> (String -> IO Int)
                -> IO Credential
getCredentialIO oauth getPIN =
    runResourceT $ getCredential oauth $ lift . getPIN
