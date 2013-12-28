{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Web.Twitter.Auth
    ( OAuth
    , AccessToken
    , newOAuth
    , newAccessToken
    , authorize
    , authorizeIO
    ) where

import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Conduit (MonadResource, MonadBaseControl, runResourceT)
import Network.HTTP.Conduit (withManager)
import Web.Authenticate.OAuth hiding (newOAuth)

import Web.Twitter.Internal.Util

type AccessToken = Credential

newOAuth :: ByteString -> ByteString -> OAuth
newOAuth key secret = def
    { oauthServerName = "api.twitter.com"
    , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthConsumerKey = key
    , oauthConsumerSecret = secret
    }

newAccessToken :: ByteString -> ByteString -> AccessToken
newAccessToken = newCredential

authorize :: (MonadResource m, MonadBaseControl IO m)
          => OAuth
          -> (String -> m Int)
          -> m AccessToken
authorize oauth getPIN = withManager $ \man -> do
    tmp <- getTemporaryCredential oauth man
    let url = authorizeUrl oauth tmp
    pin <- lift $ getPIN url
    let tmp' = injectVerifier (showBS pin) tmp
    getAccessToken oauth tmp' man

authorizeIO :: OAuth
            -> (String -> IO Int)
            -> IO AccessToken
authorizeIO oauth getPIN =
    runResourceT $ authorize oauth $ lift . getPIN
