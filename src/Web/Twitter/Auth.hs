module Web.Twitter.Auth
    ( OAuth
    , AccessToken
    , newOAuth
    , newAccessToken
    , authorize
    , authorizeIO
    ) where

import Control.Monad.Trans (lift)
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit (MonadResource, MonadBaseControl, runResourceT)
import Network.HTTP.Conduit (withManager)
import Web.Authenticate.OAuth hiding (newOAuth)

import Web.Twitter.Internal.Types
import Web.Twitter.Internal.Util

newOAuth :: String -> String -> OAuth
newOAuth key secret = def
    { oauthServerName = "api.twitter.com"
    , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthConsumerKey = BSC.pack key
    , oauthConsumerSecret = BSC.pack secret
    }

newAccessToken :: String -> String -> AccessToken
newAccessToken token secret = newCredential (BSC.pack token) (BSC.pack secret)

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
