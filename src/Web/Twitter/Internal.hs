{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable #-}

module Web.Twitter.Internal
    ( TwitterT
    , Env (..)
    , Auth (..)
    , runTwitterT
    , runTwitterTWithManager
    , Twitter
    , runTwitter
    , TwitterException (..)
    ) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..))
import Data.ByteString
import Data.Conduit
import Data.Typeable (Typeable)
import Network.HTTP.Conduit (Manager, withManager)
import Web.Authenticate.OAuth

type TwitterT m = ReaderT Env m

data Env = Env
    { twitterAuth :: Auth
    , twitterManager :: Manager
    }

data Auth = Auth
    { twitterOAuth :: OAuth
    , twitterCredential :: Credential
    }

runTwitterT :: (MonadIO m, MonadBaseControl IO m)
            => TwitterT (ResourceT m) a
            -> (ByteString, ByteString) -- ^ Consumer key, Consumer secret
            -> (ByteString, ByteString) -- ^ Access token, Access token secret
            -> m a
runTwitterT twitter con acc =
    withManager $ runTwitterTWithManager twitter con acc

runTwitterTWithManager :: TwitterT (ResourceT m) a
                       -> (ByteString, ByteString) -- ^ Consumer key, Consumer secret
                       -> (ByteString, ByteString) -- ^ Access token, Access token secret
                       -> Manager
                       -> ResourceT m a
runTwitterTWithManager twitter (ckey, csec) (atok, asec) man =
    runReaderT twitter env
  where
    env = Env
        { twitterAuth = auth
        , twitterManager = man
        }
    auth = Auth
        { twitterOAuth = oauth
        , twitterCredential = cred
        }
    oauth = def
        { oauthServerName = "api.twitter.com"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = ckey
        , oauthConsumerSecret = csec
        }
    cred = newCredential atok asec

type Twitter = TwitterT (ResourceT IO)

runTwitter :: Twitter a
           -> (ByteString, ByteString)
           -> (ByteString, ByteString)
           -> IO a
runTwitter twitter = runTwitterT twitter

data TwitterException
    = ParseError String
    deriving (Show, Eq, Typeable)

instance Exception TwitterException
