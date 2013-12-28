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
import Data.Conduit
import Data.Typeable (Typeable)
import Network.HTTP.Conduit (Manager, withManager)

import Web.Twitter.Auth

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
            => OAuth
            -> Credential
            -> TwitterT (ResourceT m) a
            -> m a
runTwitterT oauth cred twitter =
    withManager $ runTwitterTWithManager oauth cred twitter

runTwitterTWithManager :: OAuth
                       -> Credential
                       -> TwitterT m a
                       -> Manager
                       -> m a
runTwitterTWithManager oauth cred twitter man =
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

type Twitter = TwitterT (ResourceT IO)

runTwitter :: OAuth
           -> Credential
           -> Twitter a
           -> IO a
runTwitter = runTwitterT

data TwitterException
    = ParseError String
    deriving (Show, Eq, Typeable)

instance Exception TwitterException
