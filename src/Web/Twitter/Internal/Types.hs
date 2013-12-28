{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Internal.Types
    ( TwitterException (..)
    , Env (..)
    , OAuth
    , AccessToken
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Network.HTTP.Conduit (Manager)
import Web.Authenticate.OAuth (Credential, OAuth)

data TwitterException
    = ParseError String
    deriving (Show, Eq, Typeable)

instance Exception TwitterException

data Env = Env
    { twitterOAuth :: OAuth
    , twitterAccessToken :: AccessToken
    , twitterManager :: Manager
    }

type AccessToken = Credential
