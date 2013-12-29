module Web.Twitter.Core
    ( TwitterT
    , runTwitterT
    , runTwitterTWithManager
    , Twitter
    , runTwitter
    , TwitterException (..)
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..))
import Data.Conduit
import Network.HTTP.Conduit (Manager, withManager)

import Web.Twitter.Auth
import Web.Twitter.Internal.Types

type TwitterT m = ReaderT Env m

runTwitterT :: (MonadIO m, MonadBaseControl IO m)
            => OAuth
            -> AccessToken
            -> TwitterT (ResourceT m) a
            -> m a
runTwitterT oauth token twitter =
    withManager $ runTwitterTWithManager oauth token twitter

runTwitterTWithManager :: OAuth
                       -> AccessToken
                       -> TwitterT m a
                       -> Manager
                       -> m a
runTwitterTWithManager oauth token twitter man =
    runReaderT twitter env
  where
    env = Env
        { twitterOAuth = oauth
        , twitterAccessToken = token
        , twitterManager = man
        }

type Twitter = TwitterT (ResourceT IO)

runTwitter :: OAuth
           -> AccessToken
           -> Twitter a
           -> IO a
runTwitter = runTwitterT
