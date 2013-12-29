module Web.Twitter.Streaming.User
    ( user
    ) where

import Data.Conduit (MonadResource, MonadBaseControl, ResumableSource)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/user> 2013-08-26 14:13
user :: (MonadResource m, MonadBaseControl IO m)
     => TwitterT m (ResumableSource (TwitterT m) UserStream)
user = apiSource UserStream "user" methodGet []
