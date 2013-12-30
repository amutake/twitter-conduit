module Web.Twitter.REST.Users
    ( verifyCredentials
    , getAccountSettings
    ) where

import Data.Conduit (MonadResource, MonadBaseControl)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/account/verify_credentials> 2012-09-05 09:22
verifyCredentials :: (MonadResource m, MonadBaseControl IO m)
                  => Maybe Bool -- ^ include_entities (optional)
                  -> Maybe Bool -- ^ skip_status (optional)
                  -> TwitterT m User
verifyCredentials ent skip = rest REST "account/verify_credentials" methodGet query
  where
    query =
        [ "include_entities" <:> ent
        , "skip_status" <:> skip
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/account/settings> 2012-09-05 09:22
getAccountSettings :: (MonadResource m, MonadBaseControl IO m)
                   => TwitterT m Account
getAccountSettings = rest REST "account/settings" methodGet []
