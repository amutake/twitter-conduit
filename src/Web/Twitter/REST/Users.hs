module Web.Twitter.REST.Users
    ( verifyCredentials
    , getAccountSettings
    , updateAccountSettings
    ) where

import Data.Conduit (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Network.HTTP.Types (methodGet, methodPost)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/account/verify_credentials> 2012-09-05 09:22
verifyCredentials :: (MonadResource m, MonadBaseControl IO m)
                  => Maybe Bool -- ^ include_entities (optional)
                  -> Maybe Bool -- ^ skip_status (optional)
                  -> TwitterT m User
verifyCredentials ent skip = rest REST "account/verify_credentials" methodGet [] query
  where
    query =
        [ "include_entities" <:> ent
        , "skip_status" <:> skip
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/account/settings> 2012-09-05 09:22
getAccountSettings :: (MonadResource m, MonadBaseControl IO m)
                   => TwitterT m Account
getAccountSettings = rest REST "account/settings" methodGet [] []

-- | <https://dev.twitter.com/docs/api/1.1/post/account/settings> 2012-10-15 06:07
updateAccountSettings :: (MonadResource m, MonadBaseControl IO m)
                      => Maybe Int -- ^ trend_location_woeid (optional)
                      -> Maybe Bool -- ^ sleep_time_enabled (optional)
                      -> Maybe Int -- ^ start_sleep_time (optional) 00-23
                      -> Maybe Int -- ^ end_sleep_time (optional) 00 - 23
                      -> Maybe Text -- ^ time_zone (optional) The timezone must be one of the Rails TimeZone <http://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html> names.
                      -> Maybe LanguageCode -- ^ lang (optional) The language must be specified by the appropriate two letter ISO 639-1 representation.
                      -> TwitterT m Account
updateAccountSettings woe sleep start end tz lang
    = rest REST "account/settings" methodPost [] query
  where
    query =
        [ "trend_location_woeid" <:> woe
        , "sleep_time_enabled" <:> sleep
        , "start_sleep_time" <:> start
        , "end_sleep_time" <:> end
        , "time_zone" <:> tz
        , "lang" <:> lang
        ]
