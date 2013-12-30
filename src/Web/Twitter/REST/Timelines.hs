module Web.Twitter.REST.Timelines where

import Data.Conduit (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/mentions_timeline> 2013-06-20 13:39
mentionsTimeline :: (MonadResource m, MonadBaseControl IO m)
                 => Maybe Int -- ^ count (optional)
                 -> Maybe StatusId -- ^ since_id (optional)
                 -> Maybe StatusId -- ^ max_id (optional)
                 -> Maybe Bool -- ^ trim_user (optional)
                 -> Maybe Bool -- ^ contributor_details (optional)
                 -> Maybe Bool -- ^ include_entities (optional)
                 -> TwitterT m [Status]
mentionsTimeline count sid mid trim contrib inc =
    rest REST "statuses/mentions_timeline" methodGet [] query
  where
    query =
        [ "count" <:> count
        , "since_id" <:> sid
        , "max_id" <:> mid
        , "trim_user" <:> trim
        , "contributor_details" <:> contrib
        , "include_entities" <:> inc
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline> 2013-03-07 09:38
userTimeline :: (MonadResource m, MonadBaseControl IO m)
             => Maybe UserId -- ^ user_id (optional)
             -> Maybe Text -- ^ screen_name (optional)
             -> Maybe Int -- ^ count (optional)
             -> Maybe StatusId -- ^ since_id (optional)
             -> Maybe StatusId -- ^ max_id (optional)
             -> Maybe Bool -- ^ trim_user (optional)
             -> Maybe Bool -- ^ exclude_replies (optional)
             -> Maybe Bool -- ^ contributor_details (optional)
             -> Maybe Bool -- ^ include_rts (optional)
             -> TwitterT m [Status]
userTimeline uid name count sid mid trim rep contrib rts =
    rest REST "statuses/user_timeline" methodGet [] query
  where
    query =
        [ "user_id" <:> uid
        , "screen_name" <:> name
        , "count" <:> count
        , "since_id" <:> sid
        , "max_id" <:> mid
        , "trim_user" <:> trim
        , "exclude_replies" <:> rep
        , "contributor_details" <:> contrib
        , "include_rts" <:> rts
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/home_timeline> 2012-09-05 10:06
homeTimeline :: (MonadResource m, MonadBaseControl IO m)
             => Maybe Int -- ^ count (optional)
             -> Maybe StatusId -- ^ since_id (optional)
             -> Maybe StatusId -- ^ max_id (optional)
             -> Maybe Bool -- ^ trim_user (optional)
             -> Maybe Bool -- ^ exclude_replies (optional)
             -> Maybe Bool -- ^ contributor_details (optional)
             -> Maybe Bool -- ^ include_rts (optional)
             -> TwitterT m [Status]
homeTimeline count sid mid trim rep contrib rts =
    rest REST "statuses/home_timeline" methodGet [] query
  where
    query =
        [ "count" <:> count
        , "since_id" <:> sid
        , "max_id" <:> mid
        , "trim_user" <:> trim
        , "exclude_replies" <:> rep
        , "contributor_details" <:> contrib
        , "include_rts" <:> rts
        ]

-- | https://dev.twitter.com/docs/api/1.1/get/statuses/retweets_of_me> 2013-03-13 14:33
retweetsOfMe :: (MonadResource m, MonadBaseControl IO m)
             => Maybe Int -- ^ count (optional)
             -> Maybe StatusId -- ^ since_id (optional)
             -> Maybe StatusId -- ^ max_id (optional)
             -> Maybe Bool -- ^ trim_user (optional)
             -> Maybe Bool -- ^ include_entities (optional)
             -> Maybe Bool -- ^ include_user_entities (optional)
             -> TwitterT m [Status]
retweetsOfMe count sid mid trim ent userent =
    rest REST "statuses/retweets_of_me" methodGet [] query
  where
    query =
        [ "count" <:> count
        , "since_id" <:> sid
        , "max_id" <:> mid
        , "trim_user" <:> trim
        , "include_entities" <:> ent
        , "include_user_entities" <:> userent
        ]
