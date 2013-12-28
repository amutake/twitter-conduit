{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.REST.Timelines where

import Data.Conduit (MonadResource)
import Data.Text (Text)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/mentions_timeline> 2013-06-20 13:39
mentionsTimeline :: MonadResource m
                 => Maybe Int -- ^ count (optional)
                 -> Maybe StatusId -- ^ since_id (optional)
                 -> Maybe StatusId -- ^ max_id (optional)
                 -> Maybe Bool -- ^ trim_user (optional)
                 -> Maybe Bool -- ^ contributor_details (optional)
                 -> Maybe Bool -- ^ include_entities (optional)
                 -> TwitterT m [Status]
mentionsTimeline count sid mid trim contrib inc =
    apiSingle REST "statuses/mentions_timeline" methodGet query
  where
    query =
        [ "count" <:> count
        , "since_id" <:> sid
        , "max_id" <:> mid
        , "trim_user" <:> trim
        , "contributor_details" <:> contrib
        , "include_entities" <:> inc
        ]

userTimeline :: MonadResource m
             => Maybe UserId -- ^ user_id (optional)
             -> Maybe Text -- ^ screen_name (optional)
             -> Maybe Int -- ^ count (optional)
             -> Maybe StatusId -- ^ since_id (optional)
             -> Maybe StatusId -- ^ max_id (optional)
             -> Maybe Bool -- ^ trim_user (optional)
             -> Maybe Bool -- ^ contributor_details (optional)
             -> Maybe Bool -- ^ include_rts (optional)
             -> Maybe Bool -- ^ exclude_replies (optional)
             -> TwitterT m [Status]
userTimeline uid name count sid mid trim contrib rts rep =
    apiSingle REST "statuses/user_timeline" methodGet query
  where
    query =
        [ "user_id" <:> uid
        , "screen_name" <:> name
        , "count" <:> count
        , "since_id" <:> sid
        , "max_id" <:> mid
        , "trim_user" <:> trim
        , "contributor_details" <:> contrib
        , "include_rts" <:> rts
        , "exclude_replies" <:> rep
        ]
