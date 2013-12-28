{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.REST.Timelines where

import Data.Conduit (MonadResource)
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
