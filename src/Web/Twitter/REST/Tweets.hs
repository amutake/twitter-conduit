{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.REST.Tweets where

import Data.ByteString (ByteString)
import Data.Conduit (MonadResource)
import Data.Text (Text)
import Network.HTTP.Types (methodGet, methodPost)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/retweets/%3Aid> 2013-08-28 06:59
retweets :: MonadResource m
         => StatusId -- ^ id
         -> Maybe Int -- ^ count (optional)
         -> Maybe Bool -- ^ trim_user (optional)
         -> TwitterT m [Status]
retweets sid count trim = apiSingle REST ("statuses/retweets/" ++ show sid) methodGet query
  where
    query =
        [ "count" <:> count
        , "trim_user" <:> trim
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/show/%3Aid> 2013-03-07 09:37
showStatus :: MonadResource m
           => StatusId -- ^ id
           -> Maybe Bool -- ^ trim_user (optional)
           -> Maybe Bool -- ^ include_my_retweet (optional)
           -> Maybe Bool -- ^ include_entities (optional)
           -> TwitterT m Status
showStatus sid trim mine ent = apiSingle REST "statuses/show" methodGet query
  where
    query =
        [ "id" <:> sid
        , "trim_user" <:> trim
        , "include_my_retweet" <:> mine
        , "include_entities" <:> ent
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/statuses/destroy/%3Aid> 2013-01-29 08:36
destroy :: MonadResource m
        => StatusId -- ^ id
        -> Maybe Bool -- ^ trim_user (optional)
        -> TwitterT m Status
destroy sid trim = apiSingle REST ("statuses/destroy/" ++ show sid) methodPost query
  where
    query =
        [ "trim_user" <:> trim
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/statuses/update> 2012-11-20 07:24
update :: MonadResource m
       => Text -- ^ status
       -> Maybe StatusId -- ^ in_reply_to_status_id (optional)
       -> Maybe Double -- ^ lat (optional)
       -> Maybe Double -- ^ long (optional)
       -> Maybe ByteString -- ^ place_id (optional)
       -> Maybe Bool -- ^ display_coordinates (optional)
       -> Maybe Bool -- ^ trim_user (optional)
       -> TwitterT m Status
update status sid lat long pid disp trim = apiSingle REST "statuses/update" methodPost query
  where
    query =
        [ "status" <:> status
        , "in_reply_to_status_id" <:> sid
        , "lat" <:> lat
        , "long" <:> long
        , "place_id" <:> pid
        , "display_coordinates" <:> disp
        , "trim_user" <:> trim
        ]
