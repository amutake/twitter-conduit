{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.REST.Tweets where

import Data.ByteString (ByteString)
import Data.Conduit (MonadResource)
import Data.Text (Text)
import Network.HTTP.Types (methodPost)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

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
