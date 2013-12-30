module Web.Twitter.REST.Tweets
    ( retweets
    , showStatus
    , destroy
    , update
    , retweet
    , retweeters
    , updateStatusWithMedia
    ) where

import Data.ByteString (ByteString)
import Data.Conduit (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Network.HTTP.Client.MultipartFormData (partFileSourceChunked)
import Network.HTTP.Types (methodGet, methodPost)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/retweets/%3Aid> 2013-08-28 06:59
retweets :: (MonadResource m, MonadBaseControl IO m)
         => StatusId -- ^ id
         -> Maybe Int -- ^ count (optional)
         -> Maybe Bool -- ^ trim_user (optional)
         -> TwitterT m [Status]
retweets sid count trim = rest REST ("statuses/retweets/" ++ show sid) methodGet [] query
  where
    query =
        [ "count" <:> count
        , "trim_user" <:> trim
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/show/%3Aid> 2013-03-07 09:37
showStatus :: (MonadResource m, MonadBaseControl IO m)
           => StatusId -- ^ id
           -> Maybe Bool -- ^ trim_user (optional)
           -> Maybe Bool -- ^ include_my_retweet (optional)
           -> Maybe Bool -- ^ include_entities (optional)
           -> TwitterT m Status
showStatus sid trim mine ent = rest REST "statuses/show" methodGet [] query
  where
    query =
        [ "id" <:> sid
        , "trim_user" <:> trim
        , "include_my_retweet" <:> mine
        , "include_entities" <:> ent
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/statuses/destroy/%3Aid> 2013-01-29 08:36
destroy :: (MonadResource m, MonadBaseControl IO m)
        => StatusId -- ^ id
        -> Maybe Bool -- ^ trim_user (optional)
        -> TwitterT m Status
destroy sid trim = rest REST ("statuses/destroy/" ++ show sid) methodPost [] query
  where
    query =
        [ "trim_user" <:> trim
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/statuses/update> 2012-11-20 07:24
update :: (MonadResource m, MonadBaseControl IO m)
       => Text -- ^ status
       -> Maybe StatusId -- ^ in_reply_to_status_id (optional)
       -> Maybe Double -- ^ lat (optional)
       -> Maybe Double -- ^ long (optional)
       -> Maybe ByteString -- ^ place_id (optional)
       -> Maybe Bool -- ^ display_coordinates (optional)
       -> Maybe Bool -- ^ trim_user (optional)
       -> TwitterT m Status
update status sid lat long pid disp trim = rest REST "statuses/update" methodPost [] query
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

-- | <https://dev.twitter.com/docs/api/1.1/post/statuses/retweet/%3Aid> 2012-12-03 14:06
retweet :: (MonadResource m, MonadBaseControl IO m)
        => StatusId -- ^ id
        -> Maybe Bool -- ^ trim_user (optional)
        -> TwitterT m Status
retweet sid trim = rest REST ("statuses/retweet/" ++ show sid) methodPost [] query
  where
    query =
        [ "trim_user" <:> trim
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/retweeters/ids> 2013-05-07 09:47
retweeters :: (MonadResource m, MonadBaseControl IO m)
           => StatusId -- ^ id
           -> Maybe StatusId -- ^ cursor (semi-optional)
           -> Maybe Bool -- ^ stringify_ids (optional)
           -> TwitterT m Ids
retweeters sid cursor str = rest REST "statuses/retweeters/ids" methodGet [] query
  where
    query =
        [ "id" <:> sid
        , "cursor" <:> cursor
        , "stringify_ids" <:> str
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/statuses/update_with_media> 2013-12-01 21:28
updateStatusWithMedia :: (MonadResource m, MonadBaseControl IO m)
                      => Text -- ^ status
                      -> [FilePath] -- ^ media[] Up to max_media_per_upload (GET help/configuration) files.
                      -> Maybe Bool -- ^ possibly_sensitive (optional)
                      -> Maybe StatusId -- ^ in_reply_to_status_id (optional)
                      -> Maybe Double -- ^ lat (optional)
                      -> Maybe Double -- ^ long (optional)
                      -> Maybe Text -- ^ place_id (optional)
                      -> Maybe Bool -- ^ display_coordinates (optional)
                      -> TwitterT m Status
updateStatusWithMedia text paths sens rep lat long pl co =
    rest REST "statuses/update_with_media" methodPost parts query
  where
    parts = map (partFileSourceChunked "media[]") paths
    query =
        [ "status" <:> text
        , "possibly_sensitive" <:> sens
        , "in_reply_to_status_id" <:> rep
        , "lat" <:> lat
        , "long" <:> long
        , "place_id" <:> pl
        , "display_coordinates" <:> co
        ]
