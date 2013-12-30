module Web.Twitter.Streaming.Public
    ( sample
    , firehose
    ) where

import Data.Conduit (MonadResource, MonadBaseControl, ResumableSource)
import Data.Text (Text)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/sample> 2012-08-27 16:24
sample :: (MonadResource m, MonadBaseControl IO m)
       => Maybe Text -- ^ delimited (optional) (it will throw a parser error if you enable this)
       -> Maybe Bool -- ^ stall_warnings (optional)
       -> TwitterT m (ResumableSource (TwitterT m) StreamMessage)
sample del warn = stream Stream "statuses/sample" methodGet [] query
  where
    query =
        [ "delimited" <:> del
        , "stall_warnings" <:> warn
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/statuses/firehose> 2012-08-27 16:26
firehose :: (MonadResource m, MonadBaseControl IO m)
         => Maybe Int -- ^ count (optional)
         -> Maybe Text -- ^ delimited (optional)
         -> Maybe Bool -- ^ stall_warnings (optional)
         -> TwitterT m (ResumableSource (TwitterT m) StreamMessage)
firehose count del warn = stream Stream "statuses/firehose" methodGet [] query
  where
    query =
        [ "count" <:> count
        , "delimited" <:> del
        , "stall_warnings" <:> warn
        ]
