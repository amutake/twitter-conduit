module Web.Twitter.Streaming.Public
    ( sample
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
       => Maybe Text -- ^ delimited (optional) (currently not supported)
       -> Maybe Bool -- ^ stall_warnings (optional)
       -> TwitterT m (ResumableSource (TwitterT m) UserStreamMessage)
sample del warn = stream Stream "statuses/sample" methodGet query
  where
    query =
        [ "delimited" <:> del
        , "stall_warnings" <:> warn
        ]
