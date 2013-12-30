module Web.Twitter.Streaming.Site
    ( site
    ) where

import Data.Conduit (MonadResource, MonadBaseControl, ResumableSource)
import Data.Text (Text)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/site> 2013-08-26 14:14
site :: (MonadResource m, MonadBaseControl IO m)
     => [UserId] -- ^ follow
     -> Maybe Text -- ^ delimited (optional)
     -> Maybe Bool -- ^ stall_warnings (optional)
     -> Maybe Text -- ^ with (optional)
     -> Maybe Text -- ^ replies (optional)
     -> Maybe Bool -- ^ stringify_friend_ids (optional)
     -> TwitterT m (ResumableSource (TwitterT m) StreamMessage)
site fids del warn with rep str = stream SiteStream "site" methodGet [] query
  where
    query =
        [ "follow" <:> fids
        , "delimited" <:> del
        , "stall_warnings" <:> warn
        , "with" <:> with
        , "replies" <:> rep
        , "stringify_friend_ids" <:> str
        ]
