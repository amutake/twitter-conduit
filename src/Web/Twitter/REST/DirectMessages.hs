module Web.Twitter.REST.DirectMessages
    ( directMessages
    , sent
    , showDirectMessage
    ) where

import Data.Conduit (MonadResource, MonadBaseControl)
import Network.HTTP.Types (methodGet)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/get/direct_messages> 2013-01-29 08:08
directMessages :: (MonadResource m, MonadBaseControl IO m)
               => Maybe DirectMessageId -- ^ since_id (optional)
               -> Maybe DirectMessageId -- ^ max_id (optional)
               -> Maybe Int -- ^ count (optional)
               -> Maybe Bool -- ^ include_entities (optional)
               -> Maybe Bool -- ^ skip_status (optional)
               -> TwitterT m [DirectMessage]
directMessages sid mid count ent skip = rest REST "direct_messages" methodGet query
  where
    query =
        [ "since_id" <:> sid
        , "max_id" <:> mid
        , "count" <:> count
        , "include_entities" <:> ent
        , "skip_status" <:> skip
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/direct_messages/sent> 2012-09-05 09:32
sent :: (MonadResource m, MonadBaseControl IO m)
     => Maybe DirectMessageId -- ^ since_id (optional)
     -> Maybe DirectMessageId -- ^ max_id (optional)
     -> Maybe Int -- ^ count (optional)
     -> Maybe Int -- ^ page (optional)
     -> Maybe Bool -- ^ include_entities (optional)
     -> TwitterT m [DirectMessage]
sent sid mid count page ent = rest REST "direct_messages/sent" methodGet query
  where
    query =
        [ "since_id" <:> sid
        , "max_id" <:> mid
        , "count" <:> count
        , "page" <:> page
        , "include_entities" <:> ent
        ]

-- | <https://dev.twitter.com/docs/api/1.1/get/direct_messages/show> 2012-09-05 09:25
showDirectMessage :: (MonadResource m, MonadBaseControl IO m)
                  => DirectMessageId -- ^ id
                  -> TwitterT m [DirectMessage]
showDirectMessage did = rest REST "direct_messages/show" methodGet query
  where
    query =
        [ "id" <:> did
        ]
