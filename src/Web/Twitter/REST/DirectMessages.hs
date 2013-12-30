module Web.Twitter.REST.DirectMessages
    ( directMessages
    , sent
    , showDirectMessage
    , destroyDirectMessage
    , newDirectMessage
    ) where

import Data.Conduit (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Network.HTTP.Types (methodGet, methodPost)

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
directMessages sid mid count ent skip = rest REST "direct_messages" methodGet [] query
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
sent sid mid count page ent = rest REST "direct_messages/sent" methodGet [] query
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
                  -> TwitterT m DirectMessage
showDirectMessage did = rest REST "direct_messages/show" methodGet [] query
  where
    query =
        [ "id" <:> did
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/direct_messages/destroy> 2012-09-07 16:32
destroyDirectMessage :: (MonadResource m, MonadBaseControl IO m)
                     => DirectMessageId -- ^ id
                     -> Maybe Bool -- ^ include_entities (optional)
                     -> TwitterT m DirectMessage
destroyDirectMessage did ent = rest REST "direct_messages/destroy" methodPost [] query
  where
    query =
        [ "id" <:> did
        , "include_entities" <:> ent
        ]

-- | <https://dev.twitter.com/docs/api/1.1/post/direct_messages/new> 2012-10-02 14:07
newDirectMessage :: (MonadResource m, MonadBaseControl IO m)
                 => Either UserId ScreenName -- ^ user_id or screen_name
                 -> Text -- ^ text
                 -> TwitterT m DirectMessage
newDirectMessage uidName text = rest REST "direct_messages/new" methodPost [] query
  where
    query = ["text" <:> text] ++ case uidName of
        Left uid -> ["user_id" <:> uid]
        Right name -> ["screen_name" <:> name]
