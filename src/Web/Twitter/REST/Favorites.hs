module Web.Twitter.REST.Favorites
    ( createFavorite
    ) where

import Data.Conduit (MonadResource, MonadBaseControl)
import Network.HTTP.Types (methodPost)

import Web.Twitter.Core
import Web.Twitter.General
import Web.Twitter.Internal.Query
import Web.Twitter.Types

-- | <https://dev.twitter.com/docs/api/1.1/post/favorites/create> 2013-02-18 08:21
createFavorite :: (MonadResource m, MonadBaseControl IO m)
                => StatusId -- ^ id
                -> Maybe Bool -- ^ include_entities (optional)
                -> TwitterT m Status
createFavorite sid ent = rest REST "favorites/create" methodPost [] query
  where
    query =
        [ "id" <:> sid
        , "include_entities" <:> ent
        ]
