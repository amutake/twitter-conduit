{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Fetch
       (
       -- * Search
         search
       -- , searchSource
       -- , searchSourceFrom

       -- * Direct Messages
       , directMessages
       -- , directMessagesSent
       -- , directMessagesShowId

       -- * Friends & Followers
       , friendsIds
       , followersIds
       -- , friendshipsExists
       -- , friendshipsIncoming
       -- , friendshipsOutgoing
       -- , friendshipsShow
       -- , friendshipsLookup
       -- , friendshipsNoRetweetIds

       -- * Users
       -- , usersLookup
       -- , usersProfileImageScreenName
       -- , usersSearch
       , usersShow
       -- , usersContributees
       -- , usersContributors

       -- * Suggested Users
       -- , usersSuggestions
       -- , usersSuggestionsSlug
       -- , usersSuggestionsSlugMembers

       -- * Favorites
       -- , favorites

       -- * Lists
       , listsAll
       -- , listsStatuses
       -- , listsMemberships
       -- , listsSubscribers
       -- , listsSubscribersShow
       -- , listsMembersShow
       , listsMembers
       -- , lists
       -- , listsShow
       -- , listsSubscriptions
       ) where

import Web.Twitter.Types
import Web.Twitter.Monad
import Web.Twitter.Param
import Web.Twitter.Util
import Web.Twitter.API

import qualified Network.HTTP.Types as HT
import qualified Data.Conduit as C
import qualified Data.ByteString.Char8 as B8

endpointSearch :: String
endpointSearch = "http://search.twitter.com/search.json"

search :: TwitterBaseM m
       => String -- ^ search string
       -> Int -- ^ page
       -> HT.SimpleQuery -- ^ query
       -> TW cred m (SearchResult [SearchStatus])
search q page query = search' query'
  where query' = ("q", B8.pack $ q) : ("page", showBS page) : query

search' :: TwitterBaseM m
        => HT.SimpleQuery -- ^ query
        -> TW cred m (SearchResult [SearchStatus])
search' query = apiGet' noAuth endpointSearch query

directMessages :: TwitterBaseM m
               => HT.SimpleQuery -- ^ query
               -> C.Source (TW WithToken m) DirectMessage
directMessages query = apiWithPages authRequired "direct_messages.json" query

friendsIds, followersIds
  :: TwitterBaseM m => UserParam -> C.Source (TW cred m) UserId
friendsIds   q = apiCursor authSupported "friends/ids.json"   (mkUserParam q) "ids"
followersIds q = apiCursor authSupported "followers/ids.json" (mkUserParam q) "ids"

usersShow :: TwitterBaseM m => UserParam -> (TW cred m) User
usersShow q = apiGet authSupported "users/show.json" (mkUserParam q)

listsAll :: TwitterBaseM m => UserParam -> C.Source (TW cred m) List
listsAll q = apiCursor authSupported "lists/all.json" (mkUserParam q) ""

listsMembers :: TwitterBaseM m => ListParam -> C.Source (TW cred m) User
listsMembers q = apiCursor authSupported "lists/members.json" (mkListParam q) "users"
