{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE ConstraintKinds #-}
#endif

module Web.Twitter.Stream
       (
       -- * StreamingAPI
         userstream
       , statusesFilter
  ) where

import Web.Twitter.API
import Web.Twitter.Monad
import Web.Twitter.Types
import Web.Twitter.Util

import qualified Data.Conduit as C
import qualified Network.HTTP.Types as HT

userstream :: TwitterBaseM m => TW WithToken m (C.ResumableSource (TW WithToken m) StreamingAPI)
userstream = do
  rsrc <- api authRequired "GET" "https://userstream.twitter.com/2/user.json" []
  rsrc $=+ conduitFromJSON

statusesFilter :: TwitterBaseM m => HT.SimpleQuery -> TW WithToken m (C.ResumableSource (TW WithToken m) StreamingAPI)
statusesFilter query = do
  rsrc <- api authRequired "GET" "https://stream.twitter.com/1/statuses/filter.json" query
  rsrc $=+ conduitFromJSON
