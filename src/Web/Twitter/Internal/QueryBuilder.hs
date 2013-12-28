{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes, GADTs #-}

module Web.Twitter.Internal.QueryBuilder
    ( renderQuery'
    , (<:>)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Query, QueryItem, renderQuery)

renderQuery' :: Query -> ByteString
renderQuery' = renderQuery True . filter (isJust . snd)

class QueryValue v where
    qv :: v -> Maybe ByteString

(<:>) :: QueryValue v => ByteString -> v -> QueryItem
key <:> val = (key, qv val)

instance QueryValue Text where
    qv = return . encodeUtf8

instance QueryValue ByteString where
    qv = return

instance QueryValue BL.ByteString where
    qv = return . BL.toStrict

instance QueryValue String where
    qv = return . BS.pack

instance QueryValue Char where
    qv = return . BS.singleton

instance QueryValue Bool where
    qv True = return "true"
    qv False = return "false"

instance QueryValue Integer where
    qv = return . BS.pack . show

instance QueryValue Int where
    qv = return . BS.pack . show

instance QueryValue Double where
    qv = return . BS.pack . show

instance QueryValue v => QueryValue (Maybe v) where
    qv = (>>= qv)
