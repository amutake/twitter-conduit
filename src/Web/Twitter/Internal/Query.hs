{-# LANGUAGE FlexibleInstances #-}

module Web.Twitter.Internal.Query
    ( renderQuery'
    , (<:>)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Maybe (isJust, catMaybes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Query, QueryItem, renderQuery)

renderQuery' :: Query -> ByteString
renderQuery' = renderQuery False . filter (isJust . snd)

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

instance QueryValue Bool where
    qv True = return "true"
    qv False = return "false"

instance QueryValue Integer where
    qv = return . BS.pack . show

instance QueryValue Int where
    qv = return . BS.pack . show

instance QueryValue Int64 where
    qv = return . BS.pack . show

instance QueryValue Double where
    qv = return . BS.pack . show

instance QueryValue v => QueryValue (Maybe v) where
    qv = (>>= qv)

instance QueryValue v => QueryValue [v] where
    qv = may . catMaybes . map qv
      where
        may [] = Nothing
        may ls = Just $ BS.intercalate "," ls
