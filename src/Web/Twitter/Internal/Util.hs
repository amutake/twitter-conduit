module Web.Twitter.Internal.Util
    ( sinkJSON
    , sinkFromJSON
    , conduitJSON
    , conduitFromJSON
    , showBS
    , insertQuery
    , fromJSON'
    , eitherDecodeWith
    , eitherDecodeStrictWith
    , encodeWith
    , ($=+)
    ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson hiding (Error)
import Data.Aeson.Encode (encodeToTextBuilder)
import qualified Data.Aeson.Types as AT
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec as AS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Network.HTTP.Types as HT
import qualified Data.Map as M

import Web.Twitter.Internal.Types

sinkJSON :: MonadResource m => Consumer ByteString m Value
sinkJSON = CA.sinkParser json

sinkFromJSON :: (FromJSON a, MonadResource m) => Consumer ByteString m a
sinkFromJSON = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> monadThrow $ JsonParseError err
    AT.Success r -> return r

conduitJSON :: MonadResource m => Conduit ByteString m Value
conduitJSON = CL.sequence $ sinkJSON

conduitFromJSON :: (FromJSON a, MonadResource m) => Conduit ByteString m a
conduitFromJSON = CL.sequence $ sinkFromJSON

showBS :: Show a => a -> ByteString
showBS = B8.pack . show

insertQuery :: (ByteString, ByteString) -> HT.SimpleQuery -> HT.SimpleQuery
insertQuery (key, value) = mk
  where mk = M.toList . M.insert key value . M.fromList

fromJSON' :: FromJSON a => Value -> Maybe a
fromJSON' = AT.parseMaybe parseJSON

eitherDecodeWith :: (Value -> AT.Parser a) -> BL.ByteString -> Either String a
eitherDecodeWith parser = AL.eitherResult . AL.parse json >=> AT.parseEither parser

eitherDecodeStrictWith :: (Value -> AT.Parser a) -> ByteString -> Either String a
eitherDecodeStrictWith parser = AS.eitherResult . AS.parse json' >=> AT.parseEither parser

encodeWith :: (a -> Value) -> a -> BL.ByteString
encodeWith enc = encodeUtf8 . toLazyText . encodeToTextBuilder . enc

($=+) :: MonadIO m
      => CI.ResumableSource m a
      -> CI.Conduit a m o
      -> m (CI.ResumableSource m o)
rsrc $=+ cndt = do
  (src, finalizer) <- unwrapResumable rsrc
  return $ CI.ResumableSource (src $= cndt) finalizer
