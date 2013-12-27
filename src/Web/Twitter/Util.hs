{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Web.Twitter.Util
       ( sinkJSON
       , sinkFromJSON
       , conduitJSON
       , conduitFromJSON
       , showBS
       , insertQuery
       , fromJSON'
       , ($=+)
       ) where

import Control.Exception
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Data.Aeson hiding (Error)
import qualified Data.Aeson.Types as AT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Conduit
import qualified Data.Conduit.Internal as CI
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA
import Data.Data
import qualified Network.HTTP.Types as HT
import qualified Data.Map as M

data TwitterError
  = TwitterError String
  deriving (Show, Data, Typeable)

instance Exception TwitterError

#if MIN_VERSION_conduit(1,0,0)
sinkJSON :: MonadResource m => Consumer ByteString m Value
#else
sinkJSON :: MonadResource m => GLSink ByteString m Value
#endif
sinkJSON = CA.sinkParser json

#if MIN_VERSION_conduit(1,0,0)
sinkFromJSON :: (FromJSON a, MonadResource m) => Consumer ByteString m a
#else
sinkFromJSON :: (FromJSON a, MonadResource m) => GLSink ByteString m a
#endif
sinkFromJSON = do
  v <- sinkJSON
  case fromJSON v of
    AT.Error err -> lift $ monadThrow $ TwitterError err
    AT.Success r -> return r

#if MIN_VERSION_conduit(1,0,0)
conduitJSON :: MonadResource m => Conduit ByteString m Value
#else
conduitJSON :: MonadResource m => GLInfConduit ByteString m Value
#endif
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

($=+) :: MonadIO m
      => CI.ResumableSource m a
      -> CI.Conduit a m o
      -> m (CI.ResumableSource m o)
rsrc $=+ cndt = do
  (src, finalizer) <- unwrapResumable rsrc
  return $ CI.ResumableSource (src $= cndt) finalizer
