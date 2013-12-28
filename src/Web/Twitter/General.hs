{-# LANGUAGE FlexibleContexts, CPP #-}

module Web.Twitter.General
    ( ApiType (..)
    , api
    , apiSource
    , apiSingle
    ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (ByteString)
import Data.Conduit (MonadResource, ResumableSource, monadThrow)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Web.Authenticate.OAuth

import Web.Twitter.Core
import Web.Twitter.Internal.Query
import Web.Twitter.Internal.Types
import Web.Twitter.Internal.Util

#ifdef DEBUG
import Data.ByteString.Char8 (unpack)
import Data.Conduit (($$))
import Data.Conduit.Binary (conduitHandle, sinkHandle, sourceLbs)
import System.IO (openBinaryFile, hClose, hPutStrLn, IOMode (..))
#endif

type ApiName = String
type Endpoint = String

data ApiType = REST
             | UserStream

endpoint :: ApiType -> ApiName -> Endpoint
endpoint REST name = "https://api.twitter.com/1.1/" ++ name ++ ".json"
endpoint UserStream name = "https://userstream.twitter.com/1.1/" ++ name ++ ".json"

api :: (MonadResource m)
    => ApiType -- ^ API Type
    -> ApiName -- ^ API Name
    -> Method -- ^ HTTP request method
    -> Query -- ^ Query
    -> TwitterT m (Response (ResumableSource (TwitterT m) ByteString))
api ty name mth query = do
    env <- ask
    let oauth = twitterOAuth env
        token = twitterAccessToken env
        man = twitterManager env
    req <- liftIO $ parseUrl $ endpoint ty name
    signed <- signOAuth oauth token req
        { method = mth
        , queryString = renderQuery' query
        }
    http signed man

apiSource :: (MonadResource m, FromJSON a)
          => ApiType
          -> ApiName
          -> Method
          -> Query
          -> TwitterT m (ResumableSource (TwitterT m) a)
apiSource ty name mth query = do
    res <- responseBody <$> api ty name mth query
#ifdef DEBUG
    handle <- liftIO $ openBinaryFile "debug.log" AppendMode
    liftIO $ hPutStrLn handle $ endpoint ty name ++ unpack (renderQuery' query)
    res' <- res $=+ conduitHandle handle
    liftIO $ hPutStrLn handle ""
    liftIO $ hClose handle
    res' $=+ conduitFromJSON
#else
    res $=+ conduitFromJSON
#endif

apiSingle :: (MonadResource m, FromJSON a)
          => ApiType
          -> ApiName
          -> Method
          -> Query
          -> TwitterT m a
apiSingle ty name mth query = do
    res <- api ty name mth query >>= lbsResponse
    let body = responseBody res
#ifdef DEBUG
    handle <- liftIO $ openBinaryFile "debug.log" AppendMode
    liftIO $ hPutStrLn handle $ endpoint ty name ++ unpack (renderQuery' query)
    sourceLbs body $$ sinkHandle handle
    liftIO $ hPutStrLn handle ""
    liftIO $ hClose handle
#endif
    either (monadThrow . ParseError) return $ eitherDecode body
