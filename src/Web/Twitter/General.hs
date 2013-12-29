{-# LANGUAGE CPP #-}

module Web.Twitter.General
    ( ApiType (..)
    , api
    , stream
    , rest
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Exception.Lifted (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, Value (..), (.:))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (mk)
import Data.Conduit (MonadResource, ResumableSource, MonadThrow (..), MonadBaseControl, ($$+-))
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit
import Network.HTTP.Types
import Web.Authenticate.OAuth

import Web.Twitter.Core
import Web.Twitter.Internal.Query
import Web.Twitter.Internal.Types
import Web.Twitter.Internal.Util

#ifdef DEBUG
import Control.Exception (try, SomeException, throwIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit (Conduit, yield, awaitForever, bracketP)
import qualified System.IO as IO
#endif

type ApiName = String
type Endpoint = String

data ApiType = REST
             | UserStream

endpoint :: ApiType -> ApiName -> Endpoint
endpoint REST name = "https://api.twitter.com/1.1/" ++ name ++ ".json"
endpoint UserStream name = "https://userstream.twitter.com/1.1/" ++ name ++ ".json"

api :: (MonadResource m, MonadBaseControl IO m)
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
#ifdef DEBUG
    http signed man `catch` logger `catch` rethrowException
#else
    http signed man `catch` rethrowException
#endif
#ifdef DEBUG
  where
    logger :: MonadResource m => SomeException -> TwitterT m a
    logger exc = liftIO $ IO.withFile "debug.log" IO.AppendMode $ \h -> do
        IO.hPutStrLn h (endpoint ty name ++ BSC.unpack (renderQuery' query))
        IO.hPutStrLn h $ show exc
        throwIO exc
#endif

rethrowException :: MonadThrow m => HttpException -> TwitterT m a
rethrowException exc@(StatusCodeException _ headers _) =
    maybe (monadThrow exc) (rethrow . decodeBody) $ lookup (mk "X-Response-Body-Start") headers
  where
    rethrow = either (const $ monadThrow exc) monadThrow
    decodeBody = eitherDecodeStrictWith errors
    errors (Object o) = TwitterErrors
        <$> (map fromRaw
            <$> o .: "errors"
            )
        <|> UnknownTwitterError
        <$> o .: "errors"
    errors v = fail $ show v

rethrowException exc = monadThrow exc

stream :: (MonadResource m, MonadBaseControl IO m, FromJSON a)
          => ApiType
          -> ApiName
          -> Method
          -> Query
          -> TwitterT m (ResumableSource (TwitterT m) a)
stream ty name mth query = do
    res <- responseBody <$> api ty name mth query
#ifdef DEBUG
    res' <- res $=+ conduitLog (endpoint ty name ++ BSC.unpack (renderQuery' query))
    res' $=+ conduitFromJSON
#else
    res $=+ conduitFromJSON
#endif

#ifdef DEBUG
conduitLog :: MonadResource m => String -> Conduit ByteString m ByteString
conduitLog url = bracketP (try $ IO.openBinaryFile "debug.log" IO.AppendMode) release go
  where
    release :: Either SomeException IO.Handle -> IO ()
    release (Left _) = return ()
    release (Right h) = do
        liftIO $ BSC.hPutStrLn h ""
        IO.hClose h

    go :: MonadResource m => Either SomeException IO.Handle -> Conduit ByteString m ByteString
    go (Left _) = awaitForever yield
    go (Right h) = do
        liftIO $ IO.hPutStrLn h url
        awaitForever $ \bs -> liftIO (BSC.hPut h bs) >> yield bs
#endif

rest :: (MonadResource m, MonadBaseControl IO m, FromJSON a)
     => ApiType
     -> ApiName
     -> Method
     -> Query
     -> TwitterT m a
rest ty name mth query = do
    src <- stream ty name mth query
    ma <- src $$+- CL.head
    maybe (monadThrow $ JsonParseError "nothing parsed") return ma
