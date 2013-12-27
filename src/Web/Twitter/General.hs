{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.General
    ( api
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

import Web.Twitter.Internal
import Web.Twitter.Util

type ApiName = String
type Endpoint = String

endpoint :: ApiName -> Endpoint
endpoint name = "https://api.twitter.com/1.1/" ++ name ++ ".json"

api :: (MonadResource m)
    => ApiName -- ^ API resource name
    -> Method -- ^ HTTP request method
    -> Query -- ^ Query
    -> TwitterT m (Response (ResumableSource (TwitterT m) ByteString))
api name mth query = do
    env <- ask
    let oauth = twitterOAuth . twitterAuth $ env
        cred = twitterCredential . twitterAuth $ env
        man = twitterManager env
    req <- liftIO $ parseUrl $ endpoint name
    signed <- signOAuth oauth cred req
        { method = mth
        , queryString = renderQuery True query
        }
    http signed man

apiSource :: (MonadResource m, FromJSON a)
          => ApiName
          -> Method
          -> Query
          -> TwitterT m (ResumableSource (TwitterT m) a)
apiSource name mth query = do
    res <- responseBody <$> api name mth query
    res $=+ conduitFromJSON

apiSingle :: (MonadResource m, FromJSON a)
          => ApiName
          -> Method
          -> Query
          -> TwitterT m a
apiSingle name mth query = do
    res <- api name mth query >>= lbsResponse
    let body = responseBody res
    either (monadThrow . ParseError) return $ eitherDecode body
