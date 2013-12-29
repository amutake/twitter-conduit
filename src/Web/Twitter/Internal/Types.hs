{-# LANGUAGE DeriveDataTypeable #-}

module Web.Twitter.Internal.Types
    ( TwitterException (..)
    , TwitterErrorResponse (..)
    , TwitterErrorRaw (..)
    , fromRaw
    , Env (..)
    , OAuth
    , AccessToken
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Conduit (Manager)
import Web.Authenticate.OAuth (Credential, OAuth)

-- | <https://dev.twitter.com/docs/error-codes-responses> 2013-11-25 13:06
data TwitterException
    = JsonParseError String
    | TwitterErrors [TwitterErrorResponse]
    | UnknownTwitterError Text
    deriving (Show, Eq, Typeable)

instance Exception TwitterException

data TwitterErrorResponse
    = CouldNotAuthenticate -- ^ error code 32: Could not authenticate you
    | RateLimitExceeded -- ^ error code 88: Rate limit exceeded
    | TokenError -- ^ error code 89: Invalid or expired token
    | OverCapacity -- ^ error code 130: Over capacity
    | ServerError -- ^ error code 131: Internal error
    | DuplicateStatus -- ^ error code 187: Status is a duplicate
    | UnknownError TwitterErrorRaw -- ^ not implemented error
    deriving (Show, Eq, Typeable)

fromRaw :: TwitterErrorRaw -> TwitterErrorResponse
fromRaw raw = trans (twitterErrorCode raw)
  where
    trans 32 = CouldNotAuthenticate
    trans 88 = RateLimitExceeded
    trans 89 = TokenError
    trans 130 = OverCapacity
    trans 131 = ServerError
    trans 187 = DuplicateStatus
    trans _ = UnknownError raw

data TwitterErrorRaw = TwitterErrorRaw
    { twitterErrorCode :: Int
    , twitterErrorMessage :: Text
    } deriving (Show, Eq, Typeable)

instance FromJSON TwitterErrorRaw where
    parseJSON (Object o) = TwitterErrorRaw
        <$> o .: "code"
        <*> o .: "message"
    parseJSON v = fail $ show v

data Env = Env
    { twitterOAuth :: OAuth
    , twitterAccessToken :: AccessToken
    , twitterManager :: Manager
    }

type AccessToken = Credential
