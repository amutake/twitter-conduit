module Web.Twitter.Util
    ( readOAuthFromJsonFile
    , readAccessTokenFromJsonFile
    , saveOAuthToJsonFile
    , saveAccessTokenToJsonFile
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (Value (..), (.:), toJSON)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Map (fromList)
import qualified Data.Map as Map
import System.IO (withBinaryFile, hPutStrLn, IOMode (..))
import Web.Authenticate.OAuth (oauthConsumerKey, oauthConsumerSecret, unCredential)

import Web.Twitter.Auth
import Web.Twitter.Internal.Util

readOAuthFromJsonFile :: FilePath -> IO OAuth
readOAuthFromJsonFile path = do
    bs <- BL.readFile path
    either error return $ eitherDecodeWith parser bs
  where
    parser (Object o) = newOAuth
        <$> o .: "consumer_key"
        <*> o .: "consumer_secret"
    parser v = fail $ show v

readAccessTokenFromJsonFile :: FilePath -> IO AccessToken
readAccessTokenFromJsonFile path = do
    bs <- BL.readFile path
    either error return $ eitherDecodeWith parser bs
  where
    parser (Object o) = newAccessToken
        <$> o .: "oauth_token"
        <*> o .: "oauth_token_secret"
    parser v = fail $ show v

saveOAuthToJsonFile :: FilePath -> OAuth -> IO ()
saveOAuthToJsonFile path oauth = withBinaryFile path WriteMode $ \handle -> do
    BL.hPutStr handle $ encodeWith encoder oauth
    hPutStrLn handle ""
  where
    encoder oa = toJSON . fromList $ (
        [ ("consumer_key", BSC.unpack $ oauthConsumerKey oa)
        , ("consumer_secret", BSC.unpack $ oauthConsumerSecret oa)
        ] :: [(String, String)])

saveAccessTokenToJsonFile :: FilePath -> AccessToken -> IO ()
saveAccessTokenToJsonFile path cred = withBinaryFile path WriteMode $ \handle -> do
    BL.hPutStr handle $ encodeWith encoder cred
    hPutStrLn handle ""
  where
    encoder = toJSON
            . Map.mapKeys BSC.unpack . Map.map BSC.unpack
            . fromList . unCredential
