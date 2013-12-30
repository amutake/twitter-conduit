module Util where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.Text as T
import System.Random (randomRIO)

import Web.Twitter

getRandomText :: IO T.Text
getRandomText = T.pack <$> replicateM 16 (randomRIO ('a', 'z'))

getMe :: OAuth -> AccessToken -> IO User
getMe oauth token = runTwitter oauth token $ verifyCredentials Nothing Nothing
