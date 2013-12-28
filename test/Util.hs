module Util where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import qualified Data.Text as T
import System.Random (randomRIO)

getRandomText :: IO T.Text
getRandomText = T.pack <$> replicateM 16 (randomRIO ('a', 'z'))
