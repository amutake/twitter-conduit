module Resources where

import Control.Exception.Lifted (bracket)
import Data.Text (Text)
import Web.Twitter

withTweet :: Text -> (Status -> Twitter a) -> Twitter a
withTweet text = bracket
    (update text Nothing Nothing Nothing Nothing Nothing Nothing)
    (flip destroy Nothing . statusId)

withTweetFrom :: OAuth -> AccessToken -> Text -> (Status -> IO a) -> IO a
withTweetFrom oauth token text = bracket
    (runTwitter oauth token $ update text Nothing Nothing Nothing Nothing Nothing Nothing)
    (runTwitter oauth token . flip destroy Nothing . statusId)

withRetweetFrom :: OAuth -> AccessToken -> StatusId -> (Status -> IO a) -> IO a
withRetweetFrom oauth token sid = bracket
    (runTwitter oauth token $ retweet sid Nothing)
    (runTwitter oauth token . flip destroy Nothing . statusId)
