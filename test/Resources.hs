module Resources where

import Control.Exception.Lifted (bracket)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import Web.Twitter

mkFrom :: OAuth -> AccessToken
       -> ((a -> Twitter b) -> Twitter b)
       -> (a -> IO b) -> IO b
mkFrom oauth token withA = runTwitter oauth token . withA . (liftIO .)

withTweet :: Text -> (Status -> Twitter a) -> Twitter a
withTweet text = bracket
    (update text Nothing Nothing Nothing Nothing Nothing Nothing)
    (flip destroy Nothing . statusId)

withTweetFrom :: OAuth -> AccessToken -> Text -> (Status -> IO a) -> IO a
withTweetFrom oauth token = mkFrom oauth token . withTweet

withRetweet :: StatusId -> (Status -> Twitter a) -> Twitter a
withRetweet sid = bracket
    (retweet sid Nothing)
    (flip destroy Nothing . statusId)

withRetweetFrom :: OAuth -> AccessToken -> StatusId -> (Status -> IO a) -> IO a
withRetweetFrom oauth token = mkFrom oauth token . withRetweet

withDirectMessage :: UserId -> Text -> (DirectMessage -> Twitter a) -> Twitter a
withDirectMessage uid text = bracket
    (newDirectMessage (Left uid) text)
    (flip destroyDirectMessage Nothing . directMessageId)

withDirectMessageFrom :: OAuth -> AccessToken -> UserId -> Text -> (DirectMessage -> IO a) -> IO a
withDirectMessageFrom oauth token = (mkFrom oauth token .) . withDirectMessage
