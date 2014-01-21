{-# LANGUAGE OverloadedStrings #-}

module Favorites where

import Test.Hspec
import Web.Twitter

import Resources
import Util

runFavoritesTests :: OAuth -> AccessToken -> AccessToken -> Spec
runFavoritesTests oauth token1 token2 = do
    describe "favorites/create" $ do
        it "fav" $ do
            text <- getRandomText
            withTweetFrom oauth token1 text $ \status -> do
                status' <- runTwitter oauth token2 $ createFavorite (statusId status) Nothing
                statusFavoriteCount status' `shouldBe` Just 1
