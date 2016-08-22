{-#LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Web.Resourcerer.Resource (Resource (..))
import Web.Resourcerer.Serve (routeResources, jsonResource)

testResource :: Text -> [(Text, Text)] -> Resource Text
testResource name items =
    Resource
        { listMay = Just $ \_ -> return items
        , findMay = Just $ \i -> return $ lookup i items
        , collectionName = name
        }

app :: Application
app =
    routeResources
        [jsonResource $
            testResource "items"
                [ ("john", "John Doe")
                , ("jane", "Jane Doe")
                , ("anna", "Anna Andersson")
                , ("gabi", "Gabi Mustermann")
                ]
        ]
        []

main :: IO ()
main = do
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port app
