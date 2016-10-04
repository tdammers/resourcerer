{-#LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Web.Resourcerer.Resource ( Resource (..)
                                , StoreResult (..)
                                , DeleteResult (..)
                                )
import Web.Resourcerer.Serve (routeResources, jsonHandler)
import Data.Default (def)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

testResource :: Text -> [(Text, Text)] -> IO (Resource Text)
testResource name items = do
    db <- newIORef (HashMap.fromList items)
    return def
        { collectionName = name
        , listMay = Just $ \_ -> HashMap.toList <$> readIORef db
        , findMay = Just $ \i -> HashMap.lookup i <$> readIORef db
        , createMay = Nothing
        , storeMay = Just $ \i val ->
            atomicModifyIORef' db $ \items ->
                let oldVal = HashMap.lookup i items
                    items' = HashMap.insert i val items
                    result = case oldVal of
                        Nothing -> Created
                        Just _ -> Updated
                in (items', result)
        , deleteMay = Just $ \i ->
            atomicModifyIORef' db $ \items ->
                let oldVal = HashMap.lookup i items
                    items' = HashMap.delete i items
                    result = case oldVal of
                        Nothing -> NothingToDelete
                        Just _ -> Deleted
                in (items', result)
        }

app :: Resource Text -> Application
app resource = 
    routeResources
        [jsonHandler resource]
        []


main :: IO ()
main = do
    resource <- testResource "items"
                    [ ("john", "John Doe")
                    , ("jane", "Jane Doe")
                    , ("anna", "Anna Andersson")
                    , ("gabi", "Gabi Mustermann")
                    ]
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (app resource)
