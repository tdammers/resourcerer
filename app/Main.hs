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
import Web.Resourcerer.Serve (routeResources, jsonHandler, multiHandler)
import Web.Resourcerer.MultiDocument
import Data.Default (def)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

textToLBS :: Text -> LBS.ByteString
textToLBS = LUTF8.fromString . Text.unpack

testResource :: Text -> [(Text, MultiDocument)] -> IO (Resource MultiDocument)
testResource name items = do
    db <- newIORef (HashMap.fromList items)
    return def
        { collectionName = name
        , listMay = Just $ \_ -> HashMap.toList <$> readIORef db
        , findMay = Just $ \i -> HashMap.lookup i <$> readIORef db
        -- , createMay = Nothing
        -- , storeMay = Just $ \i val ->
        --     atomicModifyIORef' db $ \items ->
        --         let oldVal = HashMap.lookup i items
        --             items' = HashMap.insert i val items
        --             result = case oldVal of
        --                 Nothing -> Created
        --                 Just _ -> Updated
        --         in (items', result)
        -- , deleteMay = Just $ \i ->
        --     atomicModifyIORef' db $ \items ->
        --         let oldVal = HashMap.lookup i items
        --             items' = HashMap.delete i items
        --             result = case oldVal of
        --                 Nothing -> NothingToDelete
        --                 Just _ -> Deleted
        --         in (items', result)
        }

app :: Resource MultiDocument -> Application
app resource = 
    routeResources
        [multiHandler resource]
        []

nameMulti :: Text -> MultiDocument
nameMulti name =
    MultiDocument
        (Just $ toJSON name)
        [("text/plain", return $ textToLBS name)
        ]

main :: IO ()
main = do
    resource <- testResource "items"
                    [ ("john", nameMulti "John Doe")
                    , ("jane", nameMulti "Jane Doe")
                    , ("anna", nameMulti "Anna Andersson")
                    , ("gabi", nameMulti "Gabi Mustermann")
                    ]
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (app resource)
