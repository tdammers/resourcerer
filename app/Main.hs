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
                                , mapResource
                                , ListSpec (..)
                                )
import Web.Resourcerer.Serve (routeResources, jsonHandler, multiHandler)
import Web.Resourcerer.MultiDocument
import Data.Default (def)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson (toJSON, fromJSON, (.=))
import qualified Data.Aeson as JSON
import Data.Aeson.Helpers (fromJSONMay, (.==))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

textToLBS :: Text -> LBS.ByteString
textToLBS = LUTF8.fromString . Text.unpack

interpretMulti :: MultiDocument -> Either (StoreResult MultiDocument) MultiDocument
interpretMulti doc =
    case mdJSON doc of
        Just val ->
            multiFromJSON val
        Nothing ->
            case selectView ["text/plain"] (mdViews doc) of
                Nothing -> Left StoreRejectedWrongType
                Just (_, body) ->
                    return . multiFromPlainText . Text.pack . LUTF8.toString $ body

multiFromJSON :: JSON.Value -> Either (StoreResult MultiDocument) MultiDocument
multiFromJSON val =
    case fromJSON val of
        JSON.Success name -> Right $ multiFromPlainText name
        JSON.Error _ -> Left StoreRejectedMalformed

multiFromPlainText :: Text -> MultiDocument
multiFromPlainText str =
    MultiDocument
        { mdJSON =
            Just $ toJSON str
        , mdViews =
            [ ( "text/plain;charset=utf8"
              , LUTF8.fromString (Text.unpack str)
              )
            ]
        }

multiIdentifier :: MultiDocument -> Maybe Text
multiIdentifier m = do
    json <- mdJSON m
    name <- case fromJSON json of
        JSON.Error _ -> Nothing
        JSON.Success a -> return a
    return $ nameToIdentifier name

nameToIdentifier :: Text -> Text
nameToIdentifier = Text.unwords . take 1 . Text.words . Text.toCaseFold

testResource :: Text -> [(Text, Text)] -> IO (Resource Text)
testResource name items = do
    db <- newIORef (HashMap.fromList items)
    return def
        { collectionName = name
        , listMay = Just $ \_ -> HashMap.toList <$> readIORef db
        , findMay = Just $ \i -> HashMap.lookup i <$> readIORef db
        , createMay = Just $ \val -> do
            let i = nameToIdentifier val
            atomicModifyIORef' db $ \items ->
                let oldVal = HashMap.lookup i items
                    items' = HashMap.insert i val items
                in case oldVal of
                    Nothing -> (items', Created i val)
                    Just _ -> (items, StoreRejectedExists)
        , storeMay = Just $ \i val -> do
            atomicModifyIORef' db $ \items ->
                let oldVal = HashMap.lookup i items
                    items' = HashMap.insert i val items
                    result = case oldVal of
                        Nothing -> Created i val
                        Just _ -> Updated i val
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

app :: Resource MultiDocument -> Application
app resource =
    routeResources
        [ multiHandler resource
        ]
        []

nameMulti :: Text -> MultiDocument
nameMulti name =
    MultiDocument
        (Just $ toJSON name)
        [("text/plain", textToLBS name)
        ]

nameFromMulti :: MultiDocument -> Maybe Text
nameFromMulti (MultiDocument { mdJSON = Just json }) =
    fromJSONMay json
nameFromMulti (MultiDocument { mdViews = views }) =
    Text.pack . LUTF8.toString . snd <$> selectView ["text/plain"] views

main :: IO ()
main = do
    resource <- testResource "items"
                    [ ("john", "John Doe")
                    , ("jane", "Jane Doe")
                    , ("anna", "Anna Andersson")
                    , ("gabi", "Gabi Mustermann")
                    ]
    let multiResource = mapResource nameMulti nameFromMulti resource
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (app multiResource)
