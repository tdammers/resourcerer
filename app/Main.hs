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
import Data.Aeson (toJSON, fromJSON)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

textToLBS :: Text -> LBS.ByteString
textToLBS = LUTF8.fromString . Text.unpack

interpretMulti :: MultiDocument -> Either StoreResult MultiDocument
interpretMulti doc =
    case mdJSON doc of
        Just val ->
            multiFromJSON val
        Nothing ->
            case selectView ["text/plain"] (mdViews doc) of
                Nothing -> Left StoreRejectedWrongType
                Just (_, body) ->
                    multiFromPlainText . Text.pack . LUTF8.toString $ body

multiFromJSON :: JSON.Value -> Either StoreResult MultiDocument
multiFromJSON val =
    case fromJSON val of
        JSON.Success name -> multiFromPlainText name
        JSON.Error _ -> Left StoreRejectedMalformed

multiFromPlainText :: Text -> Either StoreResult MultiDocument
multiFromPlainText str =
    return MultiDocument
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

testResource :: Text -> [(Text, MultiDocument)] -> IO (Resource MultiDocument)
testResource name items = do
    db <- newIORef (HashMap.fromList items)
    return def
        { collectionName = name
        , listMay = Just $ \_ -> HashMap.toList <$> readIORef db
        , findMay = Just $ \i -> HashMap.lookup i <$> readIORef db
        , createMay = Just $ \rawVal -> do
            let valEither = interpretMulti rawVal
            case valEither of
                Left result ->
                    return result
                Right val -> do
                    let identMay = multiIdentifier val
                    case identMay of
                        Nothing -> return StoreRejectedMalformed
                        Just i -> do
                            atomicModifyIORef' db $ \items ->
                                let oldVal = HashMap.lookup i items
                                    items' = HashMap.insert i val items
                                in case oldVal of
                                    Nothing -> (items', Created i)
                                    Just _ -> (items, StoreRejectedExists)
        , storeMay = Just $ \i rawVal -> do
            let valEither = interpretMulti rawVal
            case valEither of
                Left result ->
                    return result
                Right val ->
                    atomicModifyIORef' db $ \items ->
                        let oldVal = HashMap.lookup i items
                            items' = HashMap.insert i val items
                            result = case oldVal of
                                Nothing -> Created i
                                Just _ -> Updated i
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
        [multiHandler resource]
        []

nameMulti :: Text -> MultiDocument
nameMulti name =
    MultiDocument
        (Just $ toJSON name)
        [("text/plain", textToLBS name)
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
