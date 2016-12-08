{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE LambdaCase #-}
module Main where

import Praglude

import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Web.Resourcerer.Resource ( Resource (..)
                                , ListSpec (..)
                                , StoreResult (..)
                                , DeleteResult (..)
                                , ListSpec (..)
                                )
import Web.Resourcerer.Serve (resourceToApplication)
import Text.Read (readMaybe)

makeRootResource :: IO Resource
makeRootResource = do
    let things :: HashMap Text Value
        things = fromPairs
            [ ("apple", object ["color" ~> asText "green"])
            , ("banana", object ["color" ~> asText "yellow"])
            ]
    thingsRef <- newIORef things

    let thingResource thing =
            def { getStructuredBody = Just $ return thing }

    let integerResource :: Integer -> Resource
        integerResource i =
            def { getStructuredBody = Just $ return (toJSON i) }

    let integersResource =
            def { getChildren = Just $ \listSpec -> do
                    let first = listOffset listSpec
                        last = pred $ first + listCount listSpec
                        range = [first..last]
                    return
                        [ (s (show i), integerResource i)
                        | i <- range
                        ]
                , getChild = Just $ \name -> do
                    print name
                    return (integerResource <$> readMaybe (s name))
                }

    let thingsResource =
            def { getChildren =
                    Just $ \listSpec -> do
                         things <- take (fromIntegral $ listCount listSpec)
                                 . drop (fromIntegral $ listOffset listSpec)
                                 -- TODO: respect ordering from listSpec
                                 . pairs
                                <$> readIORef thingsRef
                         return $ map (second thingResource) things
                , getChild =
                    Just $ \name -> do
                        fmap thingResource . lookup name <$> readIORef thingsRef
                , storeChild =
                    Just $ \name value -> do
                        if name == ""
                            then return StoreFailedInvalidKey
                            else do
                                modifyIORef' thingsRef $ insert name value
                                return $ Stored name (thingResource value)
                , deleteChild =
                    Just $ \name -> do
                        found <- atomicModifyIORef' thingsRef $ \m ->
                            (delete name m, member name m)
                        return $ if found
                            then Deleted name
                            else DeleteFailedDoesNotExist
                }

    return $
        def { getChildren =
                Just $ \listSpec -> do
                    return
                        [ ("things", thingsResource)
                        , ("integers", integersResource)
                        ]
            , getChild = Just $ \case
                "things" -> return $ Just thingsResource
                "integers" -> return $ Just integersResource
                _ -> return Nothing
            }

main :: IO ()
main = do
    resource <- makeRootResource
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (resourceToApplication resource)
