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
                                , defResource
                                , mount
                                )
import Web.Resourcerer.Serve (resourceToApplication)
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

testResource :: IO Resource
testResource = do
    return $ mount "things" defResource defResource

main :: IO ()
main = do
    resource <- testResource
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (resourceToApplication resource)
