{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
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
                                , ListSpec (..)
                                , defResource
                                , mount
                                )
import Web.Resourcerer.Serve (resourceToApplication)
import Data.Default (def)
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson (ToJSON, FromJSON, toJSON, fromJSON, (.=))
import qualified Data.Aeson as JSON
import Data.Aeson.Helpers (fromJSONMay, (.==))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import GHC.Generics

textToLBS :: Text -> LBS.ByteString
textToLBS = LUTF8.fromString . Text.unpack

data Fruit =
    Fruit
        { fruitName :: Text
        , fruitMessage :: Text
        }
        deriving (Show, Eq, Generic)

instance ToJSON Fruit where

instance FromJSON Fruit where

fruitResource :: Fruit -> Resource
fruitResource fruit =
    defResource
        { getStructuredBody = return $ toJSON fruit
        , getDigestBody = return $ JSON.object [ "fruitName" .= fruitName fruit ]
        }


makeFruitResource :: [(Text, Fruit)] -> IO Resource
makeFruitResource fruit = do
    fruitRef <- newIORef $ HashMap.fromList fruit
    return $ defResource
        { listChildren = listIORef fruitRef
        , getChild = getIORef fruitRef
        }

listIORef :: IORef (HashMap Text Fruit) -> ListSpec -> IO [(Text, Resource)]
listIORef fruit spec =
    HashMap.toList . fmap fruitResource <$> readIORef fruit

getIORef :: IORef (HashMap Text Fruit) -> Text -> IO (Maybe Resource)
getIORef fruit name =
    fmap fruitResource . HashMap.lookup name <$> readIORef fruit


makeRootResource :: IO Resource
makeRootResource = do
    let fruit =
            [ ("apple", Fruit "Apple" "I'm an apple, so what.")
            ]
    fruitResource <- makeFruitResource fruit
    return $ mount "fruit" fruitResource defResource

main :: IO ()
main = do
    resource <- makeRootResource
    port <- fromMaybe 5000 . fmap read <$> lookupEnv "PORT"
    run port (resourceToApplication resource)
