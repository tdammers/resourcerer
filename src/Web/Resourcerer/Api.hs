{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}
module Web.Resourcerer.Api
where

import Praglude

import Web.Resourcerer.Resource (Resource (..))
import Web.Resourcerer.Mime
import qualified Data.Aeson as JSON

data Method = GET | POST | PUT | DELETE | OtherMethod ByteString
    deriving (Show, Read)

methodFromBS :: ByteString -> Method
methodFromBS "GET" = GET
methodFromBS "POST" = POST
methodFromBS "PUT" = PUT
methodFromBS "DELETE" = DELETE
methodFromBS m = OtherMethod m

data PostedBody =
    PostedBody
        { _postedSource :: LByteString
        , _postedType :: MimeType
        , _postedValue :: Maybe Value
        }

makeLenses ''PostedBody

data ApiContext =
    ApiContext
        { _consumedPath :: [Text]
        , _remainingPath :: [Text]
        , _method :: Method
        , _accept :: [MimeType]
        , _postedBody :: IO PostedBody
        }

makeLenses ''ApiContext

data ApiResponse = StructuredBody Value
                 | BinaryBody MimeType LByteString
                 | Stored Text

data ApiError = DoesNotExistError
              | AlreadyExistsError
              | UnsupportedOperationError
              | UnsupportedMediaTypeError
              deriving (Generic, Show)

instance Exception ApiError where

type Api = StateT ApiContext IO

runApi :: Api a -> ApiContext -> IO a
runApi = evalStateT

runResource :: Resource -> Api ApiResponse
runResource resource = do
    use method >>= \case
        GET -> getResource resource
        POST -> postResource resource
        PUT -> putResource resource
        DELETE -> deleteResource resource


getResource :: Resource -> Api ApiResponse
getResource resource = do
    p <- use remainingPath
    if p == []
        then getResourceSelf resource
        else getResourceChild resource

jsonToAList :: Value -> [(Text, Value)]
jsonToAList (JSON.Object m) = pairs m
jsonToAList x = [("value", x)]

resourceDigest :: Resource -> IO Value
resourceDigest resource = do
    let method = fromMaybe (return JSON.Null) $ getStructuredBody resource
    liftIO method

getResourceSelf :: Resource -> Api ApiResponse
getResourceSelf resource = do
    let method = fromMaybe (return $ object []) $ getStructuredBody resource
    body <- jsonToAList <$> liftIO method
    let childrenMethod = fromMaybe (const $ return []) $ getChildren resource
    children <- liftIO $ childrenMethod def
    children' <- forM children $ \(name, child) -> do
        val <- liftIO $ resourceDigest child
        return (name, val)
    let body' = object $ body <> children'
    return $ StructuredBody body'

getResourceChild :: Resource -> Api ApiResponse
getResourceChild resource = do
    childName <- consumePathItem
    method <- maybe (throw DoesNotExistError) return $ getChild resource
    child <- fromMaybe (throw DoesNotExistError) <$> liftIO (method childName)
    getResource child

consumePathItem :: Api Text
consumePathItem = do
    use remainingPath >>= \case
        [] -> throw DoesNotExistError
        (cur:remaining) -> do
            consumedPath %= (<> [cur])
            remainingPath .= remaining
            return cur

postResource :: Resource -> Api ApiResponse
postResource = undefined

putResource :: Resource -> Api ApiResponse
putResource = undefined

deleteResource :: Resource -> Api ApiResponse
deleteResource = undefined
