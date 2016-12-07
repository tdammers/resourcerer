{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}
module Web.Resourcerer.Api
where

import Praglude

import Web.Resourcerer.Resource ( Resource (..)
                                , CreateResult (..)
                                , StoreResult (..)
                                , DeleteResult (..)
                                )
import Web.Resourcerer.Mime
import qualified Data.Aeson as JSON
import Control.Monad.Except (MonadError (..), ExceptT, runExceptT, throwError)

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

data ApiError = DoesNotExistError
              | AlreadyExistsError
              | UnsupportedOperationError
              | UnsupportedMediaTypeError
              | InvalidRequestError
              deriving (Generic, Show)

instance Exception ApiError where

type Api = ExceptT ApiError (StateT ApiContext IO)

runApi :: Api a -> ApiContext -> IO (Either ApiError a)
runApi = evalStateT . runExceptT

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
    if null p
        then getResourceSelf resource
        else getResourceChild resource

jsonToAList :: Value -> [(Text, Value)]
jsonToAList (JSON.Object m) = pairs m
jsonToAList JSON.Null = []
jsonToAList x = [("value", x)]

resourceDigest :: Resource -> IO Value
resourceDigest resource = do
    let method = fromMaybe (return JSON.Null) $ getStructuredBody resource
    liftIO method

maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e = maybe (throwError e) return

withChild :: (Resource -> Api a) -> Resource -> Api a
withChild go resource = do
    childName <- consumePathItem
    method <- maybeError DoesNotExistError $ getChild resource
    child <- maybeError DoesNotExistError =<< liftIO (method childName)
    go child

consumePathItem :: Api Text
consumePathItem = do
    use remainingPath >>= \case
        [] -> throwError DoesNotExistError
        (cur:remaining) -> do
            consumedPath %= (<> [cur])
            remainingPath .= remaining
            return cur

getResourceStructuredBody :: Resource -> Api Value
getResourceStructuredBody resource =
    case getStructuredBody resource of
        Nothing -> return JSON.Null
        Just method -> liftIO method

getResourceSelf :: Resource -> Api ApiResponse
getResourceSelf resource = do
    body <- jsonToAList <$> getResourceStructuredBody resource
    let childrenMethod = fromMaybe (const $ return []) $ getChildren resource
    children <- liftIO $ childrenMethod def
    children' <- forM children $ \(name, child) -> do
        val <- liftIO $ resourceDigest child
        return (name, val)
    let body' = object $ body <> children'
    return $ StructuredBody body'

getResourceChild :: Resource -> Api ApiResponse
getResourceChild = withChild getResource

postResource :: Resource -> Api ApiResponse
postResource resource = do
    p <- use remainingPath
    if null p
        then postResourceSelf resource
        else postResourceChild resource

postResourceSelf :: Resource -> Api ApiResponse
postResourceSelf resource = do
    create <- maybeError UnsupportedOperationError $ createChild resource
    getBody <- use postedBody
    body <- maybeError UnsupportedMediaTypeError =<< (view postedValue <$> liftIO getBody)
    liftIO (create body) >>= \case
        CreateFailedAlreadyExists ->
            throwError AlreadyExistsError
        Created name childResource -> do
            body <- getResourceStructuredBody childResource
            return . StructuredBody . object $
                [ "id" ~> name
                , "value" ~> body
                ]
            
postResourceChild :: Resource -> Api ApiResponse
postResourceChild = withChild postResource

putResource :: Resource -> Api ApiResponse
putResource resource = do
    p <- use remainingPath
    case p of
        [] -> throwError DoesNotExistError
        [x] -> putResourceSelf resource x
        xs -> putResourceChild resource

putResourceSelf :: Resource -> Text -> Api ApiResponse
putResourceSelf resource name = do
    store <- maybeError UnsupportedOperationError $ storeChild resource
    getBody <- use postedBody
    body <- maybeError UnsupportedMediaTypeError =<< (view postedValue <$> liftIO getBody)
    liftIO (store name body) >>= \case
        StoreFailedInvalidKey ->
            throwError InvalidRequestError
        Stored name childResource -> do
            body <- getResourceStructuredBody childResource
            return . StructuredBody . object $
                [ "id" ~> name
                , "value" ~> body
                ]

putResourceChild :: Resource -> Api ApiResponse
putResourceChild = withChild putResource

deleteResource :: Resource -> Api ApiResponse
deleteResource resource = do
    p <- use remainingPath
    case p of
        [] -> throwError DoesNotExistError
        [x] -> deleteResourceSelf resource x
        xs -> withChild deleteResource resource

deleteResourceSelf :: Resource -> Text -> Api ApiResponse
deleteResourceSelf resource name = do
    delete <- maybeError UnsupportedOperationError $ deleteChild resource
    liftIO (delete name) >>= \case
        DeleteFailedDoesNotExist ->
            throwError DoesNotExistError
        Deleted name ->
            return . StructuredBody . object $
                [ "id" ~> name
                ]
