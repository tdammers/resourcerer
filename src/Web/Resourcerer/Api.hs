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
                 | Stored Text

data ApiError = DoesNotExistError
              | AlreadyExistsError
              | UnsupportedOperationError
              | UnsupportedMediaTypeError
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
    if p == []
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

getResourceSelf :: Resource -> Api ApiResponse
getResourceSelf resource = do
    let method = fromMaybe (return JSON.Null) $ getStructuredBody resource
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
    method <- maybeError DoesNotExistError $ getChild resource
    child <- maybeError DoesNotExistError =<< liftIO (method childName)
    getResource child

consumePathItem :: Api Text
consumePathItem = do
    use remainingPath >>= \case
        [] -> throwError DoesNotExistError
        (cur:remaining) -> do
            consumedPath %= (<> [cur])
            remainingPath .= remaining
            return cur

postResource :: Resource -> Api ApiResponse
postResource resource = throwError UnsupportedOperationError

putResource :: Resource -> Api ApiResponse
putResource resource = throwError UnsupportedOperationError

deleteResource :: Resource -> Api ApiResponse
deleteResource resource = throwError UnsupportedOperationError
