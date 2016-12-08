{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
module Web.Resourcerer.Api
where

import Praglude

import Web.Resourcerer.Resource ( Resource (..)
                                , CreateResult (..)
                                , StoreResult (..)
                                , DeleteResult (..)
                                , ListSpec (..)
                                )
import Web.Resourcerer.Mime
import qualified Data.Aeson as JSON
import Control.Monad.Except (MonadError (..), ExceptT, runExceptT, throwError)
import Text.Read (readMaybe)
import Web.Resourcerer.Mime (MimeType)
import qualified Web.Resourcerer.Mime as Mime

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

instance Default PostedBody where
    def = PostedBody "" "application/octet-stream" Nothing

data ApiContext =
    ApiContext
        { _consumedPath :: [Text]
        , _remainingPath :: [Text]
        , _queryParams :: AList Text (Maybe Text)
        , _method :: Method
        , _accept :: [MimeType]
        , _postedBody :: IO PostedBody
        }

makeLenses ''ApiContext

class FromQueryParam a where
    fromQueryParam :: Maybe Text -> Maybe a

instance FromQueryParam Text where
    fromQueryParam = id

instance FromQueryParam LText where
    fromQueryParam = fmap s

instance FromQueryParam String where
    fromQueryParam = fmap s

instance FromQueryParam ByteString where
    fromQueryParam = fmap s

instance FromQueryParam LByteString where
    fromQueryParam = fmap s

instance FromQueryParam Int where
    fromQueryParam = (>>= readMaybe) . fmap s

instance FromQueryParam Integer where
    fromQueryParam = (>>= readMaybe) . fmap s

instance FromQueryParam Double where
    fromQueryParam = (>>= readMaybe) . fmap s

instance FromQueryParam () where
    fromQueryParam = const . Just $ ()

instance FromQueryParam Bool where
    fromQueryParam = fmap (/= "")

instance Default ApiContext where
    def = ApiContext
            { _consumedPath = []
            , _remainingPath = []
            , _queryParams = AList []
            , _method = GET
            , _accept = []
            , _postedBody = return def
            }

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

paramMay :: FromQueryParam a => Text -> Api (Maybe a)
paramMay paramName = do
    rawValue <- join . lookup paramName <$> use queryParams
    return $ fromQueryParam rawValue

param :: FromQueryParam a => Text -> Api a
param paramName =
    maybeError InvalidRequestError =<< paramMay paramName

paramDef :: FromQueryParam a => a -> Text -> Api a
paramDef d paramName =
    fromMaybe d <$> paramMay paramName

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

getListSpec :: Api ListSpec
getListSpec = do
    count <- paramDef (listCount def) "count"
    offset <- paramDef (listOffset def) "offset"
    return $ def
        { listCount = count
        , listOffset = offset
        }

getResource :: Resource -> Api ApiResponse
getResource resource = do
    p <- use remainingPath
    if null p
        then getResourceSelf resource
        else getResourceChild resource

getResourceStructuredBody :: Resource -> Api Value
getResourceStructuredBody resource =
    case getStructuredBody resource of
        Nothing -> return JSON.Null
        Just method -> liftIO method

getResourceSelf :: Resource -> Api ApiResponse
getResourceSelf resource = do
    accepted <- use accept
    let typedBodyGetters = do
            acceptable <- accepted
            (ty, getter) <- getTypedBodies resource
            if Mime.isMatch ty acceptable
                then [(ty, getter)]
                else []
    case typedBodyGetters of
        [] -> getResourceSelfStructured resource
        ((ty, getter):_) -> BinaryBody ty <$> liftIO getter


getResourceSelfStructured :: Resource -> Api ApiResponse
getResourceSelfStructured resource = do
    body <- jsonToAList <$> getResourceStructuredBody resource
    listSpec <- getListSpec
    let childrenMethod = fromMaybe (const $ return []) $ getChildren resource
    children <- liftIO $ childrenMethod listSpec
    children' <- forM children $ \(name, child) -> do
        val <- liftIO $ resourceDigest child
        return . object $ jsonToAList val <> ["_id" ~> name]
    let body' = object $ body <> ["_items" ~> children']
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
