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
import Web.Resourcerer.Hateoas (hateoas)
import qualified Web.Resourcerer.Mime as Mime

-- | Typed HTTP request method.
data Method = GET
            | HEAD
            | POST
            | PUT
            | PATCH
            | DELETE
            | OPTIONS
            | OtherMethod ByteString -- ^ fallback for unknown methods
    deriving (Show, Read)

methodFromBS :: ByteString -> Method
methodFromBS "GET" = GET
methodFromBS "HEAD" = HEAD
methodFromBS "POST" = POST
methodFromBS "PUT" = PUT
methodFromBS "PATCH" = PATCH
methodFromBS "DELETE" = DELETE
methodFromBS "OPTIONS" = OPTIONS
methodFromBS m = OtherMethod m

-- | An HTTP request body.
data PostedBody =
    PostedBody
        { _postedSource :: LByteString -- ^ raw body as posted
        , _postedType :: MimeType -- ^ MIME type (as per @Content-type:@)
        , _postedValue :: Maybe Value -- ^ parsed body
        }

makeLenses ''PostedBody

instance Default PostedBody where
    def = PostedBody "" "application/octet-stream" Nothing

-- | State type for the 'Api' monad
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

instance Default ApiContext where
    def = ApiContext
            { _consumedPath = []
            , _remainingPath = []
            , _queryParams = AList []
            , _method = GET
            , _accept = []
            , _postedBody = return def
            }

-- | Things that can be converted from a query parameter
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

-- | A protocol-agnostic response as returned from an API
data ApiResponse = StructuredBody Value
                 | BinaryBody MimeType LByteString

-- | RESTful error conditions
data ApiError = DoesNotExistError
              | AlreadyExistsError
              | UnsupportedOperationError
              | UnsupportedMediaTypeError
              | InvalidRequestError
              deriving (Generic, Show)

instance Exception ApiError where

-- | The 'Api' monad stack.
type Api = ExceptT ApiError (StateT ApiContext IO)

-- | Execute an 'Api' action against an initial state, returning either an
-- error or a result.
runApi :: Api a -> ApiContext -> IO (Either ApiError a)
runApi = evalStateT . runExceptT

-- | Run a resource as a handler in the 'Api' monad.
runResource :: Resource -> Api ApiResponse
runResource resource = do
    use method >>= \case
        GET -> getResource resource
        POST -> postResource resource
        PUT -> putResource resource
        DELETE -> deleteResource resource
        _ -> throwError UnsupportedOperationError

-- | Get a value from a query string parameter; fail to 'Maybe'.
paramMay :: FromQueryParam a => Text -> Api (Maybe a)
paramMay paramName = do
    rawValue <- join . lookup paramName <$> use queryParams
    return $ fromQueryParam rawValue

-- | Get a value from a query string parameter; fail by 'throwError'.
param :: FromQueryParam a => Text -> Api a
param paramName =
    maybeError InvalidRequestError =<< paramMay paramName

-- | Get a value from a query string parameter; fall back to a default value on
-- failure.
paramDef :: FromQueryParam a => a -> Text -> Api a
paramDef d paramName =
    fromMaybe d <$> paramMay paramName

-- | Force a JSON 'Value' to an association list.
-- If the value is an object, extract its pairs; if it's 'JSON.Null', make an
-- empty association list; otherwise, make a singleton association list with
-- the key @"value"@ bound to the original 'Value'.
jsonToAList :: Value -> [(Text, Value)]
jsonToAList (JSON.Object m) = pairs m
jsonToAList JSON.Null = []
jsonToAList x = [("value", x)]

-- | Get a digest value from a resource.
-- Currently an alias for just getting the structured body.
resourceDigest :: Resource -> IO Value
resourceDigest resource = do
    let method = fromMaybe (return JSON.Null) $ getStructuredBody resource
    liftIO method

-- | Turn maybes into 'MonadError' actions: 'Nothing' calls 'throwError' with 
-- the given error value, @Just a@ 'return's @a@.
maybeError :: MonadError e m => e -> Maybe a -> m a
maybeError e = maybe (throwError e) return

-- | @withChild action resource@ consumes one path item, uses it to fetch the
-- appropriate child resource, and calls @action@ on this child resource.
-- Failures are 'throwError'ed appropriately.
withChild :: (Resource -> Api a) -> Resource -> Api a
withChild go resource = do
    childName <- consumePathItem
    method <- maybeError DoesNotExistError $ getChild resource
    child <- maybeError DoesNotExistError =<< liftIO (method childName)
    go child

-- | Consume and return one path item from the remaining (unconsumed) request
-- path.
consumePathItem :: Api Text
consumePathItem = do
    use remainingPath >>= \case
        [] -> throwError DoesNotExistError
        (cur:remaining) -> do
            consumedPath %= (<> [cur])
            remainingPath .= remaining
            return cur

-- | Read a 'ListSpec' from the request context. This is needed for 'GET'
-- requests only, as these are the only ones that allow pagination.
getListSpec :: Api ListSpec
getListSpec = do
    count <- paramDef (listCount def) "count"
    offset <- paramDef (listOffset def) "offset"
    return $ def
        { listCount = count
        , listOffset = offset
        }

-- | Helper function to get a structured body from a 'Resource'.
getResourceStructuredBody :: Resource -> Api Value
getResourceStructuredBody resource =
    case getStructuredBody resource of
        Nothing -> return JSON.Null
        Just method -> liftIO method

-- | Handle a 'GET' request on a 'Resource'.
getResource :: Resource -> Api ApiResponse
getResource resource = do
    p <- use remainingPath
    if null p
        then getResourceSelf resource
        else getResourceChild resource

-- | Handle a 'GET' request on a 'Resource' endpoint.
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


-- | Handle a 'GET' request on a 'Resource' endpoint, serving a structured
-- body.
getResourceSelfStructured :: Resource -> Api ApiResponse
getResourceSelfStructured resource = do
    body <- jsonToAList <$> getResourceStructuredBody resource
    listSpec <- getListSpec
    selfPath <- use consumedPath
    let parentPath = dropEnd 1 selfPath
        selfName = case takeEnd 1 selfPath of
                    [] -> ""
                    (x:_) -> x
    let childrenMethod = fromMaybe (const $ return []) $ getChildren resource
    children <- liftIO $ childrenMethod listSpec
    children' <- forM children $ \(name, child) -> do
        val <- liftIO $ resourceDigest child
        return . object $
            jsonToAList val <>
            (hateoas def name selfPath)
    let hateoasProps = hateoas def selfName parentPath
        body' = object $
            body <>
            ["_items" ~> children'] <>
            hateoasProps
    return $ StructuredBody body'

-- | Handle a 'GET' request on a 'Resource', recursing into a child resource.
getResourceChild :: Resource -> Api ApiResponse
getResourceChild = withChild getResource

-- | Handle a 'POST' request
postResource :: Resource -> Api ApiResponse
postResource resource = do
    p <- use remainingPath
    if null p
        then postResourceSelf resource
        else postResourceChild resource

-- | Handle a 'POST' request on an endpoint
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

-- | Dispatch a 'POST' request to a child resource
postResourceChild :: Resource -> Api ApiResponse
postResourceChild = withChild postResource

-- | Handle a 'PUT' request
putResource :: Resource -> Api ApiResponse
putResource resource = do
    p <- use remainingPath
    case p of
        -- Note that the "end of path" case is an error, because PUT is handled
        -- by the parent resource, not the the resource itself, as the latter
        -- doesn't exist yet.
        [] -> throwError DoesNotExistError
        [x] -> putResourceSelf resource x
        xs -> putResourceChild resource

-- | Handle a 'PUT' request on the parent resource
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

-- | Dispatch a 'PUT' request to a child resource
putResourceChild :: Resource -> Api ApiResponse
putResourceChild = withChild putResource

-- | Handle a 'DELETE' request
deleteResource :: Resource -> Api ApiResponse
deleteResource resource = do
    p <- use remainingPath
    case p of
        -- Just like with @PUT@, the @DELETE@ request is handled by the parent,
        -- so an empty path is an error.
        [] -> throwError DoesNotExistError
        [x] -> deleteResourceSelf resource x
        xs -> withChild deleteResource resource

-- | Handle a 'DELETE' request on the parent resource
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
