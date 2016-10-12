{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Web.Resourcerer.Serve
( routeResources
, multiHandler
, jsonHandler
)
where

import Web.Resourcerer.Resource
        ( Resource (..)
        , ListSpec (..)
        , StoreResult (..)
        , DeleteResult (..)
        , StoreResult (..)
        , mapResource
        )
import Web.Resourcerer.Serve.Exceptions (HttpException (..))
import Web.Resourcerer.Serve.Responses
        ( malformedInputResponse
        , deletedResponse
        , conflictResponse
        , notFoundResponse
        , methodNotAllowedResponse
        , notAcceptableResponse
        , unsupportedMediaTypeResponse
        , responseJSON
        , exceptionResponse
        )
import Web.Resourcerer.MultiDocument (MultiDocument (..), selectView)
import Web.Resourcerer.Mime (MimeType (..))
import Web.Resourcerer.Hateoas (hateoasWrap)
import qualified Web.Resourcerer.Mime as Mime
import qualified Data.Text
import Data.Text (Text)
import Network.Wai ( responseLBS
                   , requestMethod
                   , requestHeaders
                   , mapResponseStatus
                   , pathInfo
                   , lazyRequestBody
                   , Application
                   , Response (..)
                   , Request
                   , ResponseReceived
                   )
import Network.HTTP.Types ( Status
                          , status200
                          , status201
                          , status204
                          , Header
                          )
import Data.Aeson ( Value
                  , ToJSON (..)
                  , FromJSON (..)
                  , (.=)
                  )
import qualified Data.Aeson as JSON
import Data.Aeson.Helpers ( (.==), fromJSONMay )
import qualified Data.Aeson.Helpers as JSON
import Data.Default (def)
import Data.Monoid ( (<>) )
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (throw, catch)

routeResources :: [(Text, [Text] -> Application)] -> [Text] -> Application
routeResources resources parentPath request respond =
    go `catch` handle
    where
        handle :: HttpException -> IO ResponseReceived
        handle e = respond $ exceptionResponse e

        go :: IO ResponseReceived
        go = do
            case pathInfo request of
                [] -> respond $ responseJSON
                        status200
                        []
                        [ JSON.object
                            [ "collections" .=
                                [ hateoasWrap
                                    [("self", joinPath (parentPath ++ [name]))] $
                                    JSON.object [ "name" .= name ]
                                | (name, _) <- resources
                                ]
                            ]
                        ]
                (ident:rest) -> do
                    case lookup ident resources of
                        Nothing -> throw NotFound
                        Just handler -> do
                            let request' = request { pathInfo = rest }
                            handler parentPath request' respond

joinPath :: [Text] -> Text
joinPath items = "/" <> (mconcat . List.intersperse "/" $ items)

getResource :: (Maybe (IO b)) -> (b -> Response) -> IO Response
getResource actionMay buildResponse =
    case actionMay of
        Nothing -> return methodNotAllowedResponse
        Just action -> buildResponse <$> action

itemToJSON :: ToJSON a => [Text] -> Text -> Text -> a -> JSON.Value
itemToJSON parentPath collectionName itemID item =
    hateoasWrap
            [ ("self", joinPath $ parentPath ++ [collectionName, itemID])
            , ("parent", joinPath $ parentPath ++ [collectionName])
            ]
        $ toJSON item

buildMultiCollectionResponse :: [Text] -> Resource MultiDocument -> [(Text, MultiDocument)] -> Response
buildMultiCollectionResponse parentPath resource items =
    responseJSON status200 [("Content-type", "application/json")] $
        itemsToJSON asJSON parentPath (collectionName resource) items
    where
        asJSON :: MultiDocument -> JSON.Value
        asJSON md =
            case mdJSON md of
                Just value -> value
                Nothing -> JSON.Null

itemsToJSON :: (a -> JSON.Value) -> [Text] -> Text -> [(Text, a)] -> JSON.Value
itemsToJSON asJSON parentPath collectionName items =
    hateoasWrap
        [ ("self", joinPath $ parentPath ++ [collectionName])
        , ("parent", joinPath parentPath)
        ]
    $ JSON.object
        [ collectionName .=
            [ hateoasWrap
                [ ("self", joinPath $ parentPath ++ [collectionName, i])
                , ("parent", joinPath $ parentPath ++ [collectionName])
                ]
                (asJSON v)
            | (i, v) <- items
            ]
        , "count" .= length items
        ]

requestAccept :: Request -> [MimeType]
requestAccept rq =
    let acceptHeader = fromMaybe "*/*" . lookup "accept" . requestHeaders $ rq
    in Mime.parseAccept acceptHeader

requestType :: Request -> MimeType
requestType rq =
    let contentTypeHeader = fromMaybe "*/*" . lookup "content-type" . requestHeaders $ rq
    in Mime.parse contentTypeHeader

getMulti :: [MimeType] -> [Text] -> Text -> Text -> MultiDocument -> (Maybe (MimeType, LBS.ByteString))
getMulti accepts parentPath collectionName itemID multi =
    let jsonViewMay = do
            JSON.encode . itemToJSON parentPath collectionName itemID <$> mdJSON multi
        jsonHandlers = case jsonViewMay of
            Nothing ->
                []
            Just jsonView ->
                [ ("text/json", jsonView)
                , ("application/json", jsonView)
                ]
        handlers = jsonHandlers ++ mdViews multi
    in case selectView accepts handlers of
        Nothing -> Nothing
        Just (mimeType, body) -> Just (mimeType, body)

checkStoreResult :: StoreResult a -> (Text, a, Status)
checkStoreResult StoreRejectedWrongType = throw UnsupportedMediaType
checkStoreResult StoreRejectedMalformed = throw MalformedInput
checkStoreResult StoreRejectedExists = throw Conflict
checkStoreResult StoreRejectedDoesNotExist = throw NotFound
checkStoreResult (Created itemID item) = (itemID, item, status201)
checkStoreResult (Updated itemID item) = (itemID, item, status200)

multiToResponse :: [MimeType] -> Resource MultiDocument -> [Text] -> Text -> MultiDocument -> Status -> Response
multiToResponse accepts resource parentPath itemID item status =
    let viewMay = getMulti
            accepts
            parentPath
            (collectionName resource)
            itemID
            item
        (mimeType, body) = fromMaybe (throw NotAcceptable) viewMay
    in responseLBS
        status
        [("Content-type", Mime.pack mimeType)]
        body

multiGET :: Resource MultiDocument -> [Text] -> Application
multiGET resource parentPath request respond = do
    let accepts = requestAccept request
    case pathInfo request of
        [] -> respond =<< getResource
                (listMay resource <*> pure def)
                (buildMultiCollectionResponse parentPath resource)
        [itemID] -> do
            let find = fromMaybe (throw NotFound) $ findMay resource
            item <- fromMaybe (throw NotFound) <$> find itemID
            respond $
                multiToResponse
                    accepts
                    resource
                    parentPath
                    itemID
                    item
                    status200
        _ -> throw NotFound

multiFromRequestBody :: MimeType -> LBS.ByteString -> MultiDocument
multiFromRequestBody mimeType body =
    if Mime.isMatch mimeType "text/json" ||
       Mime.isMatch mimeType "application/json"
        then
            let parseResult :: Maybe JSON.Value
                parseResult = JSON.decode body
            in case parseResult of
                Nothing -> throw MalformedInput
                Just jsonItem -> MultiDocument (Just jsonItem) []
        else
            MultiDocument Nothing [(mimeType, body)]

multiPOST :: Resource MultiDocument -> [Text] -> Application
multiPOST resource parentPath request respond =
    case (pathInfo request, createMay resource) of
        ([], Just create) -> do
            body <- lazyRequestBody request
            let mimeType = requestType request
                accepts = requestAccept request
                item = multiFromRequestBody mimeType body
            storeResult <- create item
            let (itemID, item, status) = checkStoreResult storeResult
            respond $ multiToResponse
                accepts resource parentPath itemID item status
        ([itemID], _) -> throw MethodNotAllowed
        _ -> throw NotFound

multiPUT :: Resource MultiDocument -> [Text] -> Application
multiPUT resource parentPath request respond =
    case (pathInfo request, storeMay resource) of
        ([itemID], Just store) -> do
            body <- lazyRequestBody request
            let mimeType = requestType request
                accepts = requestAccept request
                item = multiFromRequestBody mimeType body
            storeResult <- store itemID item
            let (itemID, item, status) = checkStoreResult storeResult
            respond $ multiToResponse
                accepts resource parentPath itemID item status
        ([], _) -> throw MethodNotAllowed
        _ -> throw NotFound

multiDELETE :: Resource a -> [Text] -> Application
multiDELETE resource parentPath request respond =
    case pathInfo request of
        [] -> throw MethodNotAllowed
        [itemID] -> do
            let delete = fromMaybe (throw MethodNotAllowed) $ deleteMay resource
            delete itemID >>= \case
                NothingToDelete -> throw NotFound
                DeleteRejected -> throw Conflict
                Deleted -> respond deletedResponse
        _ -> throw NotFound

jsonHandler :: (FromJSON a, ToJSON a) => Resource a -> (Text, [Text] -> Application)
jsonHandler =
    multiHandler . mapResource multiFromJSON multiToJSON

multiFromJSON :: ToJSON a => a -> MultiDocument
multiFromJSON val = def { mdJSON = Just (toJSON val) }

multiToJSON :: FromJSON a => MultiDocument -> Maybe a
multiToJSON = (>>= fromJSONMay) . mdJSON

multiHandler :: Resource MultiDocument -> (Text, [Text] -> Application)
multiHandler resource =
    (collectionName resource, handler)
    where
        handler :: [Text] -> Application
        handler parentPath request respond = do
            case requestMethod request of
                "GET" ->
                    multiGET resource parentPath request respond
                "POST" ->
                    multiPOST resource parentPath request respond
                "PUT" ->
                    multiPUT resource parentPath request respond
                "DELETE" ->
                    multiDELETE resource parentPath request respond
                _ -> respond methodNotAllowedResponse
