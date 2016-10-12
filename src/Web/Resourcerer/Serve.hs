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
import Web.Resourcerer.Serve.Responses
        ( malformedInputResponse
        , deletedResponse
        , conflictResponse
        , notFoundResponse
        , methodNotAllowedResponse
        , notAcceptableResponse
        , unsupportedMediaTypeResponse
        , responseJSON
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

routeResources :: [(Text, [Text] -> Application)] -> [Text] -> Application
routeResources resources parentPath request respond = do
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
                Nothing -> respond notFoundResponse
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

storeResultToResponse :: (Text -> Response)
                       -> StoreResult
                       -> Response
storeResultToResponse okResponse =
    \case
        StoreRejectedWrongType ->
            unsupportedMediaTypeResponse
        StoreRejectedMalformed ->
            malformedInputResponse
        StoreRejectedExists ->
            conflictResponse
        StoreRejectedDoesNotExist ->
            notFoundResponse
        Created itemID ->
            okResponse itemID
        Updated itemID ->
            okResponse itemID

multiToResponse :: [MimeType] -> Resource MultiDocument -> [Text] -> Text -> MultiDocument -> Response
multiToResponse accepts resource parentPath itemID item =
    let viewMay = getMulti
            accepts
            parentPath
            (collectionName resource)
            itemID
            item
    in case viewMay of
        Nothing ->
            notAcceptableResponse
        Just (mimeType, body) ->
            responseLBS
                status200
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
            case findMay resource <*> pure itemID of
                Nothing -> respond notFoundResponse
                Just find -> do
                    itemMay <- find
                    case itemMay of
                        Nothing ->
                            respond notFoundResponse
                        Just item ->
                            respond $
                                multiToResponse
                                    accepts
                                    resource
                                    parentPath
                                    itemID item
        _ -> respond notFoundResponse

multiFromRequestBody :: MimeType -> LBS.ByteString -> Maybe MultiDocument
multiFromRequestBody mimeType body =
    if Mime.isMatch mimeType "text/json" ||
       Mime.isMatch mimeType "application/json"
        then
            let parseResult :: Maybe JSON.Value
                parseResult = JSON.decode body
            in case parseResult of
                Nothing -> Nothing
                Just jsonItem ->
                    Just $ MultiDocument (Just jsonItem) []
        else
            Just $ MultiDocument Nothing [(mimeType, body)]

multiPOST :: Resource MultiDocument -> [Text] -> Application
multiPOST resource parentPath request respond =
    case (pathInfo request, createMay resource) of
        ([], Just create) -> do
            body <- lazyRequestBody request
            let mimeType = requestType request
                accepts = requestAccept request
                docResult = multiFromRequestBody mimeType body
            case docResult of
                Nothing -> respond malformedInputResponse
                Just item -> do
                    storeResult <- create item
                    respond $ storeResultToResponse
                        (\itemID -> multiToResponse
                            accepts
                            resource
                            parentPath
                            itemID item) storeResult
        ([itemID], _) -> respond methodNotAllowedResponse
        _ -> respond notFoundResponse

multiPUT :: Resource MultiDocument -> [Text] -> Application
multiPUT resource parentPath request respond =
    case (pathInfo request, storeMay resource) of
        ([itemID], Just store) -> do
            body <- lazyRequestBody request
            let mimeType = requestType request
                accepts = requestAccept request
                docResult = multiFromRequestBody mimeType body
            case docResult of
                Nothing -> respond malformedInputResponse
                Just item -> do
                    storeResult <- store itemID item
                    respond $ storeResultToResponse
                        (\itemID -> multiToResponse
                            accepts
                            resource
                            parentPath
                            itemID item) storeResult
        ([], _) -> respond methodNotAllowedResponse
        _ -> respond notFoundResponse

handleDELETE :: Resource a -> [Text] -> Application
handleDELETE resource parentPath request respond =
    case pathInfo request of
        [] -> respond methodNotAllowedResponse
        [itemID] -> case deleteMay resource of
            Nothing -> respond methodNotAllowedResponse
            Just delete -> do
                result <- delete itemID
                case result of
                    Deleted -> respond deletedResponse
                    NothingToDelete -> respond notFoundResponse
                    DeleteRejected -> respond conflictResponse
        _ -> respond notFoundResponse

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
                    handleDELETE resource parentPath request respond
                _ -> respond methodNotAllowedResponse
