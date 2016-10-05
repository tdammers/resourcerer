{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Web.Resourcerer.Serve
( routeResources
, jsonHandler
, multiHandler
)
where

import Web.Resourcerer.Resource
        ( Resource (..)
        , ListSpec (..)
        , StoreResult (..)
        , DeleteResult (..)
        )
import Web.Resourcerer.MultiDocument (MultiDocument (..), selectView)
import Web.Resourcerer.Mime (MimeType (..))
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
                          , status400
                          , status404
                          , status405
                          , status406
                          , status409
                          , Header
                          )
import Data.Aeson ( Value
                  , ToJSON (..)
                  , FromJSON (..)
                  , (.=)
                  )
import qualified Data.Aeson as JSON
import Data.Default (def)
import Data.Monoid ( (<>) )
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as LBS

(.==) :: Text -> Value -> (Text, Value)
(.==) = (,)

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

hateoasWrap :: [(Text, Text)] -> Value -> Value
hateoasWrap links JSON.Null =
    hateoasWrap links (JSON.object [])
hateoasWrap links (JSON.Object o) =
    let JSON.Object p =
            JSON.object
                [ "links" .== JSON.object
                    [ name .= value | (name, value) <- links ]
                ]
    in JSON.Object $ o <> p
hateoasWrap links v =
    hateoasWrap links $ JSON.object [ "value" .== v ]

joinPath :: [Text] -> Text
joinPath items = "/" <> (mconcat . List.intersperse "/" $ items)

getResource :: (Maybe (IO b)) -> (b -> Response) -> IO Response
getResource actionMay buildResponse =
    case actionMay of
        Nothing -> return methodNotAllowedResponse
        Just action -> buildResponse <$> action

buildJSONItemResponse :: ToJSON a => [Text] -> Resource a -> Text -> Maybe a -> Response
buildJSONItemResponse _ _ _ Nothing =
    notFoundResponse
buildJSONItemResponse parentPath resource itemID (Just item) =
    responseJSON status200 [] $
        itemToJSON parentPath (collectionName resource) itemID item

itemToJSON :: ToJSON a => [Text] -> Text -> Text -> a -> JSON.Value
itemToJSON parentPath collectionName itemID item =
    hateoasWrap
            [ ("self", joinPath $ parentPath ++ [collectionName, itemID])
            , ("parent", joinPath $ parentPath ++ [collectionName])
            ]
        $ toJSON item

buildJSONCollectionResponse :: ToJSON a => [Text] -> Resource a -> [(Text, a)] -> Response
buildJSONCollectionResponse parentPath resource items =
    responseJSON status200 [("Content-type", "application/json")] $
        itemsToJSON toJSON parentPath (collectionName resource) items

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

getMulti :: [MimeType] -> [Text] -> Text -> Text -> MultiDocument -> IO (Maybe (MimeType, LBS.ByteString))
getMulti accepts parentPath collectionName itemID multi = do
    let jsonViewMay = do
            return . JSON.encode . itemToJSON parentPath collectionName itemID <$> mdJSON multi
    let jsonHandlers = case jsonViewMay of
            Nothing ->
                []
            Just jsonView ->
                [ ("text/json", jsonView)
                , ("application/json", jsonView)
                ]
    let handlers = jsonHandlers ++ mdViews multi
    case selectView accepts handlers of
        Nothing -> return Nothing
        Just (mimeType, load) -> do
            body <- load
            return $ Just (mimeType, body)

multiGET :: Resource (MultiDocument) -> [Text] -> Application
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
                        Just item -> do
                            viewMay <- getMulti
                                accepts
                                parentPath
                                (collectionName resource)
                                itemID
                                item
                            case viewMay of
                                Nothing ->
                                    respond notAcceptableResponse
                                Just (mimeType, body) ->
                                    respond $
                                        responseLBS
                                            status200
                                            [("Content-type", Mime.pack mimeType)]
                                            body
        _ -> respond notFoundResponse

jsonGET :: (FromJSON a, ToJSON a) => Resource a -> [Text] -> Application
jsonGET resource parentPath request respond =
    case pathInfo request of
        [] -> respond =<< getResource
                (listMay resource <*> pure def)
                (buildJSONCollectionResponse parentPath resource)
        [itemID] -> respond =<< getResource
                        (findMay resource <*> pure itemID)
                        (buildJSONItemResponse
                            parentPath resource itemID)
        _ -> respond notFoundResponse

jsonPOST :: (FromJSON a, ToJSON a) => Resource a -> [Text] -> Application
jsonPOST resource parentPath request respond =
    case pathInfo request of
        [] -> case createMay resource of
            Nothing -> respond methodNotAllowedResponse
            Just create -> do
                body <- lazyRequestBody request
                let parseResult = JSON.decode body
                case parseResult of
                    Nothing ->
                        respond malformedJSONResponse
                    Just item -> do
                        itemIDMay <- create item
                        let response =
                                case itemIDMay of
                                    Nothing ->
                                        conflictResponse
                                    Just itemID ->
                                        mapResponseStatus
                                            (const status201) $
                                        buildJSONItemResponse
                                            parentPath
                                            resource
                                            itemID
                                            (Just item)
                        respond response
        [itemID] -> respond methodNotAllowedResponse
        _ -> respond notFoundResponse

jsonPUT :: (FromJSON a, ToJSON a) => Resource a -> [Text] -> Application
jsonPUT resource parentPath request respond =
    case pathInfo request of
        [] -> respond methodNotAllowedResponse
        [itemID] -> case storeMay resource of
            Nothing -> respond methodNotAllowedResponse
            Just store -> do
                body <- lazyRequestBody request
                let parseResult = JSON.decode body
                case parseResult of
                    Nothing ->
                        respond malformedJSONResponse
                    Just item -> do
                        result <- store itemID item
                        let response =
                                buildJSONItemResponse
                                    parentPath
                                    resource
                                    itemID
                                    (Just item)
                        case result of
                            Created -> respond .
                                mapResponseStatus
                                    (const status201) $
                                response
                            Updated -> respond response
                            StoreRejected -> respond conflictResponse
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
jsonHandler resource =
    (collectionName resource, handler)
    where
        handler :: [Text] -> Application
        handler parentPath request respond = do
            case requestMethod request of
                "GET" ->
                    jsonGET resource parentPath request respond
                "POST" ->
                    jsonPOST resource parentPath request respond
                "PUT" ->
                    jsonPUT resource parentPath request respond
                "DELETE" ->
                    handleDELETE resource parentPath request respond
                _ -> respond methodNotAllowedResponse

multiHandler :: Resource MultiDocument -> (Text, [Text] -> Application)
multiHandler resource =
    (collectionName resource, handler)
    where
        handler :: [Text] -> Application
        handler parentPath request respond = do
            case requestMethod request of
                "GET" ->
                    multiGET resource parentPath request respond
                "DELETE" ->
                    handleDELETE resource parentPath request respond
                _ -> respond methodNotAllowedResponse

malformedJSONResponse :: Response
malformedJSONResponse =
    responseJSON
        status400
        []
        (JSON.object
            [ "error" .= (400 :: Int)
            , "message" .== "Malformed JSON"
            ]
        )

deletedResponse :: Response
deletedResponse =
    responseLBS status204 [] ""

conflictResponse :: Response
conflictResponse =
    responseJSON
        status409
        []
        (JSON.object
            [ "error" .= (409 :: Int)
            , "message" .== "Conflict"
            ]
        )

notFoundResponse :: Response
notFoundResponse =
    responseJSON
        status404
        []
        (JSON.object
            [ "error" .= (404 :: Int)
            , "message" .== "Not Found"
            ]
        )

methodNotAllowedResponse :: Response
methodNotAllowedResponse =
    responseJSON
        status405
        []
        (JSON.object
            [ "error" .= (405 :: Int)
            , "message" .== "Method Not Allowed"
            ]
        )

notAcceptableResponse :: Response
notAcceptableResponse =
    responseJSON
        status406
        []
        (JSON.object
            [ "error" .= (406 :: Int)
            , "message" .== "Not Acceptable"
            ]
        )

responseJSON :: ToJSON a => Status -> [Header] -> a -> Response
responseJSON status headers val =
    responseLBS
        status
        (("Content-Type", "application/json"):headers)
        (JSON.encode val)
