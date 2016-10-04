{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Serve
( routeResources
, jsonHandler
)
where

import Web.Resourcerer.Resource
        ( Resource (..)
        , ListSpec (..)
        , StoreResult (..)
        , DeleteResult (..)
        )
import qualified Data.Text
import Data.Text (Text)
import Network.Wai ( responseLBS
                   , requestMethod
                   , mapResponseStatus
                   , pathInfo
                   , lazyRequestBody
                   , Application
                   , Response (..)
                   )
import Network.HTTP.Types ( Status
                          , status200
                          , status201
                          , status204
                          , status400
                          , status404
                          , status405
                          , status406
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
    responseJSON status200 []
        . hateoasWrap
            [ ("self", joinPath $ parentPath ++ [collectionName resource, itemID])
            , ("parent", joinPath $ parentPath ++ [collectionName resource])
            ]
        $ toJSON item

buildJSONCollectionResponse :: ToJSON a => [Text] -> Resource a -> [(Text, a)] -> Response
buildJSONCollectionResponse parentPath resource items =
    responseJSON status200 []
    . hateoasWrap
        [ ("self", joinPath $ parentPath ++ [collectionName resource])
        , ("parent", joinPath parentPath)
        ]
    $ JSON.object
        [ collectionName resource .=
            [ hateoasWrap
                [ ("self", joinPath $ parentPath ++ [collectionName resource, i])
                , ("parent", joinPath $ parentPath ++ [collectionName resource])
                ]
                (toJSON v)
            | (i, v) <- items
            ]
        , "count" .= length items
        ]

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

jsonDELETE :: (FromJSON a, ToJSON a) => Resource a -> [Text] -> Application
jsonDELETE resource parentPath request respond =
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
                    jsonDELETE resource parentPath request respond
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
        status406
        []
        (JSON.object
            [ "error" .= (406 :: Int)
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

responseJSON :: ToJSON a => Status -> [Header] -> a -> Response
responseJSON status headers val =
    responseLBS
        status
        (("Content-Type", "application/json"):headers)
        (JSON.encode val)
