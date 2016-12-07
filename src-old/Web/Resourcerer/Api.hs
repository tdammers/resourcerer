{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Web.Resourcerer.Api
where

import Praglude
import Web.Resourcerer.Resource
import qualified Data.Aeson as JSON

data Method = GET
            | POST
            | PUT
            | DELETE
            | PATCH
    deriving (Show, Read, Eq, Enum)

instance ToJSON Method where
    toJSON = \case
        GET -> "GET"
        POST -> "POST"
        PUT -> "PUT"
        DELETE -> "DELETE"
        PATCH -> "PATCH"

instance FromJSON Method where
    parseJSON = \case
        JSON.String "POST" -> return GET
        JSON.String "POST" -> return POST
        JSON.String "PUT" -> return PUT
        JSON.String "DELETE" -> return DELETE
        JSON.String "PATCH" -> return PATCH
        _ -> fail "Invalid JSON for Method"

data Payload = StructuredPayload Value
             | DumbPayload MimeType LByteString

data Request =
    Request
        { _remainingPath :: [Text]
        , _parentPath :: [Text]
        , _method :: Method
        , _requestPayload :: Maybe Payload
        }

data Response = GetResponse Payload
              | CreatedResponse Identifier
              | StoredResponse
              | DeletedResponse

type Api = Request -> IO Response

api :: Resource -> Api
api resource rq =
    case rq ^. method of
        GET -> getResource resource rq
        POST -> postResource resource rq
        PUT -> putResource resource rq
        DELETE -> deleteResource resource rq
        _ -> throw MethodNotAllowed

getResource :: Resource -> Api
getResource resource rq =
    case rq ^. remainingPath of
        [] -> getResourceSelf resource rq
        _ -> do
            (resource', rq') <- getChildResource resource rq
            getResource resource' rq'

getChildResource :: Resource -> Api
getChildResource resource rq = do
    case rq ^. remainingPath of
        [] -> raise NotFound
        (x:xs) -> do

            
