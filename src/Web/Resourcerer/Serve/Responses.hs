{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Serve.Responses
where

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
                          , status415
                          , Header
                          )
import Data.Aeson ( Value
                  , ToJSON (..)
                  , FromJSON (..)
                  , (.=)
                  )
import qualified Data.Aeson as JSON
import Data.Aeson.Helpers ( (.==) )
import qualified Data.Aeson.Helpers as JSON
import Web.Resourcerer.Serve.Exceptions (HttpException (..))

exceptionResponse :: HttpException -> Response
exceptionResponse MalformedInput = malformedInputResponse
exceptionResponse Conflict = conflictResponse
exceptionResponse NotFound = notFoundResponse
exceptionResponse MethodNotAllowed = methodNotAllowedResponse
exceptionResponse NotAcceptable = notAcceptableResponse
exceptionResponse UnsupportedMediaType = unsupportedMediaTypeResponse

malformedInputResponse :: Response
malformedInputResponse =
    responseJSON
        status400
        []
        (JSON.object
            [ "error" .= (400 :: Int)
            , "message" .== "Malformed Input"
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

unsupportedMediaTypeResponse :: Response
unsupportedMediaTypeResponse =
    responseJSON
        status415
        []
        (JSON.object
            [ "error" .= (415 :: Int)
            , "message" .== "Unsupported Media Type"
            ]
        )

responseJSON :: ToJSON a => Status -> [Header] -> a -> Response
responseJSON status headers val =
    responseLBS
        status
        (("Content-Type", "application/json"):headers)
        (JSON.encode val)
