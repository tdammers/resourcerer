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

data Method = GET | POST | PUT | DELETE
    deriving (Show, Read)

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
        , _postedBody :: PostedBody
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

type Api = StateT ApiContext IO

runApi :: Api a -> ApiContext -> IO a
runApi = evalStateT

runResource :: Resource -> Api ApiResponse
runResource resource = do
    use method >>= \case
        GET -> getResource resource
        POST -> postResource resource
        PUT -> putResource resource
        DELETE -> deleteResource resource


getResource :: Resource -> Api ApiResponse
getResource = undefined

postResource :: Resource -> Api ApiResponse
postResource = undefined

putResource :: Resource -> Api ApiResponse
putResource = undefined

deleteResource :: Resource -> Api ApiResponse
deleteResource = undefined
