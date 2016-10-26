{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
module Web.Resourcerer.Serve
( resourceToApplication
)
where

import Web.Resourcerer.Resource
        ( Resource (..)
        , ListSpec (..)
        , StoreResult (..)
        , DeleteResult (..)
        , StoreResult (..)
        , StructuredBody
        , TypedBody (..)
        , defResource
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
import Web.Resourcerer.Hateoas (hateoasWrap)
import Web.Resourcerer.Mime (MimeType (..))
import qualified Web.Resourcerer.Mime as Mime
import qualified Data.Text as Text
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
import Data.List (find)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (throw, catch)
import Control.Applicative ( (<|>) )

resourceToApplication :: Resource -> Application
resourceToApplication resource request respond = do
    let accepts = Mime.parseAccept . fromMaybe "*/*" . lookup "Accept" . requestHeaders $ request
    serveResource resource accepts [] request respond

serveResource :: Resource -> [MimeType] -> [Text] -> Application
serveResource resource accepts parentPath request respond = do
    go `catch` handle
    where
        handle :: HttpException -> IO ResponseReceived
        handle e = respond $ exceptionResponse e

        go :: IO ResponseReceived
        go = do
            case pathInfo request of
                [] -> serveNode resource accepts parentPath request respond
                (ident:rest) -> serveChildNode resource accepts ident parentPath request respond

serveNode :: Resource -> [MimeType] -> [Text] -> Application
serveNode resource [] parentPath request respond =
    throw NotAcceptable
serveNode resource (accept:accepts) parentPath request respond = do
    bodyMay <- getBody resource accept parentPath
    case bodyMay of
        Nothing -> serveNode resource accepts parentPath request respond
        Just body -> respond . typedResponse $ body

serveChildNode :: Resource -> [MimeType] -> Text -> [Text] -> Application
serveChildNode resource accepts name parentPath request respond = do
    childMay <- getChild resource name
    let newRequest = request { pathInfo = tail (pathInfo request) }
    case childMay of
        Nothing -> throw NotFound
        Just child -> serveResource child accepts (parentPath ++ [name]) newRequest respond

getBody :: Resource -> MimeType -> [Text] -> IO (Maybe TypedBody)
getBody resource accept parentPath = go bodyGetters
    where
        go :: [Resource -> MimeType -> [Text] -> IO (Maybe TypedBody)]
           -> IO (Maybe TypedBody)
        go [] = return Nothing
        go (getter:getters) = do
            resultMay <- getter resource accept parentPath
            case resultMay of
                Just result -> return $ Just result
                Nothing -> go getters

bodyGetters :: [Resource -> MimeType -> [Text] -> IO (Maybe TypedBody)]
bodyGetters =
    [ typedBodyGetter, structuredBodyGetter ]

typedBodyGetter :: Resource -> MimeType -> [Text] -> IO (Maybe TypedBody)
typedBodyGetter resource accept _ = getTypedBody resource accept

structuredBodyGetter :: Resource -> MimeType -> [Text] -> IO (Maybe TypedBody)
structuredBodyGetter resource accept parentPath = do
    plainBody <- getStructuredBody resource
    childResources <- listChildren resource def
    let prepareChild :: (Text, Resource) -> IO (Text, JSON.Value)
        prepareChild (name, childResource) = do
            let links =
                    [ ("_self", joinPath (parentPath ++ [name]))
                    , ("_parent", joinPath parentPath)
                    ]
            childBody <- getDigestBody childResource
            return (name, hateoasWrap links childBody)
    children <- mapM prepareChild childResources
    let links = catMaybes
            [ Just ("_self", joinPath parentPath)
            , if null parentPath
                then Nothing
                else Just ("_parent", joinPath . init $ parentPath)
            ]
        body =
            JSON.assoc "_children" (JSON.object children) .
            hateoasWrap links $
            plainBody
    return $ formatStructuredBody accept body


formatStructuredBody :: MimeType -> JSON.Value -> Maybe TypedBody
formatStructuredBody = formatStructuredBodyWith formatters

formatStructuredBodyWith :: [MimeType -> JSON.Value -> Maybe TypedBody]
                         -> MimeType
                         -> JSON.Value
                         -> Maybe TypedBody
formatStructuredBodyWith [] _ _ = Nothing
formatStructuredBodyWith (formatter:formatters) accept value =
    formatter accept value <|> formatStructuredBodyWith formatters accept value

formatters :: [MimeType -> JSON.Value -> Maybe TypedBody]
formatters = [ jsonFormatter ]

jsonFormatter :: MimeType -> JSON.Value -> Maybe TypedBody
jsonFormatter accept value = do
    mime <- resolveMime accept ["text/json;charset=utf8", "application/json"]
    return TypedBody
        { bodyType = mime
        , bodyData = JSON.encode value
        }

resolveMime :: MimeType -> [MimeType] -> Maybe MimeType
resolveMime accept [] = Nothing
resolveMime accept (mime:mimes)
    | Mime.isMatch accept mime = Just mime
    | otherwise = resolveMime accept mimes

typedResponse :: TypedBody -> Response
typedResponse body =
    responseLBS
        status200
        (("Content-Type", Mime.pack (bodyType body)):[])
        (bodyData body)

joinPath :: [Text] -> Text
joinPath = ("/" <>) . Text.intercalate "/"
