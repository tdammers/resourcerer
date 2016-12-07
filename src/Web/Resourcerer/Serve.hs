{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
module Web.Resourcerer.Serve
where

import Praglude
import Web.Resourcerer.Resource (Resource (..))
import Web.Resourcerer.Api ( runResource
                           , runApi
                           , methodFromBS
                           , ApiContext (..)
                           , ApiResponse (..)
                           , ApiError (..)
                           , PostedBody (..)
                           , Api)
import qualified Web.Resourcerer.Api as Api
import qualified Web.Resourcerer.Mime as Mime
import Network.Wai ( Application
                   , Request (..)
                   , Response
                   , responseLBS
                   , ResponseReceived
                   , lazyRequestBody
                   )
import Network.HTTP.Types ( status200
                          , status404
                          , status405
                          , status406
                          , status409
                          , Status
                          )
import qualified Data.Aeson as JSON

resourceToApplication :: Resource -> Application
resourceToApplication resource rq respond =
    resourceToApplication' resource rq respond
        `catch` \err -> catchApiError err rq respond

resourceToApplication' :: Resource -> Application
resourceToApplication' resource rq respond = do
    let apiContext = waiRequestToApiContext rq
        api = runResource resource
        accepts = apiContext ^. Api.accept
    apiResponse <- runApi api apiContext
    respond . apiResponseToWaiResponse accepts $ apiResponse

catchApiError :: ApiError -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
catchApiError err rq respond = do
    let (status, value) = apiErrorToResponse err
        accepts = requestAccepts rq
    respond $ structuredResponse accepts status value

requestAccepts :: Request -> [Mime.MimeType]
requestAccepts = Mime.parseAccept
               . fromMaybe "*/*"
               . lookup "Accept"
               . AList
               . requestHeaders

waiRequestToApiContext :: Request -> ApiContext
waiRequestToApiContext rq =
    let accepts = requestAccepts rq
        contentType = Mime.parse . fromMaybe "text/json" . lookup "Content-type" . AList . requestHeaders $ rq
    in ApiContext
        { _consumedPath = []
        , _remainingPath = pathInfo rq
        , _method = methodFromBS $ requestMethod rq
        , _accept = accepts
        , _postedBody = do
            rawBody <- lazyRequestBody rq
            let parsedBody = parseRequestBody contentType rawBody
            return $ PostedBody rawBody contentType parsedBody
        }

parseRequestBody :: Mime.MimeType -> LByteString -> Maybe Value
parseRequestBody ty src =
    let matchingReaders = filter (\(k,v) -> Mime.isMatch k ty) structuredReaders
    in case matchingReaders of
        [] -> Nothing
        ((k, read):_) -> read src

type StructuredReader = LByteString -> Maybe Value

structuredReaders :: [(Mime.MimeType, StructuredReader)]
structuredReaders =
    [ ("text/json", decodeJSON)
    , ("application/json", decodeJSON)
    , ("text/plain", Just . JSON.String . s)
    ]

type StructuredWriter = Value -> LByteString

structuredWriters :: [(Mime.MimeType, StructuredWriter)]
structuredWriters =
    [ ("text/json;charset=utf8", encodeJSON)
    , ("application/json", encodeJSON)
    , ("text/plain;charset=utf8", s . jsonToText)
    ]

jsonToText :: Value -> Text
jsonToText (JSON.String txt) = txt
jsonToText JSON.Null = ""
jsonToText (JSON.Bool True) = "true"
jsonToText (JSON.Bool False) = "false"
jsonToText (JSON.Number n) = s . show $ n
jsonToText (JSON.Array xs) = concat . intersperse ", " . map jsonToText . toList $ xs
jsonToText (JSON.Object xs) = concat . intersperse ", " $
    [ k <> ": " <> jsonToText v
    | (k, v)
    <- pairs xs
    ]

structuredResponse :: [Mime.MimeType] -> Status -> Value -> Response
structuredResponse accepts status body =
    let candidateWriters = do
            accept <- accepts
            (available, writer) <- structuredWriters
            if Mime.isMatch accept available
                then [(available, writer)]
                else []
    in case candidateWriters of
        [] ->
            responseLBS
                status406
                [ ("Content-type", "text/plain") ]
                "Not Acceptable"
        ((ty, write):_) ->
            responseLBS
                status
                [ ("Content-type", Mime.pack ty) ]
                (write body)

apiResponseToWaiResponse :: [Mime.MimeType] -> ApiResponse -> Response
apiResponseToWaiResponse accepts (StructuredBody body) =
    structuredResponse accepts status200 body
apiResponseToWaiResponse accepts (BinaryBody ty body) =
    if any (Mime.isMatch ty) accepts
        then responseLBS status200 [("Content-type", Mime.pack ty)] body
        else structuredResponse accepts status406 "Not Acceptable"
apiResponseToWaiResponse accepts (Stored name) =
    structuredResponse accepts status200 (object ["id" ~> name])

apiErrorToResponse :: ApiError -> (Status, Value)
apiErrorToResponse DoesNotExistError = (status404, "Not Found")
apiErrorToResponse AlreadyExistsError = (status409, "Already Exists")
apiErrorToResponse UnsupportedOperationError = (status405, "Method Not Allowed")
apiErrorToResponse UnsupportedMediaTypeError = (status406, "Not Acceptable")
