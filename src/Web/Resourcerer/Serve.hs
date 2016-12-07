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
                           , Api)
import qualified Web.Resourcerer.Api as Api
import qualified Web.Resourcerer.Mime as Mime
import Network.Wai ( Application
                   , Request (..)
                   , Response
                   , responseLBS
                   , ResponseReceived
                   )
import Network.HTTP.Types ( status200
                          )

resourceToApi :: Resource -> Api ()
resourceToApi = undefined

resourceToApplication :: Resource -> Application
resourceToApplication resource rq respond = do
    let apiContext = waiRequestToApiContext rq
        api = runResource resource
    apiResponse <- runApi api apiContext
    respond . apiResponseToWaiResponse $ apiResponse

waiRequestToApiContext :: Request -> ApiContext
waiRequestToApiContext rq =
    let accepts = Mime.parseAccept
                . fromMaybe "*/*"
                . lookup "Accept"
                . AList
                . requestHeaders
                $ rq
    in ApiContext
        { _consumedPath = []
        , _remainingPath = pathInfo rq
        , _method = methodFromBS $ requestMethod rq
        , _accept = accepts
        , _postedBody = undefined -- TODO
        }

apiResponseToWaiResponse :: ApiResponse -> Response
apiResponseToWaiResponse (StructuredBody body) =
    responseLBS status200 [("Content-type", "text/json;charset=utf8")] (encodeJSON body)
apiResponseToWaiResponse (BinaryBody ty body) =
    responseLBS status200 [("Content-type", Mime.pack ty)] body
apiResponseToWaiResponse (Stored name) =
    responseLBS status200 [("Content-type", "text/plain;charset=utf8")] (s name)
