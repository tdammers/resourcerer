{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
module Web.Resourcerer.Serve
where

import Praglude
import Web.Resourcerer.Resource (Resource (..))
import Web.Resourcerer.Api (runResource, runApi, ApiContext (..), ApiResponse, Api)
import qualified Web.Resourcerer.Api as Api
import Network.Wai (Application, Request, Response, ResponseReceived)

resourceToApi :: Resource -> Api ()
resourceToApi = undefined

resourceToApplication :: Resource -> Application
resourceToApplication resource rq respond = do
    let apiContext = waiRequestToApiContext rq
        api = runResource resource
    apiResponse <- runApi api apiContext
    respond . apiResponseToWaiResponse $ apiResponse

waiRequestToApiContext :: Request -> ApiContext
waiRequestToApiContext = undefined

apiResponseToWaiResponse :: ApiResponse -> Response
apiResponseToWaiResponse = undefined
