{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Serve
where

import Web.Resourcerer.Resource (Resource (..), ListSpec (..))
import qualified Data.Text
import Data.Text (Text)
import Network.Wai ( responseLBS
                   , requestMethod
                   , pathInfo
                   , Application
                   , Response (..)
                   )
import Network.HTTP.Types ( Status
                          , status200
                          , status404
                          , status405
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

routeResources :: [(Text, Text -> Application)] -> Text -> Application
routeResources resources parentPath request respond = do
    case pathInfo request of
        [] -> respond $ responseJSON
                status200
                []
                [ JSON.object
                    [ "collections" .= (map fst resources) ]
                ]
        (ident:rest) -> do
            case lookup ident resources of
                Nothing -> respond notFoundResponse
                Just handler -> do
                    let request' = request { pathInfo = rest }
                    handler parentPath request' respond

hateoasWrap :: Text -> Value -> Value
hateoasWrap selfURI (JSON.Object o) =
    let JSON.Object p = JSON.object [ "_href" .= selfURI ]
    in JSON.Object $ o <> p
hateoasWrap selfURI v =
    hateoasWrap selfURI $ JSON.object [ "value" .== v ]

joinPath :: [Text] -> Text
joinPath = mconcat . List.intersperse "/"

-- apPure :: m (a -> b) -> m a -> m b
-- apPure f = (f <*>) . pure

fmapPure :: Applicative m => (a -> b) -> a -> m b
fmapPure f = pure . f

getResource :: (Maybe (IO b)) -> (b -> Response) -> IO Response
getResource actionMay buildResponse =
    case actionMay of
        Nothing -> return methodNotAllowedResponse
        Just action -> buildResponse <$> action

buildItemResponse :: ToJSON a => Text -> Resource a -> Text -> Maybe a -> Response
buildItemResponse _ _ _ Nothing =
    notFoundResponse
buildItemResponse parentPath resource itemID (Just item) =
    responseJSON status200 []
        . hateoasWrap (joinPath [parentPath, collectionName resource, itemID])
        $ toJSON item

buildCollectionResponse :: ToJSON a => Text -> Resource a -> [(Text, a)] -> Response
buildCollectionResponse parentPath resource items =
    responseJSON status200 []
    .  hateoasWrap (joinPath [parentPath, collectionName resource])
    $ JSON.object
        [ collectionName resource .=
            [ hateoasWrap
                (joinPath [parentPath, collectionName resource, i])
                (toJSON v)
            | (i, v) <- items
            ]
        , "count" .= length items
        ]

jsonResource :: ToJSON a => Resource a -> (Text, Text -> Application)
jsonResource resource =
    (collectionName resource, handler)
    where
        handler :: Text -> Application
        handler parentPath request respond = do
            case requestMethod request of
                "GET" -> case pathInfo request of
                    [] -> respond =<< getResource
                            (listMay resource <*> pure def)
                            (buildCollectionResponse parentPath resource)
                    [itemID] -> respond =<< getResource
                                    (findMay resource <*> pure itemID)
                                    (buildItemResponse
                                        parentPath resource itemID)
                    _ -> respond notFoundResponse
                _ -> respond methodNotAllowedResponse

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
