{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveFunctor #-}
module Web.Resourcerer.Resource
where

import qualified Data.Text
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (Default (..))
import Web.Resourcerer.Mime (MimeType)
import qualified Web.Resourcerer.Mime as Mime
import qualified Data.Aeson as JSON

data ListSpec =
    ListSpec
        { listOrdering :: [Text]
        , listOffset :: Integer
        , listPageSize :: Maybe Integer
        }
        deriving (Show, Eq)

instance Default ListSpec where
    def = ListSpec [] 0 Nothing

type Identifier = Text

type StructuredBody = JSON.Value

type DigestBody = JSON.Value

data TypedBody =
    TypedBody
        { bodyType :: MimeType
        , bodyData :: LBS.ByteString
        }

data Body =
    BodyTyped TypedBody |
    BodyStructured StructuredBody

data StoreResult =
    StoreFailedUnsupportedMediaType |
    StoreFailedMalformed |
    StoreFailedExists |
    StoreFailedMethodNotAllowed |
    Created Identifier |
    Updated Identifier
    deriving (Show)

data DeleteResult =
    DeleteFailedDoesNotExist |
    DeleteFailedMethodNotAllowed |
    Deleted
    deriving (Show)

data Resource =
    Resource
        { getStructuredBody :: IO StructuredBody
        , getTypedBody :: MimeType -> IO (Maybe TypedBody)
        , getDigestBody :: IO DigestBody
        , listChildren :: ListSpec -> IO [(Identifier, Resource)]
        , getChild :: Identifier -> IO (Maybe Resource)
        , storeStructuredDocument :: Identifier -> StructuredBody -> IO StoreResult
        , storeTypedDocument :: Identifier -> TypedBody -> IO StoreResult
        , createStructuredDocument :: StructuredBody -> IO StoreResult
        , createTypedDocument :: TypedBody -> IO StoreResult
        , deleteDocument :: Identifier -> IO DeleteResult
        }

defResource :: Resource
defResource =
    Resource
        { getStructuredBody = return JSON.Null
        , getTypedBody = const $ return Nothing
        , getDigestBody = return JSON.Null
        , listChildren = const $ return []
        , getChild = const $ return Nothing
        , storeStructuredDocument = const . const . return $ StoreFailedMethodNotAllowed
        , storeTypedDocument = const . const . return $ StoreFailedMethodNotAllowed
        , createStructuredDocument = const . return $ StoreFailedMethodNotAllowed
        , createTypedDocument = const . return $ StoreFailedMethodNotAllowed
        , deleteDocument = const . return $ DeleteFailedMethodNotAllowed
        }

mount :: Text -> Resource -> Resource -> Resource
mount name child parent =
    parent
        { getChild = \childName ->
            if name == childName
                then return (Just child)
                else  getChild parent childName
        , listChildren = \spec -> do
            origChildren <- listChildren parent spec
            return . maybe id (take . fromIntegral) (listPageSize spec) $
                origChildren ++ [(name, child)]
        }
