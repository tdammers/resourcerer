{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE DeriveGeneric #-}
module Web.Resourcerer.Resource
where

import Praglude
import Web.Resourcerer.Mime (MimeType)
import qualified Web.Resourcerer.Mime as Mime

-- | Sort order: ascending or descending.
data AscDesc = Ascending | Descending
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Default AscDesc where
    def = Ascending

-- | Properties that further define a listing: pagination, ordering, and
-- possibly other properties to be added later.
data ListSpec =
    ListSpec
        { listOrder :: [(Text, AscDesc)]
        , listOffset :: Integer
        , listCount :: Integer
        }

instance Default ListSpec where
    def = ListSpec [] 0 1000

-- | A resource, the central modeling entity.
data Resource =
    Resource
        { 
          -- | Get a "structured" body: pure data, library takes care of
          -- content negotiation and formatting
          getStructuredBody :: Maybe (IO Value)

          -- | Get an association list of supported binary response types.
          -- For every listed 'MimeType', provide an 'IO' action that produces
          -- the raw response body, which will be sent unchanged.
        , getTypedBodies :: [(MimeType, IO LByteString)]

          -- | Get an association list of child resources, reported as
          -- @(identifier, resource)@ pairs, according to the given 'ListSpec'
        , getChildren :: Maybe (ListSpec -> IO [(Text, Resource)])

          -- | Get one child by ID
        , getChild :: Maybe (Text -> IO (Maybe Resource))

          -- | Create a child resource from a 'Value'; the resource itself
          -- is responsible for assigning a suitable unique ID
        , createChild :: Maybe (Value -> IO CreateResult)

          -- | Create or update a child resource at the specified ID; if it
          -- already exist, overwrite, otherwise create a new one.
        , storeChild :: Maybe (Text -> Value -> IO StoreResult)

          -- | Delete the child resource under the specified ID.
        , deleteChild :: Maybe (Text -> IO DeleteResult)
        }

-- | Result reported from 'createChild'
data CreateResult = CreateFailedAlreadyExists -- ^ create rejected, entity
                                              -- already exists
                  | CreateFailedMalformed -- ^ create rejected, payload does
                                          -- not match expected schema
                  | Created Text Resource

-- | Result reported from 'storeChild'
data StoreResult = StoreFailedInvalidKey -- ^ store rejected, key is not valid
                 | StoreFailedMalformed -- ^ store rejected, payload does not
                                        -- match expected schema
                 | Stored Text Resource -- ^ successfully stored

-- | Result reported from 'deleteChild'
data DeleteResult = DeleteFailedDoesNotExist -- ^ nothing to delete
                  | Deleted Text -- ^ successfully deleted

instance Default Resource where
    def = Resource Nothing [] Nothing Nothing Nothing Nothing Nothing
