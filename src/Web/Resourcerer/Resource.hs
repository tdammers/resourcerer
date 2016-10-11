{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Resource
where

import qualified Data.Text
import Data.Text (Text)
import Data.Default (Default (..))

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

data StoreResult = Created Identifier
                 | Updated Identifier
                 | StoreRejectedWrongType
                 | StoreRejectedExists
                 | StoreRejectedDoesNotExist
                 | StoreRejectedMalformed
                 deriving (Show, Read, Eq)

storeRejected :: StoreResult -> Bool
storeRejected StoreRejectedWrongType = True
storeRejected StoreRejectedExists = True
storeRejected StoreRejectedDoesNotExist = True
storeRejected StoreRejectedMalformed = True
storeRejected _ = False

data DeleteResult = Deleted
                  | NothingToDelete
                  | DeleteRejected
                  deriving (Show, Read, Eq, Enum)

deleteRejected :: DeleteResult -> Bool
deleteRejected DeleteRejected = True
deleteRejected _ = False

class Rejectable a where
    rejected :: a -> Bool
    rejected = not . accepted
    accepted :: a -> Bool
    accepted = not . rejected

instance Rejectable StoreResult where
    rejected = storeRejected

instance Rejectable DeleteResult where
    rejected = deleteRejected

data Resource a =
    Resource
        { collectionName :: Text
        , findMay :: Maybe (Identifier -> IO (Maybe a))
        , listMay :: Maybe (ListSpec -> IO [(Identifier, a)])
        , subResourcesMay :: Maybe (Identifier -> IO (Resource a))
        , createMay :: Maybe (a -> IO StoreResult) -- ^ create new, auto-generate ID
        , storeMay :: Maybe (Identifier -> a -> IO StoreResult) -- ^ create or overwrite by ID
        , deleteMay :: Maybe (Identifier -> IO DeleteResult) -- ^ delete an item by ID
        }

instance Default (Resource a) where
    def =
        Resource
            { collectionName = ""
            , findMay = Nothing
            , listMay = Nothing
            , subResourcesMay = Nothing
            , createMay = Nothing
            , storeMay = Nothing
            , deleteMay = Nothing
            }

mapResource :: (a -> b) -> (b -> Maybe a) -> Resource a -> Resource b
mapResource encode decode r =
    Resource
        mappedCollectionName
        mappedFindMay
        mappedListMay
        mappedSubResourcesMay
        mappedCreateMay
        mappedStoreMay
        mappedDeleteMay
    where
        mappedCollectionName = collectionName r

        mappedFindMay = (fmap . fmap . fmap . fmap $ encode) $ findMay r
        mappedListMay = (fmap . fmap . fmap . fmap . fmap $ encode) $ listMay r
        mappedSubResourcesMay = (fmap . fmap . fmap . fmap $ mapResource encode decode) subResourcesMay r
        mappedCreateMay = case createMay r of
            Nothing -> Nothing
            Just create -> Just $ \x -> do
                case decode x of
                    Nothing -> return StoreRejectedMalformed
                    Just y -> create y
        mappedStoreMay = case storeMay r of
            Nothing -> Nothing
            Just store -> Just $ \i x -> do
                case decode x of
                    Nothing -> return StoreRejectedWrongType
                    Just y -> store i y
        mappedDeleteMay = deleteMay r
