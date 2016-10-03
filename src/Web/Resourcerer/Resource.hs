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

data StoreResult = Created | Updated | StoreRejected

data DeleteResult = Deleted | NothingToDelete | DeleteRejected

data Resource a =
    Resource
        { collectionName :: Text
        , findMay :: Maybe (Identifier -> IO (Maybe a))
        , listMay :: Maybe (ListSpec -> IO [(Identifier, a)])
        , subResourcesMay :: Maybe (Identifier -> IO (Resource a))
        , createMay :: Maybe (a -> IO Identifier) -- ^ create new, auto-generate ID
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
