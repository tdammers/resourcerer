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

data Resource a =
    Resource
        { collectionName :: Text
        , findMay :: Maybe (Identifier -> IO (Maybe a))
        , listMay :: Maybe (ListSpec -> IO [(Identifier, a)])
        , subResourcesMay :: Maybe (Identifier -> IO (Resource a))
        }
