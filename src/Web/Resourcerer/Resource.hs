{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE DeriveGeneric #-}
module Web.Resourcerer.Resource
where

import Praglude

data AscDesc = Ascending | Descending
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Default AscDesc where
    def = Ascending

data ListSpec =
    ListSpec
        { listOrder :: [(Text, AscDesc)]
        , listOffset :: Integer
        , listCount :: Integer
        }

instance Default ListSpec where
    def = ListSpec [] 0 1000

data Resource =
    Resource
        { getStructuredBody :: Maybe (IO Value)
        , getChildren :: Maybe (ListSpec -> IO [(Text, Resource)]) 
        , getChild :: Maybe (Text -> IO (Maybe Resource))
        }

instance Default Resource where
    def = Resource Nothing Nothing Nothing
