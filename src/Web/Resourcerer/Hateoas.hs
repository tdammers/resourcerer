{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE DeriveGeneric #-}
module Web.Resourcerer.Hateoas
where

import Praglude

data KeyNames =
    KeyNames
        { hrefName :: Text
        , parentName :: Text
        , idName :: Text
        }
        deriving (Show, Eq, Generic)

instance Default KeyNames where
    def =
        KeyNames
            { hrefName = "_href"
            , parentName = "_parent"
            , idName = "_id"
            }
joinUrlPath :: [Text] -> Text
joinUrlPath = concat . map ("/" <>)

maybeIf :: Bool -> a -> Maybe a
maybeIf True = Just
maybeIf False = const Nothing

hateoas :: KeyNames
        -> Text
        -> [Text]
        -> [(Text, Value)]
hateoas kn selfID parentPath = catMaybes
    [ Just $
        hrefName kn ~> joinUrlPath (parentPath <> [selfID])
    , maybeIf (parentPath /= []) $
        parentName kn ~> joinUrlPath parentPath
    , Just $
        idName kn ~> selfID
    ]
