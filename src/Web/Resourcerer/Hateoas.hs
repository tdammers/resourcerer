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

hateoas :: KeyNames
        -> Text
        -> [Text]
        -> [(Text, Value)]
hateoas kn selfID parentPath =
    [ hrefName kn ~> joinUrlPath (parentPath <> [selfID])
    , parentName kn ~> joinUrlPath parentPath
    , idName kn ~> selfID
    ]
