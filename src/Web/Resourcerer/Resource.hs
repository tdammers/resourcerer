{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
module Web.Resourcerer.Resource
where

import Praglude

data Resource =
    Resource
        { getStructuredBody :: Maybe (IO Value)
        , getChild :: Maybe (Text -> IO (Maybe Resource))
        }
