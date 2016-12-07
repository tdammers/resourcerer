{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Hateoas
where

import Praglude
import Data.Aeson.Helpers ( (.==), assoc )

hateoasWrap :: [(Text, Text)] -> Value -> Value
hateoasWrap links =
    assoc "links" $ object [ name ~> value | (name, value) <- links ]
