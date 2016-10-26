{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Hateoas
where

import Data.Text (Text)
import Data.Aeson (Value (..), object, (.=) )
import Data.Aeson.Helpers ( (.==), assoc )
import Data.Monoid ( (<>) )

hateoasWrap :: [(Text, Text)] -> Value -> Value
hateoasWrap links =
    assoc "links" $ object [ name .= value | (name, value) <- links ]
