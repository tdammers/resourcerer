{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Hateoas
where

import Data.Text (Text)
import Data.Aeson (Value (..), object, (.=) )
import Data.Aeson.Helpers ( (.==) )
import Data.Monoid ( (<>) )

hateoasWrap :: [(Text, Text)] -> Value -> Value
hateoasWrap links Null =
    hateoasWrap links (object [])
hateoasWrap links (Object o) =
    let Object p =
            object
                [ "links" .== object
                    [ name .= value | (name, value) <- links ]
                ]
    in Object $ o <> p
hateoasWrap links v =
    hateoasWrap links $ object [ "value" .== v ]

