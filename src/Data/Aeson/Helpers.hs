{-#LANGUAGE OverloadedStrings #-}
module Data.Aeson.Helpers
where

import Data.Aeson
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Monoid ( (<>) )

(.==) :: Text -> Value -> (Text, Value)
(.==) = (,)

fromJSONMay :: FromJSON a => Value -> Maybe a
fromJSONMay val =
    case fromJSON val of
        Error _ -> Nothing
        Success x -> Just x

objectifyWith :: Text -> Value -> HashMap Text Value
objectifyWith key Null = HashMap.empty
objectifyWith key (Object o) = o
objectifyWith key value = HashMap.singleton key value

objectify :: Value -> HashMap Text Value
objectify = objectifyWith "_value"

assoc :: Text -> Value -> Value -> Value
assoc key item collection =
    Object $ objectify collection <> HashMap.singleton key item

alist :: ToJSON a => [(Text, a)] -> Value
alist elems = object [ name .= value | (name, value) <- elems ]
