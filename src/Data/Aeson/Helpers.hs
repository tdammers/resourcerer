module Data.Aeson.Helpers
where

import Data.Aeson
import Data.Text (Text)

(.==) :: Text -> Value -> (Text, Value)
(.==) = (,)

fromJSONMay :: FromJSON a => Value -> Maybe a
fromJSONMay val =
    case fromJSON val of
        Error _ -> Nothing
        Success x -> Just x

