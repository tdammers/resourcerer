module Web.Resourcerer.MultiDocument
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Web.Resourcerer.Mime (MimeType)
import qualified Web.Resourcerer.Mime as Mime
import qualified Data.Aeson as JSON

data MultiDocument =
    MultiDocument
        { mdJSON :: Maybe JSON.Value
        , mdViews :: [(MimeType, IO LBS.ByteString)]
        }

selectView :: [MimeType] -> [(MimeType, a)] -> Maybe (MimeType, a)
selectView accepts options =
    let matches = do
            accept <- accepts
            option <- options
            if Mime.isMatch accept (fst option)
                then [option]
                else []
    in case matches of
        [] -> Nothing
        (match:_) -> Just match
