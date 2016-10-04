{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Mime
where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.List as List
import Data.Word (Word8)

data MimeType =
    MimeType
        { mimeType :: ByteString
        , mimeSubtype :: ByteString
        , mimeParams :: [(ByteString, ByteString)]
        }
        deriving (Eq, Show)

breakBS :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakBS p s =
    let (l, rem) = BS.break p s
    in (l, BS.drop 1 rem)

parse :: ByteString -> MimeType
parse bs =
    let (ty, paramsStr) = breakBS (== ';') bs
        (tyMaj, tyMin) = breakBS (== '/') ty
        paramsStrs = BS.split ',' paramsStr
        params = map (breakBS (== '=')) paramsStrs
    in MimeType tyMaj tyMin params

pack :: MimeType -> ByteString
pack m =
    mimeType m <> "/" <> mimeSubtype m <> tail
    where
        tail = if null (mimeParams m)
            then
                ""
            else
                (";" <>) . mconcat . List.intersperse "," $
                    [ n <> "=" <> v | (n,v) <- mimeParams m ]

isMatch :: MimeType -> MimeType -> Bool
isMatch a b
    | mimeType a == "*" && mimeSubtype a == "*" = True
    | mimeType b == "*" && mimeSubtype b == "*" = True
    | mimeType a == mimeType b && mimeSubtype a == "*" = True
    | mimeType a == mimeType b && mimeSubtype b == "*" = True
    | mimeType a == mimeType b && mimeSubtype a == mimeSubtype b = True
    | otherwise = False
