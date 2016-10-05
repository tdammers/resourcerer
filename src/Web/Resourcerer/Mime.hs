{-#LANGUAGE OverloadedStrings #-}
module Web.Resourcerer.Mime
( 
-- * Types
  MimeType (..)
-- * Parsing MIME types
, parse
, parseV
-- * Encoding MIME types
, pack
-- * Inspecting MIME types
, isValid
, getParam
, Specificity (..)
, specificity
-- * Dealing with HTTP Accept
, getQ
, sortAccepts
, isMatch
, parseAccept
)
where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.List as List
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Data.Char (ord, chr, isAlphaNum, isSpace)
import Data.String

-- | A MIME type, consisting of a type, subtype, and parameters.
-- Type and subtype are represented as raw 'ByteString's, no attempts are made
-- at parsing them any further. Specifically, namespaced subtypes and @+xml@
-- are not parsed, but rather just included in the subtype as-is.
data MimeType =
    MimeType
        { mimeType :: ByteString
        , mimeSubtype :: ByteString
        , mimeParams :: [(ByteString, ByteString)]
        }
        deriving (Eq, Show)

instance IsString MimeType where
    fromString = parse . fromString

-- | Indication to how specific a MIME type is.
data Specificity = AnyType -- ^ The \"any\" type, @*/*@
                 | AnySubtype -- ^ Wildcarded subtype, e.g. @text/*@
                 | Specific -- ^ A fully specified type, e.g. @text/plain@
                 deriving (Show, Read, Eq, Ord, Enum)

-- | Get a MIME type's specificity. Note that no guarantees are promised for
-- invalid MIME types; specifically, @*/something@ is reported as 'AnyType', as
-- if it were @*/*@.
specificity :: MimeType -> Specificity
specificity m =
    case (isWildcard (mimeType m), isWildcard (mimeSubtype m)) of
        (False, False) -> Specific
        (False, True) -> AnySubtype
        (True, _) -> AnyType

-- | Sort a list of MIME types by their @q@ parameter, highest first,
-- defaulting to 1.0; secondary sort criterion is the type's specificity, most
-- specific first. This is the behavior specified in RFC 2616.
sortAccepts :: [MimeType] -> [MimeType]
sortAccepts = List.sortOn (\m -> (negate . getQ $ m, specificity m))

-- | Get the @q@ parameter from a MIME type.
getQ :: MimeType -> Double
getQ = fromMaybe 1 . (>>= parseDouble) . getParam "q"

-- | Get a parameter from a MIME type.
getParam :: ByteString -> MimeType -> Maybe ByteString
getParam key = List.lookup key . mimeParams

breakBS :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakBS p s =
    let (l, rem) = BS8.break p s
    in (l, BS.drop 1 rem)

-- | Parse a 'ByteString' into a 'MimeType'. No validation is performed, which
-- means that the resulting MIME type may be invalid. Use 'parseV' to ensure
-- well-formedness.
parse :: ByteString -> MimeType
parse bs =
    let (ty, paramsStr) = breakBS (== ';') bs
        (tyMaj, tyMin) = breakBS (== '/') ty
        paramsStrs = BS8.split ',' paramsStr
        params = map (breakBS (== '=')) paramsStrs
    in MimeType tyMaj tyMin params

-- | Checks whether the given MIME type is well-formed. A type is considered
-- well-formed if it meets the following criteria:
-- - both type and subtype contain only valid characters (alphanumeric, -, ., _, :, +)
isValid :: MimeType -> Bool
isValid (MimeType maj min params) =
    (isValidType maj && isValidType min) ||
    (isValidType maj && isWildcard min) ||
    (isWildcard maj && isWildcard min)

parseV :: ByteString -> Maybe MimeType
parseV bs =
    let m = parse bs
    in case isValid m of
        True -> Just m
        False -> Nothing

isWildcard :: ByteString -> Bool
isWildcard "*" = True
isWildcard _ = False

isValidType :: ByteString -> Bool
isValidType "" = False
isValidType bs = List.all isValidTypeChar . BS8.unpack $ bs

isValidTypeChar :: Char -> Bool
isValidTypeChar c = isAlphaNum c || (c `elem` ("-._:+" :: String))

-- | Packs a MIME type into a bytestring representation.
pack :: MimeType -> ByteString
pack m =
    mimeType m <> "/" <> mimeSubtype m <> tail
    where
        tail = if null (mimeParams m)
            then
                ""
            else
                (";" <>) . mconcat . List.intersperse ";" $
                    [ n <> "=" <> v | (n,v) <- mimeParams m ]

-- | Checks whether two mime types align. Only the type and subtype are
-- considered, and either argument specifying a media range that contains the
-- other is considered a match. The behavior for invalid mime types is
-- undefined. Aliases are not resolved, that is, @text/json@ and
-- @application/json@ are not considered a match.
isMatch :: MimeType -> MimeType -> Bool
isMatch a b
    | isWildcard (mimeType a) && isWildcard (mimeSubtype a) = True
    | isWildcard (mimeType b) && isWildcard (mimeSubtype b) = True
    | mimeType a == mimeType b && isWildcard (mimeSubtype a) = True
    | mimeType a == mimeType b && isWildcard (mimeSubtype b) = True
    | mimeType a == mimeType b && mimeSubtype a == mimeSubtype b = True
    | otherwise = False

parseDouble :: ByteString -> Maybe Double
parseDouble bs = do
    let (intstr, fracstr) = breakBS (== '.') bs
    intpart <- parseIntpart 0 (BS.unpack intstr)
    fracpart <- parseFracpart 10 (BS.unpack fracstr)
    return $ intpart + fracpart

parseIntpart :: Double -> [Word8] -> Maybe Double
parseIntpart a [] = Just a
parseIntpart a (x:xs) = do
    d <- parseDigit x
    let a' = d + a * 10
    parseIntpart a' xs

parseFracpart :: Double -> [Word8] -> Maybe Double
parseFracpart f [] = Just 0
parseFracpart f (x:xs) = do
    d <- (/ f) <$> parseDigit x
    a <- parseFracpart (f * 10) xs
    return $ a + d

parseDigit :: Word8 -> Maybe Double
parseDigit x
    | fromIntegral x >= ord '0' && fromIntegral x <= ord '9' = Just $ fromIntegral x - fromIntegral (ord '0')
    | otherwise = Nothing

-- | Parses the value in an HTTP Accept header into a list of MIME types,
-- sorted according to RFC 2616.
parseAccept :: ByteString -> [MimeType]
parseAccept =
    sortAccepts . map (parse . BS8.takeWhile (not . isSpace) . BS8.dropWhile isSpace) . BS8.split ','
