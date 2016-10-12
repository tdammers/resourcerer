module Data.SimpleParsers
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

breakBS :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakBS p s =
    let (l, rem) = BS8.break p s
    in (l, BS.drop 1 rem)

parseDouble :: ByteString -> Maybe Double
parseDouble bs = do
    let (intstr, fracstr) = breakBS (== '.') bs
    intpart <- parseIntpart (BS.unpack intstr)
    fracpart <- parseFracpart (BS.unpack fracstr)
    return $ intpart + fracpart

parseInteger :: [Word8] -> Maybe Integer
parseInteger = go 0
    where
        go :: Integer -> [Word8] -> Maybe Integer
        go a [] = Just a
        go a (x:xs) = do
            d <- fromIntegral <$> parseDigit x
            let a' = d + a * 10
            go a' xs

parseIntpart :: [Word8] -> Maybe Double
parseIntpart = go 0
    where
        go :: Double -> [Word8] -> Maybe Double
        go a [] = Just a
        go a (x:xs) = do
            d <- fromIntegral <$> parseDigit x
            let a' = d + a * 10
            go a' xs

parseFracpart :: [Word8] -> Maybe Double
parseFracpart = go 10
    where
        go :: Double -> [Word8] -> Maybe Double
        go f [] = Just 0
        go f (x:xs) = do
            d <- (/ f) . fromIntegral <$> parseDigit x
            a <- go (f * 10) xs
            return $ a + d

parseDigit :: Word8 -> Maybe Int
parseDigit x
    | fromIntegral x >= ord '0' && fromIntegral x <= ord '9' = Just $ fromIntegral x - fromIntegral (ord '0')
    | otherwise = Nothing
