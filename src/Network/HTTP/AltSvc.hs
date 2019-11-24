-- |
-- Module      : Network.HTTP.AltSvc
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : stable
-- Portability : good
--
-- HTTP Alternative Services, defined in <https://tools.ietf.org/html/rfc7838
-- RFC 7838>.
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.AltSvc
    ( AltSvc(..)
    , AltValue(..)
    ) where

import Control.Applicative
import Control.Monad

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Lazy (toStrict)

import Data.Char (chr)
import Data.Semigroup
import Data.Serialize

import Network.HTTP.AltSvc.Utils

-- | Data type to represent the @Alt-Svc@ header content.  It will generally
-- contain one or more values.  An empty list allows to invalidate all
-- alternatives, which is different from providing no header at all.
--
-- The content can be encoded to/decoded from bytestring using the 'Serialize'
-- instance.
newtype AltSvc = AltSvc [AltValue] deriving (Show, Eq)

instance Serialize AltSvc where
    get = getAltSvc
    put = putAltSvc

-- | An individual alternative service in the @Alt-Svc@ header.
--
-- The content can be encoded to/decoded from bytestring using the 'Serialize'
-- instance.
data AltValue = AltValue
    { altValueProtocolId :: ByteString
      -- ^ The protocol to use for the alternative service.  Has the syntax of
      -- an ALPN protocol name and cannot be empty.
    , altValueHost       :: ByteString
      -- ^ Host name to connect to the alternative service.  Is optional so may
      -- be empty.
    , altValuePort       :: Int
      -- ^ Port number to connect to the alternative service.  Is mandatory and
      -- should not be negative.
    , altValueParams     :: [(ByteString, ByteString)]
      -- ^ Additional parameters for the alternative.  The parameter names must
      -- be valid tokens, which means no delimiter character is accepted and
      -- cannot be empty.  The values may be any bytestring.
    }
    deriving (Show, Eq)

instance Serialize AltValue where
    get = getAltValue
    put = putAltValue

putPercentEncoded :: Putter ByteString
putPercentEncoded bs = mapM_ f (B.unpack bs)
  where
    f 0x25          = putEncodeChar 0x25
    f b | istchar b = putWord8 b
        | otherwise = putEncodeChar b

    putEncodeChar b =
        let (d, r) = b `divMod` 16
            high   = toDigit d
            low    = toDigit r
         in putWord8 0x25 >> putWord8 high >> putWord8 low

    toDigit b | b < 10    = 0x30 + b
              | otherwise = b - 10 + 0x41

getPercentEncoded :: Get ByteString
getPercentEncoded = B.pack <$> parse
  where
    parse = do
        b <- getWord8
        guard (istchar b)
        case b of
            0x25 -> do
                c <- label "percent-encoded byte" $ do
                    d <- getWord8 >>= fromDigit
                    r <- getWord8 >>= fromDigit
                    return $! d * 16 + r
                (c :) <$> parseMore
            _    -> (b :) <$> parseMore

    parseMore = parse <|> return []

    fromDigit b
        | b >= 0x30 && b <= 0x39 = return $! b - 0x30
        | b >= 0x41 && b <= 0x46 = return $! b - 0x41 + 10
        | otherwise              = fail "bad hex digit"

getAltSvc :: Get AltSvc
getAltSvc = AltSvc <$> (getCommaList getAltValue <|> getClear)
  where getClear = getExpected "clear" >> return []

putAltSvc :: Putter AltSvc
putAltSvc (AltSvc [])   = putByteString "clear"
putAltSvc (AltSvc vals) = putCommaList putAltValue vals

getAltValue :: Get AltValue
getAltValue = do
    protocolId <- getPercentEncoded
    label "equals sign" $ skipWord8 0x3d
    authority <- getQuoted
    (host, port) <- either fail return (parseAuth authority)
    params <- getMany getParam
    return AltValue { altValueProtocolId = protocolId
                    , altValueHost       = host
                    , altValuePort       = port
                    , altValueParams     = params
                    }

putAltValue :: Putter AltValue
putAltValue altValue = do
    putPercentEncoded (altValueProtocolId altValue)
    putWord8 0x3d
    putQuoted $ buildAuth (altValueHost altValue) (altValuePort altValue)
    mapM_ putParam (altValueParams altValue)

parseAuth :: ByteString -> Either String (ByteString, Int)
parseAuth authority = case B.elemIndexEnd 0x3a authority of
    Nothing -> Left "invalid authority"
    Just i  ->
        let str = B.drop (i + 1) authority
            p = read (map (chr . fromIntegral) $ B.unpack str)
         in if allDigits str
                then Right (B.take i authority, p)
                else Left "invalid authority port"
  where allDigits = B.all $ \b -> b >= 0x30 && b <= 0x39

buildAuth :: ByteString -> Int -> ByteString
buildAuth host port = toStrict $ B.toLazyByteString authority
  where authority = B.byteString host <> B.word8 0x3a <> B.intDec port

getParam :: Get (ByteString, ByteString)
getParam = do
    skipOWP >> label "semi-colon" (skipWord8 0x3b) >> skipOWP
    name <- getToken
    label "equals sign" $ skipWord8 0x3d
    value <- getToken <|> getQuoted
    return (name, value)

putParam :: Putter (ByteString, ByteString)
putParam (name, value) = do
    putByteString "; "
    putToken name
    putWord8 0x3d
    let validTokenValue = not (B.null value) && B.all istchar value
    if validTokenValue then putToken value else putQuoted value
