-- |
-- Module      : Network.HTTP.AltSvc.Utils
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : stable
-- Portability : good
--
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.AltSvc.Utils
    ( skipWord8
    , getMany
    , skipOWP
    , istchar
    , getExpected
    , getToken
    , putToken
    , getQuoted
    , putQuoted
    , getCommaList
    , putCommaList
    ) where

import Control.Applicative
import Control.Monad

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Serialize
import Data.Word (Word8)

skipWord8 :: Word8 -> Get ()
skipWord8 b = getWord8 >>= guard . (== b)

skipMany :: Get a -> Get ()
skipMany skipOne = (skipOne >> skipMany skipOne) <|> return ()

getMany :: Get a -> Get [a]
getMany getOne = go <|> return []
  where go = do { a <- getOne; as <- getMany getOne; return (a:as) }

skipWSP :: Get ()
skipWSP = label "whitespace" $ getWord8 >>= guard . flip B.elem " \t"

skipOWP :: Get ()
skipOWP = skipMany skipWSP

getExpected :: ByteString -> Get ()
getExpected expected =
    getBytes (B.length expected) >>= \bs -> guard (bs == expected)

isvchar :: Word8 -> Bool
isvchar b = b > 0x20 && b < 0x80  -- visible (printing) characters

istchar :: Word8 -> Bool
istchar b = isvchar b && B.notElem b "\"(),/:;<=>?@[\\]{}"

getToken :: Get ByteString
getToken = B.pack <$> getTokenBytes
  where
    getTokenBytes = do
        b <- getWord8
        guard (istchar b)
        (b :) <$> (getTokenBytes <|> return [])

putToken :: Putter ByteString
putToken = putByteString

getQuoted :: Get ByteString
getQuoted = label "double quote" (skipWord8 0x22) >> (B.pack <$> getInner)
  where
    getInner = do
        b <- getWord8
        case b of
            0x22 -> return []
            0x5c -> label "quoted byte" getWord8 >>= \c -> (c :) <$> getInner
            _    -> (b :) <$> getInner

putQuoted :: Putter ByteString
putQuoted bs = do
    putWord8 0x22
    mapM_ putQuotedByte (B.unpack bs)
    putWord8 0x22
  where
    putQuotedByte 0x22 = putByteString "\\\""
    putQuotedByte 0x5c = putByteString "\\\\"
    putQuotedByte b    = putWord8 b

getCommaList :: Show a => Get a -> Get [a]
getCommaList getOne = do
    a <- getOne
    skipOWP
    as <- (skipComma >> skipOWP >> getCommaList getOne) <|> return []
    return (a:as)
  where
    skipComma = label "comma" (skipWord8 0x2c)

putCommaList :: Putter a -> Putter [a]
putCommaList _      []     = error "putCommaList: empty list"
putCommaList putOne (a:as) =
    putOne a >> unless (null as) (putByteString ", " >> putCommaList putOne as)
