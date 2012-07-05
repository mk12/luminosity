-- Copyright 2012 Mitchell Kember.

module Export (savePng) where

-- import Data.Array

import Data.Bits (shiftR, xor)
import Data.Digest.CRC32 (crc32)
-- import Data.List
import Data.Word ()
import qualified Codec.Compression.Zlib as Z
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as S
import qualified Data.ByteString.Lazy.Char8 as C

-- be8 :: Word8 -> B.ByteString
-- be8 = B.singleton
-- 
-- be32 :: Word32 -> B.ByteString
-- be32 x = B.pack [fromIntegral (x `shiftR` sh) | sh <- [24, 16, 8, 0]]
-- be32 x = B.pack

infixr 4 <>
(<>) = B.append

header, hdr, dat, end :: B.ByteString
header = B.pack [137, 80, 78, 71, 13, 10, 26, 10]
(hdr, dat, end) = map C.pack ["IHDR", "IDAT", "IEND"]

chunk :: S.ByteString -> S.ByteString -> Builder
chunk tag xs = B.putWord32be (fromIntegral $ S.length xs)
            <> B.fromLazyByteString dat
            <> B.fromLazyByteString $ B.putWord32be (crc32 dat)
    where dat = S.append tag xs
