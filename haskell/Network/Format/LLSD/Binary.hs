-- |
-- Module      :  Network.Format.LLSD.Binary
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Network.Format.LLSD.Binary (
    formatBinary,
    parseBinary
    )
    where

import Control.Monad (liftM, replicateM, when)
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy as L
import Data.Char (chr, ord)
import qualified Data.Map as Map
import Data.UUID (UUID)
import Data.Serialize
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word (Word64)
import Network.Format.LLSD.IEEE754
import Network.Format.LLSD.Internal
import Network.URI (URI, uriToString)


parseBinary :: B.ByteString -> Either String LLSD
parseBinary = decode

formatBinary :: LLSD -> L.ByteString
formatBinary = toLazyByteString . encode

instance Serialize LLSD where
    get = getLLSD
    put = putLLSD


getLLSD :: Get LLSD
getLLSD = do
    c <- getChar8
    case c of
        '!' -> return undef
        '1' -> return $ toLLSD True
        '0' -> return $ toLLSD False
        'i' -> getInt32be >>= return . toLLSD
        'r' -> getWord64be >>= return . toLLSD . decodeIEEEDouble
        's' -> getString >>= return . toLLSD
        'u' -> getUUID >>= return . toLLSD
        'd' -> getWord64be >>= return . toLLSD . decodeDate
        'l' -> getString >>= return . toLLSD
                    . (fromLLSD . toLLSD :: String -> URI)
        'b' -> getInt32be >>= getLazyByteString . fromIntegral
                    >>= return . toLLSD
        '[' -> getLLSDArray
        '{' -> getLLSDMap
        _   -> fail $ "unrecognized binary tag: " ++ show c

getLLSDArray :: Get LLSD
getLLSDArray = do
    n <- getInt32be
    vs <- replicateM n getLLSD
    validateChar8 ']' "missing closing bracket on array"
    return $ toLLSD vs

getLLSDMap :: Get LLSD
getLLSDMap = do
    n <- getInt32be
    vs <- replicateM n getLLSDEntry
    validateChar8 '}' "missing closing bracket on map"
    return $ toLLSD $ Map.fromAscList vs

getLLSDEntry :: Get (String, LLSD)
getLLSDEntry = do
    validateChar8 'k' "missing key in map"
    k <- getString
    v <- getLLSD
    return (k, v)

validateChar8 :: Char -> String -> Get ()
validateChar8 c msg = do
    a <- getChar8
    when (a /= c) $ fail msg




putLLSD :: Putter LLSD
putLLSD (LUndef)        = putChar8 '!'
putLLSD (LBool True)    = putChar8 '1'
putLLSD (LBool False)   = putChar8 '0'
putLLSD (LInt v)        = putChar8 'i' >> putInt32be v
putLLSD (LReal v)       = putChar8 'r' >> putWord64be (encodeIEEEDouble v)
putLLSD (LString v)     = putChar8 's' >> putString v
putLLSD (LUUID v)       = putChar8 'u' >> putUUID v
putLLSD (LDate v)       = putChar8 'd' >> putWord64be (encodeDate v)
putLLSD (LURI v)        = putChar8 'l' >> putString (uriToString id v "")
putLLSD (LBinary v)     = putChar8 'b' >> putInt32be (fromIntegral $ L.length v)
                                       >> putLazyByteString v
putLLSD (LArray m)      = putChar8 '[' >> putLLSDArray m >> putChar8 ']'
putLLSD (LMap m)        = putChar8 '{' >> putLLSDMap m >> putChar8 '}'


putLLSDArray :: Putter (Map.Map Int LLSD)
putLLSDArray m = putInt32be (length vs) >> mapM_ putLLSD vs
                    where vs = expandLLSDArray m

putLLSDMap :: Putter (Map.Map String LLSD)
putLLSDMap m = putInt32be (length vs) >> mapM_ putLLSDEntry vs
                    where vs = Map.assocs m

putLLSDEntry :: Putter (String, LLSD)
putLLSDEntry (k, v) = putChar8 'k' >> putString k >> putLLSD v



--
-- Utilities to make working with Serial easier
--

getChar8 :: Get Char
getChar8 = liftM (chr . fromIntegral) getWord8
putChar8 :: Putter Char
putChar8 = putWord8 . fromIntegral . ord

getInt32be :: Get Int
getInt32be = liftM fromIntegral getWord32be
putInt32be :: Putter Int
putInt32be = putWord32be . fromIntegral

getString :: Get String
getString = getInt32be >>= getByteString >>= return . U.toString
putString :: Putter String
putString s = putInt32be (B.length u) >> putByteString u
    where u = U.fromString s

-- These are a hack until Data.UUID supports either extracting the bytes
-- or being an instance of Serialize
getUUID :: Get UUID
getUUID = getLazyByteString 16 >>= \bs -> return (Binary.decode bs :: UUID)
putUUID :: Putter UUID
putUUID = putLazyByteString . Binary.encode


--
-- Conversion Utilities
--

toByteString :: L.ByteString -> B.ByteString
toByteString = B.concat . L.toChunks

toLazyByteString :: B.ByteString -> L.ByteString
toLazyByteString = L.fromChunks . (:[])

decodeDate :: Word64 -> UTCTime
decodeDate = posixSecondsToUTCTime . realToFrac . decodeIEEEDouble

encodeDate :: UTCTime -> Word64
encodeDate = encodeIEEEDouble . realToFrac . utcTimeToPOSIXSeconds



{-
    Notes on cereal

    needed utility functions getInt32be, getChar8, etc....
    needed utility functions for IEEE floats / doubles
    getBytes but not putBytes
    needing to include Serial, Serial.Get, Serial.Put seems excessive
    built in instances seem generally unhelpful

-}
