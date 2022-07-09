{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Network.Format.LLSD.Conversions
-- Copyright   :  (c) Linden Lab 2009, 2010
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Conversion operations for the simple types
--
-- This module implements the conversion rules for LLSD. The conversion rules
-- define how a received LLSD value of type a should be converted to be used
-- as a value of type b. These conversions are designed to ensure fidelity of
-- intended values as LLSD moves through systems and languages that may have
-- less rich type systems, and/or "common" automatic value conversion rules.
--
-- Conversion in LLSD is always defined to return a value. However, for use
-- by LLIDL, knowledge of the fidelity of the conversion is needed

module Network.Format.LLSD.Conversion (
        Fidelity(..), (~&~), (~|~),
        Conversion,
        ConvertTo,
        ConvertFrom(..),
        convert,
    )
    where

import Data.Maybe (fromJust, listToMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Locale (defaultTimeLocale)

import qualified Data.ByteString.Lazy as Byte
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Network.URI as URI


-- | How accurately a conversion was performed
data Fidelity = Incompatible  -- the value was a type that couldn't convert
              | Unconvertable -- convertable type, but conversion failed
              | Approximate   -- converted, but information was lost
              | Mixed         -- value has both additional and defaulted data
              | Additional    -- value had additional data (for maps and arrays)
              | Defaulted     -- the default value due to undef or ""
              | Converted     -- the value was converted
              | Native        -- value was natively the correct type
              | Matched       -- the value has the correct shape 
    deriving (Eq, Ord, Show)
{-  Notes:

    Mixed, Additional, and Matched are used by LLIDL for maps and arrays. No
    conversions return these values.
          
    This whole scheme is somewhat overblown. There are really only three
    levels of Fidelity LLIDL cares about:
    
        Fidelity        Match   Valid   Invalid
        Incompatible      -       -        x
        Unconvertable     -       -        x
        Approximate       -       -        x
        Defaulted         -       x        -
        Converted         x       x        -
        Native            x       x        -
        
        Integrity       Match   Valid   Invalid
        Incompatible      -       -        x
        Mixed             -       x        -
        Additional        -       x        -
        Defaulted         -       x        -
        Matched           x       x        -
-}

(~&~) :: Fidelity -> Fidelity -> Fidelity
Incompatible ~&~ _          = Incompatible  -- important short circuit case
Defaulted    ~&~ Additional = Mixed
Additional   ~&~ Defaulted  = Mixed
a            ~&~ b          = min a b
infixr 3 ~&~

(~|~) :: Fidelity -> Fidelity -> Fidelity
Matched ~|~ _ = Matched -- important short circuit case
a       ~|~ b = max a b
infixr 2 ~|~



-- | The result of a conversion
type Conversion a = (a, Fidelity)


class ConvertTo a where
    defaultValue :: a
    
    convertFromUndef  :: ()     -> Conversion a
    convertFromBool   :: Bool   -> Conversion a
    convertFromInt    :: Int    -> Conversion a
    convertFromReal   :: Double -> Conversion a
    convertFromString :: String -> Conversion a
    convertFromUUID   :: UUID.UUID       -> Conversion a
    convertFromDate   :: Time.UTCTime    -> Conversion a
    convertFromURI    :: URI.URI         -> Conversion a
    convertFromBinary :: Byte.ByteString -> Conversion a
    
    -- default implementations
    convertFromUndef  = const $ defaultWith Defaulted
    convertFromBool   = const $ defaultWith Incompatible
    convertFromInt    = const $ defaultWith Incompatible
    convertFromReal   = const $ defaultWith Incompatible
    convertFromString = const $ defaultWith Incompatible
    convertFromUUID   = const $ defaultWith Incompatible
    convertFromDate   = const $ defaultWith Incompatible
    convertFromURI    = const $ defaultWith Incompatible
    convertFromBinary = const $ defaultWith Incompatible



native :: a -> Conversion a
native a = (a, Native)

convertAsJust :: a -> Conversion a
convertAsJust a = (a, Converted)

defaultWith :: (ConvertTo a) => Fidelity -> Conversion a
defaultWith f = (defaultValue, f)

convertString :: (ConvertTo a) =>
                     (String -> Maybe a) -> String -> Conversion a
convertString f s =
    if null s
        then defaultWith Defaulted
        else maybe (defaultWith Unconvertable) convertAsJust $ f s


          
instance ConvertTo Bool where
    defaultValue = False
    convertFromBool     = native
    convertFromInt      = boolConv 0   1      (/= 0)
    convertFromReal     = boolConv 0.0 1.0    (\v -> v < 0.0 || 0.0 < v)
                            -- written this way to handle NaNs
    convertFromString   = boolConv ""  "true" (/= "")

boolConv :: (Eq a) => a -> a -> (a -> Bool) -> a -> Conversion Bool
boolConv vFalse vTrue f v = (f v, if canonical then Converted else Approximate)
    where canonical = v == vFalse || v == vTrue


instance ConvertTo Int where
    defaultValue = 0
    convertFromBool v   = convertAsJust $ if v then 1 else 0
    convertFromInt      = native
    convertFromReal     = intFromDouble
    convertFromString   = intFromDoubleConv . convertFromString

intFromDouble :: Double -> Conversion Int
intFromDouble d = (i, f)
    where i = round d
          f = if fromIntegral i == d then Converted else Approximate
          
intFromDoubleConv :: Conversion Double -> Conversion Int
intFromDoubleConv (d, Converted) = intFromDouble d
intFromDoubleConv (_, f)         = (defaultValue, f)


instance ConvertTo Double where
    defaultValue = 0.0
    convertFromBool v   = convertAsJust $ if v then 1.0 else 0.0
    convertFromInt      = convertAsJust . realToFrac
    convertFromReal     = native
    convertFromString   = convertString readDouble

instance ConvertTo String where
    defaultValue = ""
    convertFromBool v   = convertAsJust $ if v then "true" else ""
    convertFromInt      = convertAsJust . show
    convertFromReal     = convertAsJust . show
    convertFromString   = native
    convertFromUUID     = convertAsJust . UUID.toString
    convertFromDate     =
        convertAsJust . Time.formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
    convertFromURI v    = convertAsJust $ URI.uriToString id v ""
    
instance ConvertTo UUID.UUID where
    defaultValue =
        fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
        -- FIXME: replace when UUID exports the nilUUID
    convertFromString  = convertString UUID.fromString
    convertFromUUID    = native

instance ConvertTo Time.UTCTime where
    defaultValue = posixSecondsToUTCTime 0
    convertFromString =
        convertString $ Time.parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
    convertFromDate = native

instance ConvertTo URI.URI where
    defaultValue = URI.nullURI
    convertFromString = convertString URI.parseURIReference
    convertFromURI = native

instance ConvertTo Byte.ByteString where
    defaultValue = Byte.empty
    convertFromBinary = native



class ConvertFrom a where
    conversion :: (ConvertTo b) => a -> Conversion b

instance ConvertFrom ()              where conversion = convertFromUndef
instance ConvertFrom Bool            where conversion = convertFromBool
instance ConvertFrom Int             where conversion = convertFromInt
instance ConvertFrom Double          where conversion = convertFromReal
instance ConvertFrom String          where conversion = convertFromString
instance ConvertFrom UUID.UUID       where conversion = convertFromUUID
instance ConvertFrom Time.UTCTime    where conversion = convertFromDate
instance ConvertFrom URI.URI         where conversion = convertFromURI
instance ConvertFrom Byte.ByteString where conversion = convertFromBinary



convert :: (ConvertFrom a, ConvertTo b) => a -> b
convert = fst . conversion



readDouble :: String -> Maybe Double
readDouble = listToMaybe . map fst . filter ((=="") . snd) . reads

