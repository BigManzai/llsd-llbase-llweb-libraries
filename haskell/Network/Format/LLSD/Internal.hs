{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}

-- |
-- Module      :  Network.Format.LLSD.Internal
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Internal Implementation
-- Other modules inside LLSD include this in order to get at the details
-- and utility functions. The only exposed module, Data.LLSD, re-exports
-- ony the public bits from here.

module Network.Format.LLSD.Internal where

{-
    Missing operations

        has :: String -> LLSD -> Bool

        delete :: String -> LLSD -> LLSD
        append :: (SData a) -> a -> LLSD -> LLSD

        pretty :: LLSD -> PP.Doc
-}

import qualified Data.ByteString.Lazy as Byte
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import Network.Format.LLSD.Conversion
import qualified Network.URI as URI


{- |
    An LLSD value. May represent a single value of any of the LLSD types:
    Undefined, Boolean, Integer, Real, String, UUID, Data, URI or Binary; or may
    be an Array or Map of LLSD values.
-}
data LLSD = LUndef
          | LBool Bool
          | LInt Int
          | LReal Double
          | LString String
          | LUUID UUID.UUID
          | LDate Time.UTCTime
          | LURI URI.URI
          | LBinary Byte.ByteString
          | LArray (M.Map Int LLSD)
          | LMap (M.Map String LLSD)



-- | An undefined LLSD value. Useful for a starting point for building up values
-- using the 'with' construct:
--
-- > llsd `with` "name" .= "Amy"
-- >      `with` "skill" .= 42
llsd :: LLSD
llsd = LUndef

-- | The LLSD with the value of type Undefined.
undef :: LLSD
undef = LUndef

-- | The number of values in a map or array, else 0.
size :: LLSD -> Int
size (LArray m) = M.size m
size (LMap m) = M.size m
size _ = 0


expandLLSDArray :: M.Map Int LLSD -> [LLSD]
expandLLSDArray m = map (vOrUndef . lookupIn m) [0..maxIndex]
    where vOrUndef v = fromMaybe undef v
          lookupIn = flip M.lookup
          maxIndex = if M.null m then (-1) else fst . M.findMax $ m


conversionFromLLSD :: (ConvertTo a) => LLSD -> Conversion a
conversionFromLLSD (LUndef)    = conversion ()
conversionFromLLSD (LBool v)   = conversion v
conversionFromLLSD (LInt v)    = conversion v
conversionFromLLSD (LReal v)   = conversion v
conversionFromLLSD (LString v) = conversion v
conversionFromLLSD (LUUID v)   = conversion v
conversionFromLLSD (LDate v)   = conversion v
conversionFromLLSD (LURI v)    = conversion v
conversionFromLLSD (LBinary v) = conversion v
conversionFromLLSD _           = conversion ()

convertFromLLSD :: (ConvertTo a) => LLSD -> a
convertFromLLSD = fst. conversionFromLLSD


{- |
    Types that can be converted to and from LLSD.

    LLSD was designed to be very resilient and flexible in light of difference
    between end points. In particular, it has well defined, though liberal
    type conversion, designed to allow data to map directly into the data
    system of an implementation.

    Instances of SData can be converted to and from LLSD values. The LLSD types
    are handled by these provided instances of SData:

        * LLSD Boolean <-> 'Bool'

        * LLSD Integer <-> 'Int'

        * LLSD Real <-> 'Double'

        * LLSD String <-> 'String'

        * LLSD UUID <-> 'UUID' (from 'Data.UUID')

        * LLSD Date <-> 'UTCTime' (from 'Data.Time')

        * LLSD URI <-> 'URI' (from 'Network.URI')

        * LLSD Binary <-> 'ByteString' (from 'Data.ByteString.Lazy')

    Other Haskell types can be extended to support conversion to and from LLSD
    by being made instances of SData:

    @
data Address = Address String String String Int
    deriving (Eq, Show)

instance SData Address where
    toLLSD (Address street city state zipcode) =
        llsd `with` "street" .= street
             `with` "city" .= city
             `with` "state" .= state
             `with` "zip" .= zipcode
    fromLLSD v =
        Address (v `at` "street")
                (v `at` "city")
                (v `at` "state")
                (v `at` "zip")
    @

    For a given @'SData a'@ type, @[a]@ and @'Map' 'String' a@ are also
    instances of SData, allow easy conversion to and from such structures.

    Finally, @'LLSD'@ itself is an instance of @'SData'@, which is convienent
    for extracting lists or maps of heterogenous values from an LLSD.
-}
class SData a where
    -- | Convert the value to an LLSD value.
    toLLSD :: a -> LLSD
    -- | Convert an LLSD value to a value of the given type.
    -- The instances for the base types perform conversion
    -- as per the LLSD spec, if the underlying LLSD type doesn't match.
    fromLLSD :: LLSD -> a

instance SData LLSD where
    toLLSD = id
    fromLLSD = id

{-
instance SData () where
    toLLSD () = LUndef
    fromLLSD _ = ()
    -- Dang, this won't compile!
-}

instance (SData a) => SData (Maybe a) where
    toLLSD Nothing = LUndef
    toLLSD (Just v) = toLLSD v
    fromLLSD LUndef = Nothing
    fromLLSD l = Just $ fromLLSD l
    -- FIXME: Not clear if this is correct behavior
    -- should it be Nothing unless it is of the right type?
    -- should this even be defined?


instance SData Bool where
    toLLSD b = LBool b
    fromLLSD = convertFromLLSD

instance SData Int where
    toLLSD v = LInt v
    fromLLSD = convertFromLLSD

instance SData Double where
    -- FIXME: generalize to all Fractionals some how
    toLLSD v = LReal v
    fromLLSD = convertFromLLSD

instance SData String where
    -- FIXME: requires OverlappingInstances which seems scary
    toLLSD v = LString v
    fromLLSD = convertFromLLSD

instance SData UUID.UUID where
    toLLSD v = LUUID v
    fromLLSD = convertFromLLSD

instance SData Time.UTCTime where
    toLLSD v = LDate v
    fromLLSD = convertFromLLSD

instance SData URI.URI where
    toLLSD v = LURI v
    fromLLSD = convertFromLLSD

instance SData Byte.ByteString where
    toLLSD v = LBinary v
    fromLLSD = convertFromLLSD

instance (SData a) => SData [a] where
    toLLSD as = LArray $ M.fromDistinctAscList $ zip [0..] $ map toLLSD as
    fromLLSD (LArray m) = map fromLLSD $ expandLLSDArray m
    fromLLSD _ = []

instance (SData a) => SData (M.Map String a) where
    toLLSD m = LMap (M.map toLLSD m)
    fromLLSD (LMap m) = M.map fromLLSD m
    fromLLSD _ = M.empty

instance (SData a) => SData [(String, a)] where
    toLLSD = toLLSD . M.fromList
    fromLLSD = M.toList . fromLLSD



-- seems like it would be okay to export LLSDSegment, it's constructors,
-- and LLSDPath... if cleaned up

data LLSDSegment = PIndex Int | PKey String
    deriving Show

type LLSDPath = [LLSDSegment]

class SPath a where
    toPath :: a -> LLSDPath

instance SPath LLSDPath where
    toPath = id

instance SPath Int where
    toPath i = [ PIndex i ]

instance SPath String where
    toPath k = [ PKey k ]

class SSegment a where
    toSegment :: a -> LLSDSegment

instance SSegment LLSDSegment where
    toSegment = id

instance SSegment Int where
    toSegment i = PIndex i

instance SSegment String where
    toSegment k = PKey k


{--  Native LLSD access interface

        All these functions take or return SData values. These values have
        toLLSD applied when combined into an LLSD, and fromLLSD applied when
        returned. Since LLSD is an instance of SData, then can be used with
        other LLSD values, any of the base types that LLSD supports, or your
        own extensions (via instancing SData).

        The set family of functions all return a copy of an LLSD with a
        given within the LLSD updated. These operations will
        force values within the LLSD to become arrays or maps as needed.
--}

innerArray :: LLSD -> M.Map Int LLSD
innerArray (LArray m) = m
innerArray  _         = M.empty

innerMap :: LLSD -> M.Map String LLSD
innerMap (LMap m) = m
innerMap  _       = M.empty

setAtIndex :: (SData a) => Int -> a -> LLSD -> LLSD
-- ^ Return an updated LLSD array, with the index set to the given value
setAtIndex i v d = LArray $ M.insert i (toLLSD v) (innerArray d)


setAtKey :: (SData a) => String -> a -> LLSD -> LLSD
-- ^ Return an updated LLSD map, with the key set to the given value
setAtKey k v d = LMap $ M.insert k (toLLSD v) (innerMap d)


setSegment :: (SData a) => LLSDSegment -> a -> LLSD -> LLSD
setSegment (PIndex i) = setAtIndex i
setSegment (PKey k) = setAtKey k

setPath :: (SData a) => LLSDPath -> a -> LLSD -> LLSD
setPath (p:ps) v d = setSegment p (setPath ps v (getSegment p d)) d
setPath []     v _ = toLLSD v

set :: (SPath a, SData b) => a -> b -> LLSD -> LLSD
-- ^ Return a copy of the LLSD, with the value at the given path
set p v = setPath (toPath p) (toLLSD v)


-- | Return a the value at an LLSD array
getAtIndex :: (SData a) => Int -> LLSD -> a
getAtIndex i d = fromLLSD $ fromMaybe undef $ M.lookup i (innerArray d)

-- | Return a value in an LLSD map
getAtKey :: (SData a) => String -> LLSD -> a
getAtKey k d = fromLLSD $ fromMaybe undef $ M.lookup k (innerMap d)

getSegment :: (SData a) => LLSDSegment -> LLSD -> a
getSegment (PIndex i) = getAtIndex i
getSegment (PKey i) = getAtKey i

getPath :: (SData a) => LLSDPath -> LLSD -> a
getPath p d = fromLLSD $ foldl' (flip getSegment) d p

-- | Retun a value in an LLSD at a given path
get :: (SPath a, SData b) => a -> LLSD -> b
get p = getPath (toPath p)

-- | Test if an LLSD is an array, and has an entry at the index
-- Note: The entry could be undef
hasIndex :: Int -> LLSD -> Bool
hasIndex i d = M.member i (innerArray d)

-- | Test if an LLSD is a map, and has a entry for the key
hasKey :: String -> LLSD -> Bool
hasKey k d = M.member k (innerMap d)

-- | Test if an LLSD has a value (even undef) at a given path.
-- Note: A value may be 'fetched' from the path even if this function
-- returns False, though the value will be the default for the type.
has :: (SPath a) => a -> LLSD -> Bool
has p d = isJust $ foldl' hasSegment (Just d) (toPath p)
    where hasSegment Nothing _ = Nothing
          hasSegment (Just e) (PIndex i) =  M.lookup i (innerArray e)
          hasSegment (Just e) (PKey k) = M.lookup k (innerMap e)



(./) :: (SSegment a, SPath b) => a -> b -> LLSDPath
-- ^ Combine a segment and a path into a path
a ./ b = toSegment a : toPath b
infixr 4 ./

at :: (SPath a, SData b) => LLSD -> a -> b
-- ^ Return a value in an LLSD at a path
d `at` ps =  get ps d
infixl 2 `at`

with :: (SPath a, SData b) => LLSD -> (a, b) -> LLSD
-- ^ Return a copy of the LLSD with the given path updated to a new value
d `with` (ps, v) = set ps v d
infixl 2 `with`

(.=) :: (SPath a, SData b) => a -> b -> (a, b)
-- ^ Suppy the value to set at a path, for use with with
ps .= v = (ps, v)
infix 3 .=


toMap :: (SData a) => LLSD -> M.Map String a
-- ^ Convert to a Data.Map
toMap = fromLLSD -- M.map fromLLSD . innerMap

toList :: (SData a) => LLSD -> [a]
-- ^ Convert to a list
toList = map fromLLSD . expandLLSDArray . innerArray

