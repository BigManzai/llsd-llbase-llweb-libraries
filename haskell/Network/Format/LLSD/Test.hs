{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}

-- |
-- Module      :  Network.Format.LLSD.Test
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Network.Format.LLSD.Test (
    runTests,
    runChecks
    )
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (ord)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time
import qualified Data.UUID as UUID
import Network.Format.LLSD
import qualified Network.URI as URI
import System.IO (stderr)
import Test.HUnit
import Test.QuickCheck


{-
    unit tests to write
        path access
        updating
        SData extension

        llsd
        size

        get 34 l
        get "foo" l

        set 34 thing l
        set "foo" thing l

        getPath ("abc" ./ "def") l
        setPath ("abc" ./ "def") v l
            -- test paths with strings and with numbers

        test parsing of with, at, ./ and .=

-}


instance Arbitrary Char where
    arbitrary     = choose ('\32', '\xcfff')  -- FIXME: expand to all Unicode chars
    coarbitrary c = variant (ord c `rem` 4)
    -- boilerplate for using Char with Test.QuickCheck

--
-- Identity Conversion Tests
--

prop_toFromIdentity :: (Eq a, SData a) => a -> Bool
prop_toFromIdentity a = fromLLSD (toLLSD a) == a

prop_BoolToFromIdentity :: Bool -> Bool
prop_BoolToFromIdentity = prop_toFromIdentity

prop_IntToFromIdentity :: Int -> Bool
prop_IntToFromIdentity = prop_toFromIdentity

prop_DoubleToFromIdentity :: Double -> Bool
prop_DoubleToFromIdentity = prop_toFromIdentity

prop_StringToFromIdentity :: String -> Bool
prop_StringToFromIdentity = prop_toFromIdentity

nullUUID = fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
someUUID = fromJust $ UUID.fromString "14409e2f-5588-4c12-b719-ff33f614e3b7"

nullDate = UTCTime (fromGregorian 1970 01 01) (secondsToDiffTime        0)
someDate = UTCTime (fromGregorian 2003 06 23) (secondsToDiffTime $ 4*3600)

nullURI = URI.nullURI
someURI = fromJust $ URI.parseURI
    "http://slurl.com/secondlife/Ambleside/57/104/26"

nullBytes = L.empty
someBytes = L.pack [222, 173, 190, 239]

test_identity = "scalar type identities" ~: TestList [
    prop_toFromIdentity True                    ~? "true identity",
    prop_toFromIdentity False                   ~? "false identity",
    prop_toFromIdentity(42::Int)                ~? "42 identity",
    prop_toFromIdentity(0::Int)                 ~? "zero identity",
    prop_toFromIdentity(-12345::Int)            ~? "neg identity",
    prop_toFromIdentity(2000000000::Int)        ~? "big identity",
    prop_toFromIdentity(3.14159265359::Double)  ~? "pi identity",
    prop_toFromIdentity(6.7e256::Double)        ~? "big float identity",
    prop_toFromIdentity "now is the time"       ~? "string identity",
    prop_toFromIdentity nullUUID                ~? "null uuid identity",
    prop_toFromIdentity someUUID                ~? "some uuid identity",
    prop_toFromIdentity nullDate                ~? "null date identity",
    prop_toFromIdentity someDate                ~? "some date identity",
    prop_toFromIdentity nullURI                 ~? "null URI identity",
    prop_toFromIdentity someURI                 ~? "some URI identity",
    prop_toFromIdentity nullBytes               ~? "null byte array",
    prop_toFromIdentity someBytes               ~? "some byte array"
    ]

props_identity = [
    label "prop_BoolToFromIdentity"     prop_BoolToFromIdentity,
    label "prop_IntToFromIdentity"      prop_IntToFromIdentity,
    label "prop_DoubleToFromIdentity"   prop_DoubleToFromIdentity,
    label "prop_StringToFromIdentity"   prop_StringToFromIdentity
    ]

-- FIXME: test identity conversion of Maybe abs
-- FIXME: test identity conversion of lists
-- FIXME: test identity conversion of maps



--
-- Conversion Tests
--

test_conversion :: (SData a, Show a) => a -> Bool -> Int -> Double -> String -> Test
test_conversion av eb ei ed es =
        show av ~: TestList $ map TestCase [
            fromLLSD al @?= eb,
            fromLLSD al @?= ei,
            not (fromLLSD al < ed || fromLLSD al > ed) @? "double compare",
                -- written this way to handle NaNs correctly
            fromLLSD al @?= es
            ]
    where al = toLLSD av

test_IntConversion :: Int -> Bool -> Int -> Double -> String -> Test
test_IntConversion = test_conversion

test_DoubleConversion :: Double -> Bool -> Int -> Double -> String -> Test
test_DoubleConversion = test_conversion

test_toFromString :: (SData a, Show a, Eq a) => a -> String -> Test
test_toFromString av es =
        show av ~: TestList $ map TestCase [
            fromLLSD (toLLSD av) @?= es,
            fromLLSD (toLLSD es) @?= av
            ]

test_conversions = "conversions" ~: TestList [
        test_conversion llsd        False   0       0.0         "",
        test_conversion False       False   0       0.0         "",
        test_conversion True        True    1       1.0         "true",
        test_IntConversion 0        False   0       0.0         "0",
        test_IntConversion 1        True    1       1.0         "1",
        test_IntConversion (-33)    True    (-33)   (-33.0)     "-33",
        test_DoubleConversion 0.0   False   0       0.0         "0.0",
            -- differs from C, which gives just "0" for string conversion
        test_DoubleConversion 0.5   True    0       0.5         "0.5",
        test_DoubleConversion 0.9   True    1       0.9         "0.9",
            -- differs from C, which seems to use trunc for int conversion
        test_DoubleConversion (-3.9)
                                    True    (-4)    (-3.9)      "-3.9",
            -- differs from C, which seems to use trunc for int conversion
        test_DoubleConversion (sqrt(-1))
                                    False   0       (sqrt(-1))  "NaN",
            -- differs from C, which gives "nan" for string conversion
        test_conversion ""          False   0       0.0         "",
        test_conversion "0"         True    0       0.0         "0",
        test_conversion "10"        True    10      10.0        "10",
        test_conversion "-2.345"    True    (-2)    (-2.345)    "-2.345",
        test_conversion "apple"     True    0       0.0         "apple",
        test_conversion "33bob"     True    0       0.0         "33bob",
        test_conversion " "         True    0       0.0         " ",
        test_conversion "\n"        True    0       0.0         "\n",
        test_toFromString nullUUID  "00000000-0000-0000-0000-000000000000",
        test_toFromString someUUID  "14409e2f-5588-4c12-b719-ff33f614e3b7",
        test_toFromString nullDate  "1970-01-01T00:00:00Z",
        test_toFromString someDate  "2003-06-23T04:00:00Z",
        test_toFromString nullURI   "",
        test_toFromString someURI
            "http://slurl.com/secondlife/Ambleside/57/104/26"
     ]


compile_fromLLSD :: LLSD -> Bool
compile_fromLLSD v = b && (i /= 0) && (d /= 0.0) && (s /= "")
    where b :: Bool
          b = fromLLSD v
          i :: Int
          i = fromLLSD v
          d :: Double
          d = fromLLSD v
          s :: String
          s = fromLLSD v

compile_toLLSD :: Bool -> Int -> Double -> String -> LLSD
compile_toLLSD b i d s = toLLSD [vb, vi, vd, vs]
    where vb, vi, vd, vs :: LLSD
          vb = toLLSD b
          vi = toLLSD i
          vd = toLLSD d
          vs = toLLSD s


prop_arrayHasIndex :: [Int] -> Bool
prop_arrayHasIndex is = all sameAsElem $ range is
    where ls = foldl' (\l i -> l `with` i .= "bob") undef is
          sameAsElem i = (hasIndex i ls) == (elem i is)
          range [] = [-1..1]
          range is' = [minimum is' - 1 .. maximum is' + 1]

prop_mapHasKey :: [String] -> Bool
prop_mapHasKey ks = all sameAsElem $ range ks
    where ls = foldl' (\l k -> l `with` k .= "bob") undef ks
          sameAsElem k = (hasKey k ls) == (elem k ks)
          range [] = ["a", "z"]
          range ks' = ks ++ map (++"x") ks' ++ map ("x"++) ks'
{-
prop_mapHasMaps :: [String] -> Bool
prop_mapHasMaps ks = all sameAsPrefix $ paths ks
    where ls = foldl' (\l k -> l `with` k .= "yo") undef ks
          sameAsPrefix p = (hasPath p ls) == (isPrefixOf p ks)
-}

props_has = [
    label "prop_arrayHas"     prop_arrayHasIndex,
    label "prop_mapHas"       prop_mapHasKey
    ]

--
-- XML Format Tests
--

test_xmlFormat :: (SData a, Show a) => a -> String -> Test
test_xmlFormat v xml = xml ~=? (U.toString . formatXML . toLLSD $ v)

test_xmlFormats = "XML formats" ~: TestList [
    test_xmlFormat undef        "<llsd><undef/></llsd>",
    test_xmlFormat True         "<llsd><boolean>true</boolean></llsd>",
    test_xmlFormat False        "<llsd><boolean>false</boolean></llsd>",
    test_xmlFormat (3463::Int)  "<llsd><integer>3463</integer></llsd>",
    test_xmlFormat ""           "<llsd><string/></llsd>",
    test_xmlFormat "foobar"     "<llsd><string>foobar</string></llsd>",
    test_xmlFormat (1.0::Double) "<llsd><real>1.0</real></llsd>",

    test_xmlFormat nullUUID
            "<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>",
    test_xmlFormat someUUID
            "<llsd><uuid>14409e2f-5588-4c12-b719-ff33f614e3b7</uuid></llsd>",
    test_xmlFormat nullDate
            "<llsd><date>1970-01-01T00:00:00Z</date></llsd>",
    test_xmlFormat someDate
            "<llsd><date>2003-06-23T04:00:00Z</date></llsd>",
    test_xmlFormat nullURI
            "<llsd><uri/></llsd>",
    test_xmlFormat someURI
            "<llsd><uri>http://slurl.com/secondlife/Ambleside/57/104/26</uri></llsd>",
    test_xmlFormat nullBytes
            "<llsd><binary encoding=\"base64\"/></llsd>",
    test_xmlFormat someBytes
            "<llsd><binary encoding=\"base64\">3q2+7w==</binary></llsd>",

    test_xmlFormat ([]::[Int])  "<llsd><array/></llsd>",
    test_xmlFormat [undef]      "<llsd><array><undef/></array></llsd>",
    test_xmlFormat [undef, toLLSD (1::Int)]
            "<llsd><array><undef/><integer>1</integer></array></llsd>",
    test_xmlFormat (Map.empty :: Map.Map String Int)
            "<llsd><map/></llsd>",
    test_xmlFormat (Map.fromAscList [("foo", "bar")])
            "<llsd><map><key>foo</key><string>bar</string></map></llsd>",
    test_xmlFormat (Map.fromAscList [("foo", toLLSD "bar"), ("baz", undef)])
            "<llsd><map><key>foo</key><string>bar</string><key>baz</key><undef/></map></llsd>"
    ]



--
-- Binary Format Tests
--

test_binaryFormat :: (SData a, Show a) => a -> String -> Test
test_binaryFormat v binstr = binstr ~=? (C.unpack . formatBinary . toLLSD $ v)

test_binaryFormats = "Binary formats" ~: TestList [
    test_binaryFormat undef        "!",
    test_binaryFormat True         "1",
    test_binaryFormat False        "0",
    test_binaryFormat (3463::Int)  "i\x00\x00\x0d\x87",
    test_binaryFormat ""           "s\x00\x00\x00\x00",
    test_binaryFormat "foobar"     "s\x00\x00\x00\x06\&foobar",

    test_binaryFormat nullUUID
            "u\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00",
    test_binaryFormat someUUID
            "u\x14\x40\x9e\x2f\x55\x88\x4c\x12\xb7\x19\xff\x33\xf6\x14\xe3\xb7",
    test_binaryFormat nullURI
            "l\x00\x00\x00\x00",
    test_binaryFormat someURI
            "l\x00\x00\x00\x2fhttp://slurl.com/secondlife/Ambleside/57/104/26",

    test_binaryFormat (1.0::Double)     "r\x3f\xf0\x00\x00\x00\x00\x00\x00",
    test_binaryFormat (3.14159::Double) "r\x40\x09\x21\xF9\xF0\x1B\x86\x6E",

    test_binaryFormat nullDate          "d\x00\x00\x00\x00\x00\x00\x00\x00",
    test_binaryFormat someDate          "d\x41\xCF\x7B\x3D\xA0\x00\x00\x00",

    test_binaryFormat nullBytes
            "b\x00\x00\x00\x00",
    test_binaryFormat someBytes
            "b\x00\x00\x00\x04\222\173\190\239",

    test_binaryFormat ([]::[Int])  "[\0\0\0\0]",
    test_binaryFormat [undef]      "[\0\0\0\1!]",
    test_binaryFormat [undef, toLLSD (1::Int)]
            "[\0\0\0\2!i\0\0\0\1]",
    test_binaryFormat (Map.empty :: Map.Map String Int)
            "{\0\0\0\0}",
    test_binaryFormat (Map.fromAscList [("foo", "bar")])
            "{\0\0\0\1k\0\0\0\3foos\0\0\0\3bar}",
    test_binaryFormat (Map.fromAscList [("foo", toLLSD "bar"), ("baz", undef)])
            "{\0\0\0\2k\0\0\0\3foos\0\0\0\3bark\0\0\0\3baz!}"
    ]


--
-- Round Trip Tests
--

prop_roundtrip :: (SData a, Eq a) => (LLSD -> Either String LLSD) -> a -> Bool
prop_roundtrip rt v =
    case rt lv of
        (Left _)    -> False
        (Right lw)  -> v == fromLLSD lw
    where lv = toLLSD v
    -- FIXME: should be testing the structural equality of the LLSDs

prop_mapRoundtrip :: (SData a, Eq a) => (LLSD -> Either String LLSD) -> [(String, a)] -> Bool
prop_mapRoundtrip rt v = prop_roundtrip rt (Map.fromAscList v)

test_roundtrip s rt = s ++ " Roundtrips" ~: TestList [
    --prop_roundtrip rt undef                     ~? "undef",
    prop_roundtrip rt True                      ~? "true",
    prop_roundtrip rt False                     ~? "false",
    prop_roundtrip rt (1::Int)                  ~? "one",
    prop_roundtrip rt (0::Int)                  ~? "zero",
    prop_roundtrip rt (-1::Int)                 ~? "negative one",
    prop_roundtrip rt (1234.5::Double)          ~? "float",
    prop_roundtrip rt (0.0::Double)             ~? "float zero",
    prop_roundtrip rt (-1234.5::Double)         ~? "negative float",
    prop_roundtrip rt ""                        ~? "empty string",
    prop_roundtrip rt "some string"             ~? "some string",
    prop_roundtrip rt nullUUID                  ~? "null UUID",
    prop_roundtrip rt someUUID                  ~? "some UUID",
    prop_roundtrip rt nullDate                  ~? "null date",
    prop_roundtrip rt someDate                  ~? "some date",
    prop_roundtrip rt nullURI                   ~? "null URI",
    prop_roundtrip rt someURI                   ~? "some URI",
    prop_roundtrip rt nullBytes                 ~? "null byte array",
    prop_roundtrip rt someBytes                 ~? "some byte array"
    ]

props_roundtrip s rt = [
    label (s ++ "prop_BoolToFromIdentity")     (prop_roundtrip rt :: Bool -> Bool),
    label (s ++ "prop_IntToFromIdentity")      (prop_roundtrip rt :: Int -> Bool),
    label (s ++ "prop_DoubleToFromIdentity")   (prop_roundtrip rt :: Double -> Bool),
    label (s ++ "prop_StringToFromIdentity")   (prop_roundtrip rt :: String -> Bool),
    label (s ++ "prop_ArrayToFromIdentity")    (prop_roundtrip rt :: [String] -> Bool),
    label (s ++ "prop_MapToFromIdentity")      (prop_mapRoundtrip rt :: [(String, Int)] -> Bool)
    ]

test_xmlRoundtrip  = test_roundtrip  "XML" (parseXML . toByteString . formatXML)
props_xmlRoundtrip = props_roundtrip "XML" (parseXML . toByteString . formatXML)
test_binaryRoundtrip  = test_roundtrip  "Binary" (parseBinary . toByteString . formatBinary)
props_binaryRoundtrip = props_roundtrip "Binary" (parseBinary . toByteString . formatBinary)

toByteString :: L.ByteString -> B.ByteString
toByteString = B.concat . L.toChunks


addExample :: LLSD -> LLSD
addExample origLLSD =
    origLLSD `with` "p1" ./ "name" .= "Bob"
             `with` "p1" ./ "age"  .= (45::Int)
             `with` "p2" ./ "name" .= "Amy"
             `with` "p2" ./ "age"  .= (36::Int)

fetchAge :: String -> LLSD -> Int
fetchAge ident d = d `at` ident ./ "age"

fetchName :: String -> LLSD -> String
fetchName ident d = d `at` ident ./ "name"

setSize :: String -> String -> LLSD -> LLSD
setSize ident s d = d `with` ident ./ "size" .= s

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

setAddress :: String -> Address -> LLSD -> LLSD
setAddress ident add d = d `with` ident ./ "address" .= add

data Part = Part { name:: String, weight:: Double, location:: (Int, Int) }
instance SData Part where
    toLLSD p =
        llsd `with` "name" .= name p
             `with` "weight" .= weight p
             `with` "location" .= [fst $ location p, snd $ location p]
    fromLLSD v =
        Part { name = v `at` "name",
               weight = v `at` "weight",
               location = (v `at` "location" ./ (0::Int),
                           v `at` "location" ./ (1::Int)) }
{-
oneAndTwo :: LLSD -> (Int, Int)
oneAndTwo v = (v `at` "foo" ./ 0, v `at` "foo" ./ 1)
-}



runTests = runTestText (putTextToHandle stderr False) (TestList [
        test_identity,
        test_conversions,
        test_xmlFormats,
        test_xmlRoundtrip,
        test_binaryFormats,
        test_binaryRoundtrip
    ])
runChecks = mapM_ quickCheck $ concat [
        props_identity,
        props_has,
        props_xmlRoundtrip,
        props_binaryRoundtrip
    ]
