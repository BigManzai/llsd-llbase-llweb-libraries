{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-binds #-}

-- |
-- Module      :  Network.Format.LLSD.TestIDL
-- Copyright   :  (c) Linden Lab 2009
-- License     :  MIT
-- Maintainer  :  bos@lindenlab.com
-- Stability   :  provisional
-- Portability :  portable

module Network.Format.LLSD.TestIDL (
    runTests,
    runChecks
    )
    where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Time
import qualified Data.UUID as UUID
import           Network.Format.LLSD
import           Network.Format.LLSD.LLIDL
import qualified Network.URI as URI
import           Test.HUnit
import           Test.QuickCheck
import           System.IO (stderr)


nullUUID, someUUID :: UUID.UUID
nullUUID = fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
someUUID = fromJust $ UUID.fromString "14409e2f-5588-4c12-b719-ff33f614e3b7"

nullDate, someDate :: UTCTime
nullDate = UTCTime (fromGregorian 1970 01 01) (secondsToDiffTime        0)
someDate = UTCTime (fromGregorian 2003 06 23) (secondsToDiffTime $ 4*3600)

nullURI, someURI :: URI.URI
nullURI = URI.nullURI
someURI = fromJust $ URI.parseURI
    "http://slurl.com/secondlife/Ambleside/57/104/26"

nullBinary, someBinary :: L.ByteString
nullBinary = L.empty
someBinary = L.pack [222, 173, 190, 239]

ls :: String -> LLSD;   ls = toLLSD
li :: Int -> LLSD;      li = toLLSD
ld :: Double -> LLSD;   ld = toLLSD
lu :: String -> LLSD;   lu = toLLSD . (fromLLSD :: LLSD -> URI.URI) . toLLSD

notMatch :: LLIDL -> LLSD -> Bool
notMatch i l = not $ match i l :: Bool



testIDLCases :: String -> [LLIDL -> Test] -> Test
testIDLCases itext its = itext ~:
    case parseValue itext of
        (Left err) -> TestCase $ assertFailure ("parse error: " ++ err)
        (Right i)  -> TestList $ map ($i) its

should :: (SData a, Show a) => (LLIDL -> LLSD -> Bool) -> a -> LLIDL -> Test
should f a i = show a ~: assertBool "" (f i (toLLSD a))




test_simple :: Test
test_simple = "simple" ~: TestList [
    -- This class aggregates all the parse and match tests for simple types

    testIDLCases "undef" [
        should match undef,
        should match True,
        should match False,
        should match (0 :: Int),
        should match (1 :: Int),
        should match (3 :: Int),
        should match (0.0 :: Double),
        should match (1.0 :: Double),
        should match (3.14 :: Double),
        should match "",
        should match "True",
        should match "False",
        should match someDate,
        should match someUUID,
        should match someURI,
        should match someBinary
        ],

    testIDLCases "bool" [
        should hasDefaulted undef,
        should match        True,
        should match        False,
        should match        (0 :: Int),
        should match        (1 :: Int),
        should incompatible (3 :: Int),
        should match        (0.0 :: Double),
        should match        (1.0 :: Double),
        should incompatible (3.14 :: Double),
        should match        "",
        should match        "true",
        should incompatible "false",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "int" [
        should hasDefaulted undef,
        should match        True,
        should match        False,
        should match        (0 :: Int),
        should match        (1 :: Int),
        should match        (0.0 :: Double),
        should match        (10.0 :: Double),
        should incompatible (3.14 :: Double),
        should incompatible (6.02e23 :: Double),
        should hasDefaulted "",
        should match        "0",
        should match        "1",
        should match        "0.0",
        should match        "10.0",
        should incompatible "3.14",
        should incompatible "6.02e23",
        should incompatible "blob",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "real" [
        should hasDefaulted undef,
        should match        True,
        should match        False,
        should match        (0 :: Int),
        should match        (1 :: Int),
        should match        (0.0 :: Double),
        should match        (10.0 :: Double),
        should match        (3.14 :: Double),
        should match        (6.02e23 :: Double),
        should hasDefaulted "",
        should match        "0",
        should match        "1",
        should match        "0.0",
        should match        "10.0",
        should match        "3.14",
        should match        "6.02e23",
        should incompatible "blob",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "string" [
        should hasDefaulted undef,
        should match        True,
        should match        False,
        should match        (3 :: Int),
        should match        (3.14 :: Double),
        should match        "",
        should match        "bob",
        should match        someDate,
        should match        someUUID,
        should match        someURI,
        should incompatible someBinary
        ],

    testIDLCases "date" [
        should hasDefaulted undef,
        should incompatible True,
        should incompatible (3 :: Int),
        should incompatible (3.14 :: Double),
        should hasDefaulted "",
        should match        "2009-02-06T22:17:38Z",
        should match        "2009-02-06T22:17:38.0025Z",
        should incompatible "bob",
        should match        someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "uuid" [
        should hasDefaulted undef,
        should incompatible True,
        should incompatible (3 :: Int),
        should incompatible (3.14 :: Double),
        should hasDefaulted "",
        should match        "6cb93268-5148-423f-8618-eaa0884f5b6c",
        should incompatible "bob",
        should incompatible someDate,
        should match        someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "uri" [
        should hasDefaulted undef,
        should incompatible True,
        should incompatible (3 :: Int),
        should incompatible (3.14 :: Double),
        should hasDefaulted "",
        should match        "http://example.com/",
        should incompatible someDate,
        should incompatible someUUID,
        should match        someURI,
        should incompatible someBinary
        ],

    testIDLCases "binary" [
        should hasDefaulted undef,
        should incompatible True,
        should incompatible (3 :: Int),
        should incompatible (3.14 :: Double),
        should incompatible "bob",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should match        someBinary
        ]
    ]

prop_IntMatchesDoubleIfIntegral :: Int -> Bool
prop_IntMatchesDoubleIfIntegral v = either (const False) (\i -> match i l) p
    where p = parseValue "int"
          l = toLLSD (fromIntegral v :: Double)

props_simple = [
    label "prop_IntMatchesDoubleIfIntegral" prop_IntMatchesDoubleIfIntegral
    ]

test_selector :: Test
test_selector = "selector" ~: TestList [
    -- This class aggregates all the test cases for atomic type selectors.

    testIDLCases "true" [
        should incompatible undef,
        should match        True,
        should incompatible False,
        should incompatible (0 :: Int),
        should match        (1 :: Int),
        should incompatible (3 :: Int),
        should incompatible (0.0 :: Double),
        should match        (1.0 :: Double),
        should incompatible (3.14 :: Double),
        should incompatible "",
        should match        "true",
        should incompatible "false",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "false" [
        should hasDefaulted undef,
        should incompatible True,
        should match        False,
        should match        (0 :: Int),
        should incompatible (1 :: Int),
        should incompatible (3 :: Int),
        should match        (0.0 :: Double),
        should incompatible (1.0 :: Double),
        should incompatible (3.14 :: Double),
        should match        "",
        should incompatible "true",
        should incompatible "false",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "1983" [
        should incompatible undef,
        should incompatible True,
        should incompatible False,
        should incompatible (0 :: Int),
        should incompatible (3 :: Int),
        should match        (1983 :: Int),
        should incompatible (0.0 :: Double),
        should match        (1983.0 :: Double),
        should incompatible (1983.2 :: Double),
        should incompatible "",
        should match        "1983",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "0" [
        should hasDefaulted undef,
        should incompatible True,
        should match        False,
        should match        (0 :: Int),
        should incompatible (3 :: Int),
        should match        (0.0 :: Double),
        should incompatible (16.0 :: Double),
        should hasDefaulted "",
        should match        "0",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ],

    testIDLCases "\"secondLife\"" [
        should incompatible undef,
        should incompatible True,
        should incompatible False,
        should incompatible (0 :: Int),
        should incompatible (3 :: Int),
        should incompatible (0.0 :: Double),
        should incompatible (16.0 :: Double),
        should incompatible "",
        should match        "secondLife",
        should incompatible "1983",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ]
{-
    testIDLCases "\"\"" [
        -- currently the draft doesn"t support this test
        should match        undef,
        should incompatible True,
        should incompatible False,
        should incompatible (0 :: Int),
        should incompatible (3 :: Int),
        should incompatible (0.0 :: Double),
        should incompatible (16.0 :: Double),
        should match        "",
        should incompatible "bob",
        should incompatible someDate,
        should incompatible someUUID,
        should incompatible someURI,
        should incompatible someBinary
        ]
-}
    ]



test_array :: Test
test_array = "array" ~: TestList [

    testIDLCases "[real, real, real]" [
        -- simple types
        should incompatible (123 :: Int),
        should incompatible (3.1415 :: Double),
        should incompatible someUUID,

        should notMatch     ([] :: [Double]),

        --  1 element array
        should notMatch     [ld 1.23],

        -- 2 elements array
        should notMatch     [ld 1.23, ld 789.0],

        -- 4 elements array ( > 3)
        should notMatch     [ld 1.23, ld 2.3, ld 4.56, ld 5.67],

        -- 3 elements with incorrect value type
        should incompatible [ld 1.23, ld 3.3, ls "string"],

        -- 3 elements with correct value type
        should match        [ld 1.23, ld 3.3, ld 2.3434]
        ],

    testIDLCases "[int, string]" [
        -- simple types
        should incompatible (123 :: Int),
        should incompatible (3.1415 :: Double),
        should incompatible "string",
        should incompatible someUUID,

        -- empty array
        should notMatch     ([] :: [LLSD]),

        should notMatch     [li 123],
        should incompatible [ls "str1"],
        should notMatch     [li 123, ls "str1", ls "str2"],
        should incompatible [ls "str1", li 123, li 345],

        should match        [li 123, ls "str1"],
        should match        [li 123, ld 1.2323],
        should match        [li 123, li 456],
        should incompatible [ld 9.999, ls "str1"]
        ],


    testIDLCases "[uri,...]" [
        -- simple types
        should incompatible (123 :: Int),
        should incompatible (3.1415 :: Double),
        should incompatible "string",
        should incompatible someUUID,

        -- empty array
        should match        ([] :: [LLSD]),

        -- 1 element array
        should match        [lu "www.foo.com"],

        -- 2 elements array
        should match        [lu "www.foo.com", lu "www.bar.com"],

        -- 3 elements array
        should match        [lu "www.foo.com", lu "www.bar.com",
                     lu "www.topcoder.com/tc/review?project_id=30012"],

        -- 1 element
        should incompatible [li 123],

        -- 2 elements
        should incompatible [lu "www.google.com", li 123]
        ],

    testIDLCases "[string, int,...]" [

        -- simple types
        should incompatible (123 :: Int),
        should incompatible (3.1415 :: Double),
        should incompatible "string",
        should incompatible someUUID,

        -- empty array
        should match        ([] :: [LLSD]),

        should notMatch     [ls "str"],
        should notMatch     [li 12345],
        should notMatch     [ls "str", li 123, ls "str2"],
        should incompatible [ls "foo", ls "bar"],

        should match        [ls "foo", li 123],
        should match        [ls "foo", li 123, ls "bar", li 345]
        ],

    testIDLCases "[int, int]" [

        should match        [li 123, li 123],
        should notMatch     [li 123, li 345, li 678],
        should hasAdditional [li 123, li 345, li 678],

        should valid        [li 123, li 123],
        should valid        [li 123, li 345, li 678]
        ]
    ]

test_map :: Test
test_map = "map" ~: TestList [

    let a0 = toLLSD ([] :: [String])
        m0 = toLLSD (M.empty :: M.Map String LLSD)
        m1 = llsd `with` "name" .= "ant"
        m2 = m1 `with` "phone" .= (429873232 :: Int)
        m3 = m2 `with` "cam" .= True
    in testIDLCases "{ name : string, phone: int }" [

        should incompatible (123 :: Int),
        should incompatible (3.1415 :: Double),
        should incompatible "string",
        should incompatible someUUID,

        should incompatible a0,
        should hasDefaulted m0,
        should hasDefaulted m1,
        should hasDefaulted $ llsd `with` "phone" .= (42312334 :: Int),
        should incompatible $ llsd `with` "phone" .= "error",
        should match        m2,

        -- three entries map
        should notMatch     $ m2 `with` "address" .= "xx",
        should hasAdditional m3
        ],


    let a0 = toLLSD ([] :: [String])
        m0 = toLLSD (M.empty :: M.Map String LLSD)
        m1 = llsd `with` "id1" .= (1234 :: Int)
        m2 = m1 `with` "id2" .= (100 :: Int)
        m4 = m2 `with` "id3" .= (101 :: Int) `with` "id4" .= (102 :: Int)
    in testIDLCases "{ $: int }" [

        should incompatible (123 :: Int),
        should incompatible (3.1415 :: Double),
        should incompatible "string",
        should incompatible someUUID,

        should incompatible a0,

        -- empty map
        should match        m0,

        -- one entry map
        should incompatible $ llsd `with` "name" .= "ant",
        should match        $ m1,
        should incompatible $ llsd `with` "phone" .= "error",

        -- two entires map
        should match        $ m2,
        should incompatible $ m1 `with` "phone" .= "error",
        should hasDefaulted $ llsd `with` "id1" .= undef `with` "id2" .= (100 :: Int),

        -- many entires map
        should match        $ m4
        ],


    let m1 = llsd `with` "name" .= "bob"
        m2 = m1 `with` "address" .= "NYC"
    in testIDLCases "{name : string}" [
        should match        m1,
        should notMatch     m2,
        should hasAdditional m2
        ],

    let m1 = llsd `with` "name" .= "bob"
        m2 = m1 `with` "address" .= "NYC"
        m3 = m2 `with` "phone" .= (123 :: Int)
    in testIDLCases "{name : string, phone : int}" [
        should valid        m1,
        should valid        m2,
        should valid        m3
        ]
    ]


shouldParseValue :: String -> Test
shouldParseValue itext = itext ~: assertBool ("should've parsed: " ++ e) p
    where parse = parseValue itext
          e = either id (const "") parse
          p = either (const False) (const True) parse

shouldNotParseValue :: String -> Test
shouldNotParseValue itext = itext ~: assertBool "shouldn't've parsed" np
    where parse = parseValue itext
          np = either (const True) (const False) parse


test_parseValue :: Test
test_parseValue = "parse value" ~: TestList [
        shouldParseValue "undef",
        shouldParseValue "bool",
        shouldParseValue "int",
        shouldParseValue "real",
        shouldParseValue "string",
        shouldParseValue "uuid",
        shouldParseValue "date",
        shouldParseValue "uri",
        shouldParseValue "binary",
        
        shouldNotParseValue "blob",
        shouldNotParseValue "Bool",
        shouldNotParseValue "BOOL",
        shouldNotParseValue "--",
        shouldNotParseValue "*",
        shouldNotParseValue "_",

        shouldParseValue "true",
        shouldParseValue "false",
        shouldParseValue "0",
        shouldParseValue "1",
        shouldParseValue "42",
        shouldParseValue "1000000",
        shouldParseValue "\"red\"",
        shouldParseValue "\"blue\"",
        shouldParseValue "\"a/b/c_d\"",
        
        shouldNotParseValue "3.14159",
        shouldNotParseValue "-10",
        shouldNotParseValue "2x2",
        shouldNotParseValue "0x3f",
        shouldNotParseValue "\"\"",
        shouldNotParseValue "\"a-b\"",
        shouldNotParseValue "\"3\"",
        shouldNotParseValue "\"~boo~\"",
        shouldNotParseValue "\"feh",
{-        
        for w in ["[ real]", "[\treal]", "[  real]", "[\t\treal]"]:
            self.good_parse_value(w)
        for nl in ["\n", "\r\n", "\r", "\n\n"]:
            w = ("[%s\tint,%s\tbool, ;comment and stuff!!%s\tstring%s]"
                    % (nl, nl, nl, nl,
            self.good_parse_value(w)
-}

        shouldNotParseValue "",
        shouldNotParseValue " ",
        shouldParseValue " int",
        shouldParseValue "int ",
            -- NOTE: Differs from Python implementation.
            -- The Haskell implementation allows leading and trailing whitespace

        shouldParseValue "[int,bool]",
        shouldParseValue "[ int,bool]",
        shouldParseValue "[int ,bool]",
        shouldParseValue "[int, bool]",
        shouldParseValue "[int,bool ]",
        
        shouldParseValue "[int,...]",
        shouldParseValue "[ int,...]",
        shouldParseValue "[int ,...]",
        shouldParseValue "[int, ...]",
        shouldParseValue "[int,... ]",
        
        shouldParseValue "[string, int, bool, real...]",
        shouldParseValue "[int, date, uuid]",
        shouldParseValue "{name1 : string, name2 : real, name3 : date, name4 : uuid}",
        shouldParseValue "{$ : int}",

        shouldNotParseValue "foo",
        shouldNotParseValue "bar",
        shouldNotParseValue "[]",
        shouldNotParseValue "{}",
        shouldNotParseValue "[int, real, date",
        shouldNotParseValue "{name : string, phone : int",
        shouldNotParseValue "{name:string, name:int}",
        shouldNotParseValue "{name:string,phone:}",
        shouldNotParseValue "{name:string, :int}",
        shouldNotParseValue "{name:string phone,int}",
        shouldNotParseValue "[int, real, invalid]",
        shouldNotParseValue "{name: string, id:invalid}",
        shouldNotParseValue "[...]",
        shouldNotParseValue "{alphone-omega:real}",
        shouldNotParseValue "{$}",
        shouldNotParseValue "{$:}"
    ]





testSuiteCases :: String -> [Suite -> Test] -> Test
testSuiteCases itext its = (take 20 itext ++ "...") ~:
    case parseSuite itext of
        (Left err) -> TestCase $ assertFailure ("parse error: " ++ err)
        (Right i)  -> TestList $ map ($i) its

suiteShould :: (SData a, Show a) =>
                (LLIDL -> LLSD -> Bool) ->
                (String -> Suite -> LLIDL) -> String ->
                a -> Suite -> Test
suiteShould f r t a s =
    (t ++ ": " ++ show a) ~: assertBool "" (f (r t s) (toLLSD a))

test_parseSuite :: Test
test_parseSuite = "parse suite" ~: TestList [
        testSuiteCases "\
            \;test suite\n\
            \%% agent/name\n\
            \-> { agent_id: uuid }\n\
            \<- { first: string, last: string }\n\
            \\n\
            \%% region/hub\n\
            \-> { region_id: uuid }\n\
            \<- { loc: [ real, real, real ] }\n\
            \\n\
            \%% event_record\n\
            \-> { log: string, priority: int }\n\
            \<- undef\n\
            \\n\
            \%% motd\n\
            \-> undef\n\
            \<- { message: string }\n"
            [
                suiteShould match request "agent/name" 
                    $ llsd `with` "agent_id" .= someUUID,
                suiteShould match response "agent/name"
                    $ llsd `with` "first" .= "Amy"
                           `with` "last" .= "Ant",

                suiteShould match request "region/hub" 
                    $ llsd `with` "region_id" .= (UUID.toString someUUID),
                suiteShould match response "region/hub"
                    $ llsd `with` "loc" .= ([128,128,24]::[Int]),

                suiteShould valid request "event_record" 
                    $ llsd `with` "log" .= "Beep-Bepp-Beep",
                suiteShould match response "event_record" (12345::Int),

                suiteShould match request "motd" "please",
                suiteShould valid response "motd"
                    $ llsd `with` "message" .= "To infinity, and beyond!"
                           `with` "author" .= ["Buzz", "Lightyear"]
            ],
        
        let p = ([ 128.0, 128.0, 26.0 ] :: [Double])
        in testSuiteCases "\
            \;variant suite\n\
            \%% object/info\n\
            \-> undef\n\
            \<- { name: string, pos: [ real, real, real ], geom: &geometry }\n\
            \\n\
            \&geometry = { type: \"sphere\", radius: real }\n\
            \&geometry = { type: \"cube\", side: real }\n\
            \&geometry = { type: \"prisim\", faces: int, twist: real }\n"
            [
                suiteShould match response "object/info"
                    $ llsd `with` "name" .= "ball"
                           `with` "pos" .= p
                           `with` "geom" ./ "type" .= "sphere"
                           `with` "geom" ./ "radius" .= (2.2::Double),

                suiteShould match response "object/info"
                    $ llsd `with` "name" .= "box"
                           `with` "pos" .= p
                           `with` "geom" ./ "type" .= "cube"
                           `with` "geom" ./ "side" .= (2.2::Double),
                           
                suiteShould match response "object/info"
                    $ llsd `with` "name" .= "lith"
                           `with` "pos" .= p
                           `with` "geom" ./ "type" .= "prisim"
                           `with` "geom" ./ "faces" .= (3::Int)
                           `with` "geom" ./ "twist" .= (2.2::Double),
                           
                suiteShould incompatible response "object/info"
                    $ llsd `with` "name" .= "blob"
                           `with` "pos" .= p
                           `with` "geom" ./ "type" .= "mesh"
                           `with` "geom" ./ "verticies" .= ([1,2,3,4]::[Int])               
            ],
        
        let iv = 42::Int
            sv = "Amy"
            idl = [$llidlSuite|
                %% api/get
                << int
                
                %% api/getput
                <> int
                
                %% api/getputdel
                <x> int
            |]
        in "api/get..." ~: TestList $ map ($idl)
            [
                suiteShould incompatible request  "api/get" iv,
                suiteShould incompatible request  "api/get" sv,
                suiteShould match        response "api/get" iv,
                suiteShould incompatible response "api/get" sv,

                suiteShould match        request  "api/getput" iv,
                suiteShould incompatible request  "api/getput" sv,
                suiteShould match        response "api/getput" iv,
                suiteShould incompatible response "api/getput" sv,

                suiteShould match        request  "api/getputdel" iv,
                suiteShould incompatible request  "api/getputdel" sv,
                suiteShould match        response "api/getputdel" iv,
                suiteShould incompatible response "api/getputdel" sv
            ]
    ]


quotedValue :: LLIDL
quotedValue = [$llidlValue|{ name : string, phone: int }|]
    
quotedSuite :: Suite
quotedSuite = [$llidlSuite|
;test suite
%% agent/name
-> { agent_id: uuid }
<- { first: string, last: string }

%% region/hub
-> { region_id: uuid }
<- { loc: [ real, real, real ] }

%% event_record
-> { log: string, priority: int }
<- undef

%% motd
-> undef
<- { message: string }
    |]
    
test_quote :: Test
test_quote = "quote" ~: TestList [
    should incompatible "string" quotedValue,
    should match (llsd `with` "name" .= "ant"
                       `with` "phone" .= (429873232 :: Int)) quotedValue,
    suiteShould match response "agent/name"
        (llsd `with` "first" .= "Amy" `with` "last" .= "Ant")
        quotedSuite,
    suiteShould valid request "event_record" 
        (llsd `with` "log" .= "Beep-Bepp-Beep")
        quotedSuite
    ]
 
    
    
runTests = runTestText (putTextToHandle stderr False) (TestList [
        test_simple,
        test_selector,
        test_array,
        test_map,
        test_parseValue,
        test_parseSuite,
        test_quote
    ])

runChecks = mapM_ quickCheck $ concat [
        props_simple
    ]
