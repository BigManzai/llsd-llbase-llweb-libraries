while (<>) {
    s/^ *def .*//;
    s/    v = llidl.parse_value\(['"](.*)['"]\)/testIDLCases "$1" [/;
    s/self\.assert_\(llidl\.parse_value\(['"](.*)['"]\) != None\)/shouldParseValue "$1",/;
    s/self\.assertRaises\(ParseError, llidl\.parse_value, ['"](.*)['"]\)/shouldNotParseValue "$1",/;
    s/self.assert_\(v./should /;
    s/self.assert_\(not v./should not /;
    s/# /-- /;
    s/\((\d+)\)/(($1 :: Int))/;
    s/\((\d+.\d+)\)/(($1 :: Double))/;
    s/'([^']*)'/"$1"/g;
    s/ {(.*)}/ \$ llsd $1/;
    s/("\w+")\s*:\s*([^],]+)/`with` $1 .= $2/;
    s/None/undef/;
    s/_dateToday\(\)/someDate/;
    s/_uuid\(\)/someUUID/;
    s/_uri\(\)/someURI/;
    s/_binary\(\)/someBinary/;
    s/not match\(/notMatch    /;
    s/match\(/match        /;
    s/incompatible\(/incompatible /;
    s/has_defaulted\(/hasDefaulted /;
    s/has_additional\(/hasAdditional /;
    s/valid\(/valid        /;
    s/\)\)\s*$/,\n/;
    print;
}
