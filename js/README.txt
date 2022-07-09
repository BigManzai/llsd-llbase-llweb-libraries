
= LLSD/LLIDL Implementations for ECMAScript (JavaScript) =

== Files ==

* es5.js - fallback implementation of ECMAScript 5 methods for ECMAScript 3
** Most of the implementations are from the Mozilla Development Center or other
   public documentation sources, with appropriate type checking added.
** Required for llsd.js, llidl.js

* json2.js - from http://json.org - implementation of JSON api from ES5 for ES3
** Required for llsd.js if JSON serialization is used

* typedarray.js - implementation of DRAFT spec for typed arrays
** https://cvs.khronos.org/svn/repos/registry/trunk/public/webgl/doc/spec/TypedArray-spec.html
** If running under ECMAScript 5, indexed getter/setter notation (typed_array[index]) can be
   used; otherwise, use typed_array.get(index)/typed_array.set(index, value)
** Required for llsd.js if Binary serialization is used

* llsd.js - implementation of LLSD
** Requires es5.js
** Requires json2.js if JSON serialization is used
** Requires typedarray.js if Binary serialization is used

* llidl.js - implementation of LLIDL
** Requires es5.js
** Requires llsd.js

== API ==

=== Types ===

LLSD Type          ECMAScript Type
------------------ ---------------
Undefined          null
Boolean            Boolean
Integer            Number
Real               Number
String             String
Date               Date
URI                URI (see below)
UUID               UUID (see below)
Binary             Binary (see below)
Array              Array
Map                Object

=== URI ===

var u = new URI("http://www.example.com");
u.toString() // -> "http://www.example.com"
u.toJSON()   // -> "http://www.example.com"


=== UUID ===

var u = new UUID(); // 00000000-0000-0000-0000-000000000000
var u = new UUID( array-of-octets );
var u = new UUID("12345678-1234-1234-1234-123456789abc");
u.toString() // UUID string
u.toJSON()   // UUID string
u.getOctets() // [ 0x00, 0x01, 0x02 ... 0x0f ]


=== Binary ===

var b = new Binary(); // length 0
var b = new Binary( octets ); // Array of numbers
var b = new Binary( binary ); // Clone constructor
var b = new Binary( string, encoding );

b.toString() // string line "[Binary <length>]"
b.toString( encoding ) // encoding of octets
b.toJSON()   // base64 encoding of octets
b.toArray() // Array of octets (a copy)

Supported encodings are "UTF-8", "BASE64", "BASE16", "BINARY"
Unsupported encodings or invalid data will throw
a RangeError.


=== LLSD API ===

LLSD.MIMETYPE_XML    // "application/llsd+xml"
LLSD.MIMETYPE_JSON   // "application/llsd+json"
LLSD.MIMETYPE_BINARY // "application/llsd+binary"

LLSD.parse( content_type, string )
LLSD.parseBinary( array-of-octets )
LLSD.parseXML( string )
LLSD.parseJSON( string )

LLSD.format( content_type, data )
LLSD.formatBinary( data )
LLSD.formatXML( data )
LLSD.formatJSON( data )

LLSD.asUndefined( value )
LLSD.asBoolean( value )
LLSD.asInteger( value )
LLSD.asReal( value )
LLSD.asString( value )
LLSD.asUUID( value )
LLSD.asDate( value )
LLSD.asURI( value )
LLSD.asBinary( value )

LLSD.parseISODate(str) // returns date or throws if invalid
LLSD.MAX_INTEGER // maximum 32-bit two's complement value
LLSD.MIN_INTEGER // minimum 32-bit two's complement value
LLSD.isNegativeZero(n) // true if n is negative zero
LLSD.isInt32(n) // true if n can be represented as an LLSD integer
LLSD.type(v) // one of 'undefined', 'string', 'boolean', 'integer',
                       'real', 'date', 'uri', 'uuid', 'binary',
                       'array', 'map'
LLSD.parseFloat(str) // following Appendix A of spec
LLSD.formatFloat(val) // following Appendix A of spec

Notation formatting (a pre-JSON, Linden-specific format) is
supported if a script sets LL_LEGACY on the global object
before llsd.js is included:

LLSD.parseNotation( string )
LLSD.formatNotation( data )

=== LLIDL API ===

var suite = LLIDL.parse_suite(definitions); // Throws if invalid
suite.match_request(name, value); // boolean - request matches exactly
suite.match_response(name, value); // boolean - response matches exactly
suite.valid_request(name, value); // boolean - request matches or has additional data
suite.valid_response(name, value); // boolean - response matches or has additional data

var value = LLIDL.parse_value(spec); // Throws if invalid
value.match(actual); // boolean - matches exactly
value.valid(actual); // boolean - matches or has additional data
value.has_additional(actual); // boolean - matches and has additional data
value.incompatible(actual); // boolean - incompatible


== Tests ==

Unit tests are present for:

* LLSD: tests/llsd_tests.js
* LLIDL: tests/llidl_tests.js
* Typed Arrays: tests/typedarray_tests.js
* ES5 Shims: tests/es5_tests.js

To run them, load the corresponding HTML file in a Web browser:

* LLSD: tests/llsd.htm
* LLIDL: tests/llidl.htm
* Typed Arrays: tests/typedarray.htm
* ES5 Shims: tests/es5.htm

These use a custom test framework (tests/test_framework.js) roughly modeled on
JSUnit (http://www.jsunit.net/).


== Code ==

The code is:

* Global namespace clean
** Only specific types are exported into the global namespace, and only
   if they are not previously defined

* Strict mode compliant
** The files define "use strict"; at the top which mandates a stricter subset
   of ECMAScript when run in ECMAScript 5-compliant browsers.
** This is harmless in older browsers.


== Notes ==

* "Notation" is a deprecated LLSD serialization format. Implementations are
  provided for compatibility with older services. Documentation can be found 
  at: http://wiki.secondlife.com/wiki/LLSD#Notation_Serialization
