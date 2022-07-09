"use strict";

var runTests, assertEquals, assertTrue, assertFalse, assertThrows; // test_framework.js
var LLSD, URI, UUID, Binary; // llsd.js
var LLIDL, LLIDLException; // llidl.js

(function () {

    function good_parse_value(v) {
        assertTrue(LLIDL.parse_value(v));
    }

    function bad_parse_value(v, ss) {
        try {
            LLIDL.parse_value(v);
            assertTrue(false);
        }
        catch (e) {
            if (ss) {
                assertTrue('expected error: ' + ss + ', saw: ' + e.message, e.message.indexOf(ss) !== -1);
            }
            else {
                assertTrue(true);
            }
        }
    }

    runTests('LLIDL Unit Tests', 554, {

        'parser': function() {
            assertTrue("type", LLIDL.parse_value("undef"));
            assertTrue("type", LLIDL.parse_value("string"));
            assertTrue("type", LLIDL.parse_value("bool"));
            assertTrue("type", LLIDL.parse_value("int"));
            assertTrue("type", LLIDL.parse_value("real"));
            assertTrue("type", LLIDL.parse_value("date"));
            assertTrue("type", LLIDL.parse_value("uri"));
            assertTrue("type", LLIDL.parse_value("uuid"));
            assertTrue("type", LLIDL.parse_value("binary"));
            assertThrows("type (empty string)", LLIDLException, LLIDL.parse_value, "");
            assertThrows("type", LLIDLException, LLIDL.parse_value, "bogus");
            assertThrows("prefix", LLIDLException, LLIDL.parse_value, " bogus");
            assertThrows("prefix", LLIDLException, LLIDL.parse_value, "~bogus");

            assertTrue("selector", LLIDL.parse_value("\"foo\""));
            assertTrue("selector", LLIDL.parse_value("123"));
            assertTrue("selector", LLIDL.parse_value("true"));
            assertTrue("selector", LLIDL.parse_value("false"));
            assertThrows("selector", LLIDLException, LLIDL.parse_value, "bogus");

            assertTrue("variant", LLIDL.parse_value("&var"));
            assertThrows("variant", LLIDLException, LLIDL.parse_value, "bogus");

            assertTrue("value", LLIDL.parse_value("undef"));
            assertTrue("value", LLIDL.parse_value("string"));
            assertTrue("value", LLIDL.parse_value("\"foo\""));
            assertTrue("value", LLIDL.parse_value("&var"));

            assertTrue("value", LLIDL.parse_value("[ string ]"));

            assertTrue("value", LLIDL.parse_value("[ string, int ]"));
            assertTrue("value", LLIDL.parse_value("[ string, int ... ]"));
            assertTrue("value", LLIDL.parse_value("[ string, int, ... ]"));
            assertTrue("value", LLIDL.parse_value("{ foo: string }"));
            assertTrue("value", LLIDL.parse_value("{ foo: string }"));
            assertTrue("value", LLIDL.parse_value("{ bar: string, baz: int }"));
            assertTrue("value", LLIDL.parse_value("{ $: string }"));

            assertTrue("value", LLIDL.parse_value("[ { foo: uuid }, { bar: date } ]"));
            assertTrue("value", LLIDL.parse_value("{ key: [ string ... ] }"));

            assertThrows("empty map", LLIDLException, LLIDL.parse_value, "{}");
            assertThrows("empty array", LLIDLException, LLIDL.parse_value, "[]");

            assertTrue("trailing comma", LLIDL.parse_value("{ foo: int, }"));
            assertTrue("trailing comma", LLIDL.parse_value("[ 1, 2, ]"));

            assertTrue("whitespace", LLIDL.parse_value("{ key: \n[ string ... ] }"));
            assertTrue("comment", LLIDL.parse_value("{ key: ;comment\n[ string ... ] }"));
            assertThrows("comment", LLIDLException, LLIDL.parse_value, "{ key: ;comment\u0001[ string ... ] }");
            assertTrue("comment", LLIDL.parse_value("[ ;0123abcd\n string ]"));
            assertTrue("comment", LLIDL.parse_value("[ ;\t \uE000\uFFFD\n string ]"));
            assertTrue("comment", LLIDL.parse_value("[ ;\uD950\uDF21\n string ]"));
            assertThrows("comment", LLIDLException, LLIDL.parse_value, "[ ;\uDF21\uD950\n string ]");

            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo <');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo <<');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo >');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo >>');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo >> bar');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo <>');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo <x>');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo < x >');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo ->');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo <-');
            assertThrows('malformed resource-def', LLIDLException, LLIDL.parse_suite, '%% foo -> <-');

            assertTrue("variant-def", LLIDL.parse_variantdef("&foo=[string...]"));
            assertTrue("suite", LLIDL.parse_suite("%% foo -> uuid <- [ string ... ]"));
            assertTrue("suite", LLIDL.parse_suite("%% query -> uuid <- [ string ... ]"));

            assertTrue("suite", LLIDL.parse_suite("&foo={key:bool} &foo={key:string} %% query -> uuid <- &foo"));
            assertThrows("suite", LLIDLException, LLIDL.parse_suite, "&foo={key:bool} foo={key:string} %% query -> uuid <- &foo");
        },

        'value validation': function() {
            assertTrue(LLIDL.parse_value("undef").valid(null));
            assertTrue(LLIDL.parse_value("string").valid("foo"));
            assertTrue(LLIDL.parse_value("string").valid(undefined));
            assertTrue(LLIDL.parse_value("bool").valid(true));
            assertTrue(LLIDL.parse_value("bool").valid(null));
            assertTrue(LLIDL.parse_value("int").valid(123456));
            assertFalse(LLIDL.parse_value("int").valid(123.456));
            assertTrue(LLIDL.parse_value("real").valid(3.14159));
            assertTrue(LLIDL.parse_value("date").valid("2009-04-14T19:06:07Z"));
            assertTrue(LLIDL.parse_value("date").valid(new Date()));
            assertTrue(LLIDL.parse_value("uri").valid("http://www.example.com"));
            assertTrue(LLIDL.parse_value("uri").valid("http://www.example.com/%7efoo"));
            assertTrue(LLIDL.parse_value("uri").valid("http://www.example.com/%7efoo?query#hash"));
            assertTrue(LLIDL.parse_value("uri").valid("")); // Empty URI is a special case
            assertFalse(LLIDL.parse_value("uri").valid("bogus")); // no scheme
            assertFalse(LLIDL.parse_value("uri").valid("_:foo")); // invalid
            assertFalse(LLIDL.parse_value("uri").valid("tel:")); // no data
            assertFalse(LLIDL.parse_value("uri").valid("foo:%zz")); // invalid %-encoded
            assertFalse(LLIDL.parse_value("uri").valid("foo:%0")); // invalid %-encoded
            assertTrue(LLIDL.parse_value("uuid").valid("ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(LLIDL.parse_value("uuid").valid("xf9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertTrue(LLIDL.parse_value("uuid").valid(""));
            assertTrue(LLIDL.parse_value("binary").valid(new Binary([0, 1, 254, 255])));
            assertTrue(LLIDL.parse_value("binary").valid("AQIDBA=="));
            assertFalse(LLIDL.parse_value("binary").valid("!~@#$%"));
            assertFalse(LLIDL.parse_value("binary").valid(1234));

            var vds = [
                LLIDL.parse_variantdef("&var = string"),
                LLIDL.parse_variantdef("&var = int")
            ];

            assertTrue(LLIDL.parse_value("&var").valid("foo", vds));
            assertTrue(LLIDL.parse_value("&var").valid(123, vds));
            assertTrue(LLIDL.parse_value("&var").valid(true, vds));
            assertFalse(LLIDL.parse_value("&var").valid([1, 2, 3], vds));

            assertTrue(LLIDL.parse_value("\"foo\"").valid("foo"));
            assertTrue(LLIDL.parse_value("123").valid(123));
            assertTrue(LLIDL.parse_value("true").valid(true));
            assertFalse(LLIDL.parse_value("true").valid(123));

            assertTrue(LLIDL.parse_value("[ int, string ... ]").valid([1, "a", 2, "b"]));
            assertTrue(LLIDL.parse_value("[ int, string, ... ]").valid([1, "a", 2, "b"]));
            assertTrue(LLIDL.parse_value("[ int, string ... ]").valid([1, 2, 3]));
            assertFalse(LLIDL.parse_value("[ int, string ... ]").valid([1, 2, []]));
            assertTrue(LLIDL.parse_value("[ int, int, string ]").valid([1, 2, "a"]));
            assertTrue(LLIDL.parse_value("[ int, int, string ]").valid([1, 2, 3]));
            assertFalse(LLIDL.parse_value("[ int, int, string ]").valid([1, 2, []]));
            assertTrue(LLIDL.parse_value("[ int, int ]").has_additional([1, 2, 3]));
            assertTrue(LLIDL.parse_value("[ int, int, int ]").valid([1, 2]));

            assertTrue(LLIDL.parse_value("{ $: int }").valid({ "a": 1, "b": 2 }));
            assertTrue(LLIDL.parse_value("{ foo: int }").valid({ "foo": 1, "b": 2 }));
        },

        'undef': function() {
            var v = LLIDL.parse_value("undef");

            assertTrue(v.match(null));
            assertTrue(v.match(true));
            assertTrue(v.match(false));
            assertTrue(v.match(0));
            assertTrue(v.match(1));
            assertTrue(v.match(3));
            assertTrue(v.match(0.0));
            assertTrue(v.match(1.0));
            assertTrue(v.match(3.14));
            assertTrue(v.match(""));
            assertTrue(v.match("true"));
            assertTrue(v.match("false"));
            assertTrue(v.match(new Date()));
            assertTrue(v.match(new UUID()));
            assertTrue(v.match(new URI()));
            assertTrue(v.match(new Binary()));
        },

        'bool': function() {
            var v = LLIDL.parse_value("bool");
            assertTrue(v.match(null));
            assertTrue(v.match(true));
            assertTrue(v.match(false));
            assertTrue(v.match(0));
            assertTrue(v.match(1));
            assertTrue(v.incompatible(3));
            assertTrue(v.match(0.0));
            assertTrue(v.match(1.0));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.match(""));
            assertTrue(v.match("true"));
            assertTrue(v.incompatible("false"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'int': function() {
            var v = LLIDL.parse_value("int");
            assertTrue(v.match(null));
            assertTrue(v.match(true));
            assertTrue(v.match(false));
            assertTrue(v.match(0));
            assertTrue(v.match(1));
            assertTrue(v.match(0.0));
            assertTrue(v.match(10.0));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.incompatible(6.02e23));
            assertTrue(v.match(""));
            assertTrue(v.match("0"));
            assertTrue(v.match("1"));
            assertTrue(v.match("0.0"));
            assertTrue(v.match("10.0"));
            assertTrue(v.incompatible("3.14"));
            assertTrue(v.incompatible("6.02e23"));
            assertTrue(v.incompatible("blob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'real': function() {
            var v = LLIDL.parse_value("real");
            assertTrue(v.match(null));
            assertTrue(v.match(true));
            assertTrue(v.match(false));
            assertTrue(v.match(0));
            assertTrue(v.match(1));
            assertTrue(v.match(0.0));
            assertTrue(v.match(10.0));
            assertTrue(v.match(3.14));
            assertTrue(v.match(6.02e23));
            assertTrue(v.match(""));
            assertTrue(v.match("0"));
            assertTrue(v.match("1"));
            assertTrue(v.match("0.0"));
            assertTrue(v.match("10.0"));
            assertTrue(v.match("3.14"));
            assertTrue(v.match("6.02e23"));
            assertTrue(v.incompatible("blob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'string': function() {
            var v = LLIDL.parse_value("string");
            assertTrue(v.match(null));
            assertTrue(v.match(true));
            assertTrue(v.match(false));
            assertTrue(v.match(3));
            assertTrue(v.match(3.14));
            assertTrue(v.match(""));
            assertTrue(v.match("bob"));
            assertTrue(v.match(new Date()));
            assertTrue(v.match(new UUID()));
            assertTrue(v.match(new URI()));
            assertTrue(v.match(new Binary())); // Spec changed
        },

        'date': function() {
            var v = LLIDL.parse_value("date");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.match(""));
            assertTrue(v.match("2009-02-06T22:17:38Z"));
            assertTrue(v.match("2009-02-06T22:17:38.0025Z"));
            assertTrue(v.incompatible("bob"));
            assertTrue(v.match(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'uuid': function() {
            var v = LLIDL.parse_value("uuid");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.match(""));
            assertTrue(v.match("6cb93268-5148-423f-8618-eaa0884f5b6c"));
            assertTrue(v.incompatible("bob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.match(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'uri': function() {
            var v = LLIDL.parse_value("uri");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.match(""));
            assertTrue(v.match("http://example.com/"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.match(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'binary': function() {
            var v = LLIDL.parse_value("binary");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.incompatible("!@#$%^"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.match(new Binary()));
        },

        'array': function() {
            var v = LLIDL.parse_value("[ int ]");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.incompatible("bob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
            assertTrue(v.match([]));
        },

        'map': function() {
            var v = LLIDL.parse_value("{ foo: int }");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.incompatible("bob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
            assertTrue(v.match({}));
        },

        'selector - true': function() {
            var v = LLIDL.parse_value("true");
            assertTrue(v.incompatible(null));
            assertTrue(v.match(true));
            assertTrue(v.incompatible(false));
            assertTrue(v.incompatible(0));
            assertTrue(v.match(1));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(0.0));
            assertTrue(v.match(1.0));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.incompatible(""));
            assertTrue(v.match("true"));
            assertTrue(v.incompatible("false"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'selector - false': function() {
            var v = LLIDL.parse_value("false");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.match(false));
            assertTrue(v.match(0));
            assertTrue(v.incompatible(1));
            assertTrue(v.incompatible(3));
            assertTrue(v.match(0.0));
            assertTrue(v.incompatible(1.0));
            assertTrue(v.incompatible(3.14));
            assertTrue(v.match(""));
            assertTrue(v.incompatible("true"));
            assertTrue(v.incompatible("false"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'number 16': function() {
            var v = LLIDL.parse_value("16");
            assertTrue(v.incompatible(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(false));
            assertTrue(v.incompatible(0));
            assertTrue(v.incompatible(3));
            assertTrue(v.match(16));
            assertTrue(v.incompatible(0.0));
            assertTrue(v.match(16.0));
            assertTrue(v.match(16.2));
            assertTrue(v.incompatible(""));
            assertTrue(v.match("16"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'number 0': function() {
            var v = LLIDL.parse_value("0");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.match(false));
            assertTrue(v.match(0));
            assertTrue(v.incompatible(3));
            assertTrue(v.match(0.0));
            assertTrue(v.incompatible(16.0));
            assertTrue(v.match(""));
            assertTrue(v.match("0"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        'string bob': function() {
            var v = LLIDL.parse_value('"bob"');
            assertTrue(v.incompatible(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(false));
            assertTrue(v.incompatible(0));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(0.0));
            assertTrue(v.incompatible(16.0));
            assertTrue(v.incompatible(""));
            assertTrue(v.match("bob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));
        },

        // def x_test_string_empty(self):
        // # currently the draft doesn't support this test
        // var v = LLIDL.parse_value('""');
        // assertTrue(v.match(null));
        // assertTrue(v.incompatible(true));
        // assertTrue(v.incompatible(false));
        // assertTrue(v.incompatible(0));
        // assertTrue(v.incompatible(3));
        // assertTrue(v.incompatible(0.0));
        // assertTrue(v.incompatible(16.0));
        // assertTrue(v.match(""));
        // assertTrue(v.incompatible("bob"));
        // assertTrue(v.incompatible(new Date()));
        // assertTrue(v.incompatible(new UUID()));
        // assertTrue(v.incompatible(new URI()));
        // assertTrue(v.incompatible(new Binary()));
        // },


        'array not_simple': function() {
            var v = LLIDL.parse_value("[ string, uuid ]");
            assertTrue(v.match(null));
            assertTrue(v.incompatible(true));
            assertTrue(v.incompatible(false));
            assertTrue(v.incompatible(3));
            assertTrue(v.incompatible(16.0));
            assertTrue(v.incompatible(""));
            assertTrue(v.incompatible("bob"));
            assertTrue(v.incompatible(new Date()));
            assertTrue(v.incompatible(new UUID()));
            assertTrue(v.incompatible(new URI()));
            assertTrue(v.incompatible(new Binary()));

            v = LLIDL.parse_value('[ "red", uuid ]');
            assertTrue(v.incompatible(null));
        },

        'array sizing': function() {
            var v = LLIDL.parse_value("[ int, int, int ]");
            assertTrue(v.match([]));
            assertTrue(v.match([1]));
            assertTrue(v.match([1, 2]));
            assertTrue(v.match([1, 2, 3]));
            assertTrue(v.has_additional([1, 2, 3, 4]));

            v = LLIDL.parse_value("[ int, 2, int ]");
            assertTrue(v.incompatible([]));
            assertTrue(v.incompatible([1]));
            assertTrue(v.match([1, 2]));
            assertTrue(v.match([1, 2, 3]));
            assertTrue(v.has_additional([1, 2, 3, 4]));
        },

        'array paring': function() {
            var v = LLIDL.parse_value("[ int, int ]");
            assertTrue(v.match([1, 2]));
            assertTrue(v.match([1, '2']));
            assertTrue(v.match([1, null]));
            assertTrue(v.incompatible([1, new Binary()]));

            assertTrue(v.match(['1', 2]));
            assertTrue(v.match(['1', '2']));
            assertTrue(v.match(['1', null]));
            assertTrue(v.incompatible(['1', new Binary()]));

            assertTrue(v.match([null, 2]));
            assertTrue(v.match([null, '2']));
            assertTrue(v.match([null, null]));
            assertTrue(v.incompatible([null, new Binary()]));

            assertTrue(v.incompatible([new Binary(), 2]));
            assertTrue(v.incompatible([new Binary(), '2']));
            assertTrue(v.incompatible([new Binary(), null]));
            assertTrue(v.incompatible([new Binary(), new Binary()]));
        },

        'array repeating': function() {
            var v = LLIDL.parse_value("[ string, int, bool, ... ]");
            assertTrue(v.match([]));
            assertTrue(v.match(['a']));
            assertTrue(v.match(['a', 2]));
            assertTrue(v.match(['a', 2, true]));
            assertTrue(v.match(['a', 2, true, 'b']));
            assertTrue(v.match(['a', 2, true, 'b', 3]));
            assertTrue(v.match(['a', 2, true, 'b', 3, false]));

            assertTrue(v.match(['a', 2, true, 7, 3, false]));
            assertTrue(v.match(['a', 2, true, 'b', 3.0, false]));
            assertTrue(v.match(['a', 2, true, 'b', 3, 0]));

            //assertTrue(v.incompatible(['a',2,true,new Binary(),3,false])); // Spec change
            assertTrue(v.incompatible(['a', 2, true, [], 3, false]));
            assertTrue(v.incompatible(['a', 2, true, 'b', 'c', false]));
            assertTrue(v.incompatible(['a', 2, true, 'b', 3, 'd']));
        },

        'map members': function() {
            var v = LLIDL.parse_value("{ amy: string, bob: int }");
            assertTrue(v.match({}));
            assertTrue(v.match({ 'amy': 'ant' }));
            assertTrue(v.match({ 'bob': 42 }));
            assertTrue(v.match({ 'amy': 'ant', 'bob': 42 }));
            assertTrue(v.has_additional({ 'amy': 'ant', 'bob': 42, 'cam': true }));
        },

        'map dict': function() {
            var v = LLIDL.parse_value("{ $: int }");
            assertTrue(v.match({}));
            assertTrue(v.match({ 'amy': 36 }));
            assertTrue(v.match({ 'amy': 36, 'bob': 42 }));
            assertTrue(v.match({ 'amy': 36, 'bob': 42, 'cam': 18 }));
            assertTrue(v.match({ 'amy': null, 'bob': 42 }));
            assertTrue(v.match({ 'amy': "36", 'bob': 42 }));
            assertTrue(v.incompatible({ 'amy': 'ant', 'bob': 42 }));
            assertTrue(v.match({ 'amy': 36, 'bob': null }));
            assertTrue(v.match({ 'amy': 36, 'bob': '42' }));
            assertTrue(v.incompatible({ 'amy': 36, 'bob': 'bee' }));
        },

        'parse simple': function() {
            // good and bad type names
            ['undef', 'bool', 'int', 'real', 'string', 'uri', 'date', 'uuid', 'binary'].forEach(good_parse_value);
            ['blob', 'Bool', 'BOOL', '#', '*', '_'].forEach(function(s) {
                bad_parse_value(s); // can't call directly as forEach() adds additional arguments
            });

            // good and bad selectors
            ['true', 'false', '0', '1', '42', '1000000', '"red"', '"blue"', '"a/b/c_d"'].forEach(good_parse_value);
            ['3.14159', '-10', '2x2', '0x3f', '""', '"a-b"', '"3"', '"~boo~"', '"feh'].forEach(function(s) {
                bad_parse_value(s); // can't call directly as forEach() adds additional arguments
            });
        },

        'whitespace': function() {
            ['[ real]', '[\treal]', '[  real]', '[\t\treal]'].forEach(good_parse_value);

            ['\n', '\r\n', '\r', '\n\n'].forEach(function(n1) {
                var w = '[' + n1 + '\tint,' + n1 + '\tbool, ;comment and stuff!!' + n1 + '\tstring' + n1 + ']';
                good_parse_value(w);
            });
        },

        'bad whitespace': function() {
            // parse_value() expects the value to be the entire string,
            //   other, larger, parsing constructs consume the surrounding whitespace
            bad_parse_value("");
            bad_parse_value(" ");
            bad_parse_value(" int");
            bad_parse_value("int ");
        },

        'array spacing': function() {
            ['[int,bool]', '[ int,bool]', '[int ,bool]', '[int, bool]', '[int,bool ]'].forEach(good_parse_value);
            ['[int,...]', '[ int,...]', '[int ,...]', '[int, ...]', '[int,... ]'].forEach(good_parse_value);

        },

        'array commas': function() {
            var v = LLIDL.parse_value('[int]');
            assertTrue(v.match([1]));
            assertTrue(v.has_additional([1, 2]));
            v = LLIDL.parse_value('[int,]');
            assertTrue(v.match([1]));
            assertTrue(v.has_additional([1, 2]));
            v = LLIDL.parse_value('[int,int]');
            assertTrue(v.match([1]));
            assertTrue(v.match([1, 2]));
            assertTrue(v.has_additional([1, 2, 3]));
            v = LLIDL.parse_value('[int,int,]');
            assertTrue(v.match([1]));
            assertTrue(v.match([1, 2]));
            assertTrue(v.has_additional([1, 2, 3]));
        },

        'empty array': function() {
            bad_parse_value('[]');
            bad_parse_value('[ ]');
            bad_parse_value('[...]');
            bad_parse_value('[ ... ]');
        },

        'malformed array': function() {
            bad_parse_value('[int');
            bad_parse_value('[ bool int ]');
            bad_parse_value('[ bool, , int]');
            bad_parse_value('[ bool, int, ... string]');
        },

        'map spacing': function() {
            ['{a:int,b:int}', '{ a:int,b:int}', '{a :int,b:int}',
                '{a: int,b:int}', '{a:int ,b:int}', '{a:int, b:int}',
                '{a:int,b :int}', '{a:int,b: int}', '{a:int,b:int }'].forEach(good_parse_value);
            ['{$:int}', '{ $:int}', '{$ :int}', '{$: int}', '{$:int }'].forEach(good_parse_value);
        },

        'map commas': function() {
            var v = LLIDL.parse_value('{a:int}');
            assertTrue(v.match({ 'a': 1 }));
            assertTrue(v.has_additional({ 'a': 1, 'b': 2 }));
            v = LLIDL.parse_value('{a:int,}');
            assertTrue(v.match({ 'a': 1 }));
            assertTrue(v.has_additional({ 'a': 1, 'b': 2 }));
            v = LLIDL.parse_value('{a:int,b:int}');
            assertTrue(v.match({ 'a': 1 }));
            assertTrue(v.match({ 'a': 1, 'b': 2 }));
            assertTrue(v.has_additional({ 'a': 1, 'b': 2, 'c': 3 }));
            v = LLIDL.parse_value('{a:int,b:int,}');
            assertTrue(v.match({ 'a': 1 }));
            assertTrue(v.match({ 'a': 1, 'b': 2 }));
            assertTrue(v.has_additional({ 'a': 1, 'b': 2, 'c': 3 }));
        },

        'empty map': function() {
            bad_parse_value('{}');
            bad_parse_value('{ }');
        },

        'malformed map': function() {
            bad_parse_value('{');
            bad_parse_value('{a}');
            bad_parse_value('{a:}');
            bad_parse_value('{:int}');
            bad_parse_value('{int}');
            bad_parse_value('{a:int,');
            bad_parse_value('{alpha-omega:real}');
            bad_parse_value('{alpha/omega:reel}');
            bad_parse_value('{$}');
            bad_parse_value('{$:}');
        },

        'error messages': function() {
            bad_parse_value('""', 'expected name');
            bad_parse_value('"3"', 'expected name');
            bad_parse_value('"feh', 'expected close quote');
            bad_parse_value('[]', 'empty array');
            bad_parse_value('[int', 'expected close bracket');
            bad_parse_value('[int,,bool]', 'expected close bracket');
            bad_parse_value('{}', 'empty map');
            bad_parse_value('{a int}', 'expected colon');
            bad_parse_value('{a:,b:int}', 'expected value');
        },

        'suite': function() {
            var suite = LLIDL.parse_suite(';test suite\r\n' +
                '%% agent/name\r\n' +
                '-> { agent_id: uuid }\r\n' +
                '<- { first: string, last: string }\r\n' +
                '\r\n' +
                '%% region/hub\r\n' +
                '-> { region_id: uuid }\r\n' +
                '<- { loc: [ real, real, real ] }\r\n' +
                '\r\n' +
                '%% event_record\r\n' +
                '-> { log: string, priority: int }\r\n' +
                '<- undef\r\n' +
                '\r\n' +
                '%% motd\r\n' +
                '-> undef\r\n' +
                '<- { message: string }\r\n');

            assertTrue(suite.match_request('agent/name', { 'agent_id': new UUID() }));
            assertTrue(suite.match_response('agent/name', { 'first': 'Amy', 'last': 'Ant' }));

            assertTrue(suite.match_request('region/hub', { 'region_id': new UUID().toString() }));
            assertTrue(suite.match_response('region/hub', { 'loc': [128, 128, 24] }));

            assertTrue(suite.match_request('event_record', { 'log': 'Beep-Beep-Beep' }));
            assertTrue(suite.match_response('event_record', 12345));

            assertTrue(suite.match_request('motd', "please"));
            assertTrue(suite.valid_response('motd',
                    { 'message': "To infinity, and beyond!",
                        'author': ["Buzz", "Lightyear"]
                    }));
        },

        'variants': function() {

            var suite = LLIDL.parse_suite(';variant suite\r\n' +
                '%% object/info\r\n' +
                '-> undef\r\n' +
                '<- { name: string, pos: [ real, real, real ], geom: &geometry }\r\n' +
                '\r\n' +
                '&geometry = { type: "sphere", radius: real }\r\n' +
                '&geometry = { type: "cube", side: real }\r\n' +
                '&geometry = { type: "prisim", faces: int, twist: real }');

            var p = [128.0, 128.0, 26.0];
            assertTrue(
                suite.match_response('object/info',
                    { 'name': 'ball', 'pos': p, 'geom':
                        { 'type': 'sphere', 'radius': 2.2 }
                    }));
            assertTrue(
                suite.match_response('object/info',
                    { 'name': 'box', 'pos': p, 'geom':
                        { 'type': 'cube', 'side': 2.2 }
                    }));
            assertTrue(
                suite.match_response('object/info',
                    { 'name': 'lith', 'pos': p, 'geom':
                        { 'type': 'prisim', 'faces': 3, 'twist': 2.2 }
                    }));
            assertFalse(
                suite.valid_response('object/info',
                    { 'name': 'blob', 'pos': p, 'geom':
                        { 'type': 'mesh', 'verticies': [1, 2, 3, 4] }
                    }));
        },


        'resource validation': function() {
            assertTrue(LLIDL.parse_suite("%% uuidToString -> uuid <- string")
                .valid_request("uuidToString", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertTrue(LLIDL.parse_suite("%% uuidToString -> uuid <- string")
                .valid_response("uuidToString", "hello world!"));
            assertFalse(LLIDL.parse_suite("%% uuidToString -> uuid <- string")
                .valid_request("uuidToString", ["not-a-uuid"]));

            assertTrue("resource-def", LLIDL.parse_suite("%% stringToIntArray -> string <- [ int ... ]")
                .valid_response("stringToIntArray", [1, 2, 3, 4]));
            assertFalse("resource-def", LLIDL.parse_suite("%% stringToIntArray -> string <- [ int ... ]")
                .valid_response("stringToIntArray", [1, "blah", 3, 4]));
            assertFalse("resource-def", LLIDL.parse_suite("%% stringToIntArray -> string <- [ int ... ]")
                .valid_response("stringToIntArray", [1, [], 3, 4]));

            assertThrows("!definitions", LLIDLException, function() {
                LLIDL.parse_suite("%% uuidToString -> uuid <- string")
                    .valid_request("someOtherMessage", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce");
            });

            assertThrows("!definitions", LLIDLException, function() {
                LLIDL.parse_suite("%% uuidToString -> uuid <- string")
                    .valid_response("someOtherMessage", "hello world!");
            });
        },

        'get': function() {
            var suite = LLIDL.parse_suite("%% getUUID << uuid");
            assertFalse(suite.valid_request("getUUID", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(suite.valid_request("getUUID", ["this is not a UUID"]));
            assertTrue(suite.valid_response("getUUID", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(suite.valid_response("getUUID", ["this is not a UUID"]));
        },

        'getput': function() {
            var suite = LLIDL.parse_suite("%% getUUID <> uuid");
            assertTrue(suite.valid_request("getUUID", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(suite.valid_request("getUUID", ["this is not a UUID"]));
            assertTrue(suite.valid_response("getUUID", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(suite.valid_response("getUUID", ["this is not a UUID"]));
        },

        'getputdel': function() {
            var suite = LLIDL.parse_suite("%% getUUID <x> uuid");
            assertTrue(suite.valid_request("getUUID", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(suite.valid_request("getUUID", ["this is not a UUID"]));
            assertTrue(suite.valid_response("getUUID", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertFalse(suite.valid_response("getUUID", ["this is not a UUID"]));
        },

        'post': function() {
            assertTrue("suite parse", LLIDL.parse_suite("%% uuidToString -> uuid <- string"));

            var suite = LLIDL.parse_suite("%% uuidToString -> uuid <- string");
            assertTrue("suite.valid_request", suite.valid_request("uuidToString", "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce"));
            assertTrue("suite.valid_response", suite.valid_response("uuidToString", "Hello, world"));

            suite = LLIDL.parse_suite("&foo={key:bool} &foo={key:string} %% query -> uuid <- &foo");
            assertTrue("validate variants", suite.valid_response("query", { "key": true }));
            assertTrue("validate variants", suite.valid_response("query", { "key": "string" }));
            assertTrue("validate variants", suite.valid_response("query", { "key": 123 }));
            assertFalse("validate variants", suite.valid_response("query", { "key": [] }));

            suite = LLIDL.parse_suite("&foo=bool &foo=string %% query -> uuid <- { key: &foo }");
            assertTrue("validate maps containing variants", suite.valid_response("query", { "key": true }));
            assertTrue("validate maps containing variants", suite.valid_response("query", { "key": "string" }));
            assertTrue("validate maps containing variants", suite.valid_response("query", { "key": 123 })); // DEFAULTED
            assertFalse("validate maps containing variants", suite.valid_response("query", { "key": [] }));
        }
    });

}());