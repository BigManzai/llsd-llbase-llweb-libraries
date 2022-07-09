"use strict";

var runTests, assertEquals, assertTrue, assertFalse, assertThrows; // test_framework.js
var LLSD, URI, UUID, Binary; // llsd.js

(function () {

    var not_defined;

    function bad_parse(msg, type, func, arg) {
        try {
            func(arg);
        }
        catch (e) {
            assertTrue('expected exception: ' + msg + ', saw: ' + e.message,
                e instanceof type && e.message === msg);
            return;
        }
        assertTrue(false, 'expected exception: ' + msg);
    }


    runTests('LLSD Unit Tests', 355, {

        'base64 encoding': function () {
            function atob(a) { return new Binary(a, "BASE64").toArray(); }
            function btoa(b) { return new Binary(b).toString("BASE64"); }

            assertEquals("Base64 Decode", atob("AQIDBA=="), [1, 2, 3, 4]);
            assertEquals("Base64 Decode", atob("A Q I\n D B A = ="), [1, 2, 3, 4]);
            assertEquals("Base64 Encode", btoa([3, 1, 4, 1, 5, 9, 2, 6]), "AwEEAQUJAgY=");

            var data = [0, 1, 2, 253, 254, 255];
            assertEquals("Base64 Encode/Decode", atob(btoa(data)), data);

            // *TODO: Is this desirable?
            //assertEquals("padding optional", atob("AQIDBA=="), [1, 2, 3, 4]);
            //assertEquals("padding optional", atob("AQIDBA="), [1, 2, 3, 4]);
            //assertEquals("padding optional", atob("AQIDBA"), [1, 2, 3, 4]);
            assertThrows("truncated", RangeError, function () { return atob("AQIDB"); });

            assertThrows("invalid base64", RangeError, function (x) { return atob("!@#$%^&"); });
        },

        'base16 encoding': function () {
            assertEquals("Base16 Encode", new Binary("0123456789ABCDEF", "BASE16").toArray(), [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]);
            assertEquals("Base16 Decode", new Binary([0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]).toString('BASE16'), "0123456789ABCDEF");

            assertThrows("invalid base16", RangeError, function (x) { return new Binary(x, "BASE16"); }, "!@#$%^&");
        },

        'binary encoding': function () {
            assertEquals("Binary Encode", new Binary("\x01\x23\x45\x67\x89\xAB\xCD\xEF", "BINARY").toArray(), [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]);
            assertEquals("Binary Decode", new Binary([0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]).toString('BINARY'), "\x01\x23\x45\x67\x89\xAB\xCD\xEF");

            assertThrows("invalid binary", RangeError, function (x) { return new Binary(x, "BINARY"); }, "\x00\u0100\xff");
        },

        'invalid encoding': function () {
            assertThrows("invalid encoding", RangeError, function () { return new Binary("foo", "bar"); });
            assertThrows("invalid encoding", RangeError, function () { return new Binary([1, 2, 3]).toString('foo'); });
        },

        'UUID': function () {
            var sample = 'ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf';
            assertTrue(new UUID());
            assertTrue(new UUID(sample));
            assertEquals(sample, new UUID(sample).toString());
            assertEquals(sample, new UUID([0xff, 0x9a, 0x71, 0xeb, 0x74, 0x14, 0x4b, 0xf8, 0x86, 0x6e, 0xa7, 0x0d, 0xde, 0xb7, 0xc3, 0xcf]).toString());
            assertEquals(sample, new UUID(new UUID(sample)).toString());
            assertThrows('Invalid UUID', Error, function () { return new UUID([1, 2, 3]); });
            assertThrows('Invalid UUID', Error, function () { return new UUID('this is not a UUID'); });
            assertThrows('Invalid UUID', Error, function () { return new UUID(/regex/); });
        },

        'URI': function () {
            assertThrows('Invalid URI', Error, function () { return new URI('this is not a URI'); });
            assertThrows('Invalid URI', Error, function () { return new URI('a b'); });
            assertThrows('Invalid URI', Error, function () { return new URI('foo:'); });

            assertTrue(new URI('foo:bar'));
            assertTrue(new URI('http://www.example.com'));
            assertTrue(new URI('mailto:nobody@example.com'));
            assertEquals(new URI(new URI('foo:bar')).toString(), 'foo:bar');
        },

        'Binary': function () {
            assertThrows('Invalid type', TypeError, function () { return new Binary(/regex/); });
            assertThrows('Non binstring', RangeError, function (x) { return new Binary(x, 'BINARY'); }, '\u0000\u00ff\u0100');
            assertTrue(new Binary('AQIDBA==', 'BASE64'));
            assertTrue(new Binary(new Binary('AQIDBA==', 'BASE64')).toString('BASE64'), 'AQIDBA==');
        },

        'round tripping': function () {
            var data = [
                null,
                123,
                -456,
                "abc",
                true,
                false,
                3.1415,
                6.0221415e+23,
                6.6260693e-34,
                6.67428e-11,
                299792458,
                0,
                [],
                [1],
                [1, 2, 3],
                [1, "abc", 2, "def"],
                {},
                { "abc": 1 },
                { "abc": 1, "def": "ghi" }
            ];

            while (data.length) {
                var datum = data.shift();
                assertEquals("XML: " + datum, datum, LLSD.parseXML(LLSD.formatXML(datum)));
                assertEquals("JSON: " + datum, datum, LLSD.parseJSON(LLSD.formatJSON(datum)));
                assertEquals("Binary: " + datum, datum, LLSD.parseBinary(LLSD.formatBinary(datum)));
                assertEquals("Notation: " + datum, datum, LLSD.parseNotation(LLSD.formatNotation(datum)));
            }
        },

        'non-finites': function () {

            var data = [
                Infinity,
                -Infinity,
                NaN
            ];

            while (data.length) {
                var datum = data.shift();
                assertEquals("XML: " + datum.toString(), datum, LLSD.asReal(LLSD.parseXML(LLSD.formatXML(datum))));
                assertEquals("JSON: " + datum.toString(), datum, LLSD.asReal(LLSD.parseJSON(LLSD.formatJSON(datum))));
                assertEquals("Binary: " + datum.toString(), datum, LLSD.asReal(LLSD.parseBinary(LLSD.formatBinary(datum))));
                assertEquals("Notation: " + datum.toString(), datum, LLSD.parseNotation(LLSD.formatNotation(datum)));
            }
        },

        'negative zero': function () {

            var data = [
                0,
                -0,
                -Math.pow(2, -1074)
            ];

            assertTrue(LLSD.isNegativeZero(-0));
            assertFalse(LLSD.isNegativeZero(0));
            assertFalse(LLSD.isNegativeZero(-Math.pow(2, -1074)));

            while (data.length) {
                var datum = data.shift();
                assertEquals("XML: " + datum.toString(), datum, LLSD.asReal(LLSD.parseXML(LLSD.formatXML(datum))));
                assertEquals("JSON: " + datum.toString(), datum, LLSD.asReal(LLSD.parseJSON(LLSD.formatJSON(datum))));
                assertEquals("Binary: " + datum.toString(), datum, LLSD.asReal(LLSD.parseBinary(LLSD.formatBinary(datum))));
                // Unspecified
                //assertEquals("Notation: " + datum.toString(), datum, LLSD.parseNotation(LLSD.formatNotation(datum)));
            }
        },

        'non-JSON round tripping': function () {
            // Non-JSON tests (some types collapse to strings)
            var datum, data = [
                new URI('urn:foo:bar'),
                new URI('http://www.example.com'),
                new Date(1234567890000),
                new UUID('01020304-aabb-ccdd-eeff-0123456789ab'),
                new Binary([0, 1, 2, 253, 254, 255])
            ];

            while (data.length) {
                datum = data.shift();
                assertEquals("XML format/parse", datum, LLSD.parseXML(LLSD.formatXML(datum)));
                assertEquals("Binary format/parse", datum, LLSD.parseBinary(LLSD.formatBinary(datum)));
                assertEquals("Notation format/parse: " + datum.toString(), datum, LLSD.parseNotation(LLSD.formatNotation(datum)));
            }
        },

        'JSON round-tripping': function () {
            var orig = [
                new URI('urn:foo:bar'),
                new URI('http://www.example.com'),
                new Date(1234567890000),
                new UUID('01020304-aabb-ccdd-eeff-0123456789ab'),
                new Binary([0, 1, 2, 253, 254, 255])
            ];
            var expected = [
                'urn:foo:bar',
                'http://www.example.com',
                /^2009-02-13T23:31:30(\.000)?Z$/, // serializing milliseconds is optional
                '01020304-aabb-ccdd-eeff-0123456789ab',
                'AAEC/f7/'
            ];

            for (var i = 0; i < orig.length; i += 1) {
                var o = orig[i], e = expected[i];

                if (typeof e === 'string') {
                    assertEquals("JSON format/parse", e, LLSD.parseJSON(LLSD.formatJSON(o)));
                }
                else if (typeof e === 'object' && e instanceof RegExp) {
                    assertTrue("JSON format/parse", e.test(LLSD.parseJSON(LLSD.formatJSON(o))));
                }
            }
        },

        'XML parsing': function () {
            assertThrows('require <llsd>', Error, LLSD.parseXML, '<foo><llsd><integer>1</integer></llsd></foo>');
            assertThrows('require one child of <llsd>', Error, LLSD.parseXML, '<llsd><integer>1</integer><integer>2</integer></llsd>');
            assertThrows('single child node of string', Error, LLSD.parseXML, '<llsd><string>123<foo/></string></llsd>');
            assertThrows('text child node of string', Error, LLSD.parseXML, '<llsd><string><foo/></string></llsd>');
            assertThrows('unsupported binary encoding', Error, LLSD.parseXML, '<llsd><binary encoding="base32">MZXW6YTBOI======</binary></llsd>');
            assertThrows('require key as child of map', Error, LLSD.parseXML, '<llsd><map><string>bogus</string></map></llsd>');
            assertThrows('require key sibling in map', Error, LLSD.parseXML, '<llsd><map><key>foo</key></map></llsd>');
            assertThrows('unsupported element', Error, LLSD.parseXML, '<llsd><foo>!</foo></llsd>');
        },

        'XML parsing of invalid values': function () {
            assertEquals('invalid uuid', LLSD.parseXML('<llsd><uuid>this is not a uuid</uuid></llsd>'), new UUID());
            assertEquals('invalid date', LLSD.parseXML('<llsd><date>this is not a date</date></llsd>'), new Date(0));
            assertEquals('invalid binary', LLSD.parseXML('<llsd><binary>this is not valid binary!@#$!@#$</binary></llsd>'), new Binary());
        },

        'XML serializing': function () {

            assertEquals('escaping html', LLSD.parseXML(LLSD.formatXML('<foo> this & that </bar>')), '<foo> this & that </bar>');
        },

        'Binary parsing': function () {

            bad_parse('Unexpected end of data', Error, LLSD.parseBinary, [91, 0, 0, 0, 1, 105, 0, 0, 0, 1]);
            bad_parse('Expected array close tag', Error, LLSD.parseBinary, [91, 0, 0, 0, 1, 105, 0, 0, 0, 1, 66]);
            bad_parse('Expected map key tag', Error, LLSD.parseBinary, [123, 0, 0, 0, 1, 0, 0, 0, 1, 97, 105, 0, 0, 0, 1, 125]);
            bad_parse('Unexpected end of data', Error, LLSD.parseBinary, [123, 0, 0, 0, 1, 107, 0, 0, 0, 1, 97, 105, 0, 0, 0, 1]);
            bad_parse('Expected map close tag', Error, LLSD.parseBinary, [123, 0, 0, 0, 1, 107, 0, 0, 0, 1, 97, 105, 0, 0, 0, 1, 66]);
            bad_parse('Unexpected end of data', Error, LLSD.parseBinary, [115, 0, 0, 0, 3, 97, 98]);
            bad_parse('Unexpected continuation of binary data', Error, LLSD.parseBinary, [91, 0, 0, 0, 1, 105, 0, 0, 0, 1, 93, 66]);
            bad_parse('Unexpected end of data', Error, LLSD.parseBinary, []);
            bad_parse('Unexpected tag', Error, LLSD.parseBinary, [66]);

            assertEquals('spec binary sample', LLSD.parseBinary([
                0x5B, 0x00, 0x00, 0x00, 0x03, 0x69, 0x00, 0x00, 0x00, 0x2A, 0x75, 0x6B,
                0xAD, 0x25, 0x8E, 0x06, 0xF0, 0x4A, 0x87, 0xA6, 0x59, 0x49, 0x31, 0x17,
                0xC9, 0xC1, 0x62, 0x7B, 0x00, 0x00, 0x00, 0x04, 0x6B, 0x00, 0x00, 0x00,
                0x03, 0x68, 0x6F, 0x74, 0x73, 0x00, 0x00, 0x00, 0x04, 0x63, 0x6F, 0x6C,
                0x64, 0x6B, 0x00, 0x00, 0x00, 0x15, 0x68, 0x69, 0x67, 0x67, 0x73, 0x5F,
                0x62, 0x6F, 0x73, 0x6F, 0x6E, 0x5F, 0x72, 0x65, 0x73, 0x74, 0x5f, 0x6d,
                0x61, 0x73, 0x73, 0x21, 0x6B, 0x00, 0x00, 0x00, 0x09, 0x69, 0x6E, 0x66,
                0x6F, 0x5F, 0x70, 0x61, 0x67, 0x65, 0x6C, 0x00, 0x00, 0x00, 0x3A, 0x68,
                0x74, 0x74, 0x70, 0x73, 0x3A, 0x2f, 0x2F, 0x65, 0x78, 0x61, 0x6D, 0x70,
                0x6C, 0x65, 0x2E, 0x6F, 0x72, 0x67, 0x2F, 0x72, 0x2F, 0x36, 0x62, 0x61,
                0x64, 0x32, 0x35, 0x38, 0x65, 0x2D, 0x30, 0x36, 0x66, 0x30, 0x2D, 0x34,
                0x61, 0x38, 0x37, 0x2D, 0x61, 0x36, 0x35, 0x39, 0x2D, 0x34, 0x39, 0x33,
                0x31, 0x31, 0x37, 0x63, 0x39, 0x63, 0x31, 0x36, 0x32, 0x6B, 0x00, 0x00,
                0x00, 0x14, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x5F, 0x72, 0x65, 0x70,
                0x6F, 0x72, 0x74, 0x5F, 0x64, 0x75, 0x65, 0x5F, 0x62, 0x79, 0x64, 0x41,
                0xD2, 0x3C, 0xE6, 0xAC, 0x00, 0x00, 0x00, 0x7D, 0x5D]),
                [
                    42,
                    new UUID('6bad258e-06f0-4a87-a659-493117c9c162'),
                    {
                        'higgs_boson_rest_mass': null,
                        'hot': 'cold',
                        'info_page': new URI('https://example.org/r/6bad258e-06f0-4a87-a659-493117c9c162'),
                        'status_report_due_by': new Date('Mon Oct 13 2008 12:00:00 GMT-0700')
                    }
                ]);

            var water = '\u6C34', z = 'z', g_clef = '\uD834\uDD1E', cent = '\xA2',
                test = water + z + g_clef + cent;
            assertEquals('utf8', test, LLSD.parseBinary(LLSD.formatBinary(test)));
            assertThrows('bad utf16', Error, LLSD.formatBinary, '\uD834');
            assertThrows('bad utf16', Error, LLSD.formatBinary, '\uDD1E');
            assertThrows('bad utf16', Error, LLSD.formatBinary, '\uD834\u0000');
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xC0]);
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xC1]);
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xF5]);
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xF8]);
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xFC]);
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xFE]);
            assertThrows('bad utf8', Error, LLSD.parseBinary, [115, 0, 0, 0, 1, 0xFF]);

            assertThrows('bad tags', Error, LLSD.parseBinary, [91, 0, 0, 0, 0, 93 + 1]);
            assertThrows('bad tags', Error, LLSD.parseBinary, [123, 0, 0, 0, 0, 125 + 1]);
            assertThrows('bad tags', Error, LLSD.parseBinary, [0]);
        },

        'Notation parsing': function () {

            // true
            assertEquals(LLSD.parseNotation('1'), true);
            assertEquals(LLSD.parseNotation('t'), true);
            assertEquals(LLSD.parseNotation('T'), true);
            assertEquals(LLSD.parseNotation('true'), true);
            assertEquals(LLSD.parseNotation('TRUE'), true);

            // false
            assertEquals(LLSD.parseNotation('0'), false);
            assertEquals(LLSD.parseNotation('f'), false);
            assertEquals(LLSD.parseNotation('F'), false);
            assertEquals(LLSD.parseNotation('false'), false);
            assertEquals(LLSD.parseNotation('FALSE'), false);

            // binary
            var octets = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233];
            assertEquals(LLSD.parseNotation('b64"AAEBAgMFCA0VIjdZkOk="').toArray(), octets);
            assertEquals(LLSD.parseNotation('b16"000101020305080d1522375990e9"').toArray(), octets);
            assertEquals(LLSD.parseNotation('b(14)"\x00\x01\x01\x02\x03\x05\x08\x0d\x15\x22\x37\x59\x90\xe9"').toArray(), octets);

            // string
            assertEquals(LLSD.parseNotation('"foo\\"ba\\\\r"'), 'foo"ba\\r');
            assertEquals(LLSD.parseNotation("'foo\\'ba\\\\r'"), "foo'ba\\r");
            assertEquals(LLSD.parseNotation('s(8)"foo"ba\\r"'), 'foo"ba\\r');

            // date
            assertEquals(LLSD.parseNotation('d"1970-01-01T00:00:00.000Z"'), new Date(0));
            assertEquals(LLSD.parseNotation('d"1970-01-01T00:00:00.00Z"'), new Date(0));
            assertEquals(LLSD.parseNotation('d"1970-01-01T00:00:00.0Z"'), new Date(0));
            assertEquals(LLSD.parseNotation('d"1970-01-01T00:00:00.Z"'), new Date(0));
            assertEquals(LLSD.parseNotation('d"1970-01-01T00:00:00Z"'), new Date(0));

            var sample = "" +
                "[\n" +
                "  {'destination':'http://secondlife.com'}, \n" +
                "  {'version':i1}, \n" +
                "  {\n" +
                "    'agent_id':u3c115e51-04f4-523c-9fa6-98aff1034730, \n" +
                "    'session_id':u2c585cec-038c-40b0-b42e-a25ebab4d132, \n" +
                "    'circuit_code':i1075, \n" +
                "    'first_name':'Phoenix', \n" +
                "    'last_name':'Linden',\n" +
                "    'position':[r70.9247,r254.378,r38.7304], \n" +
                "    'look_at':[r-0.043753,r-0.999042,r0], \n" +
                "    'granters':[ua2e76fcd-9360-4f6d-a924-000000000003],\n" +
                "    'attachment_data':\n" +
                "    [\n" +
                "      {\n" +
                "        'attachment_point':i2,\n" +
                "        'item_id':ud6852c11-a74e-309a-0462-50533f1ef9b3,\n" +
                "        'asset_id':uc69b29b1-8944-58ae-a7c5-2ca7b23e22fb\n" +
                "      },\n" +
                "      {\n" +
                "        'attachment_point':i10, \n" +
                "        'item_id':uff852c22-a74e-309a-0462-50533f1ef900,\n" +
                "        'asset_id':u5868dd20-c25a-47bd-8b4c-dedc99ef9479\n" +
                "      }\n" +
                "    ]\n" +
                "  }\n" +
                "]\n";

            assertEquals(LLSD.parseNotation(sample), [
                { 'destination': 'http://secondlife.com' },
                { 'version': 1 },
                {
                    'agent_id': new UUID('3c115e51-04f4-523c-9fa6-98aff1034730'),
                    'session_id': new UUID('2c585cec-038c-40b0-b42e-a25ebab4d132'),
                    'circuit_code': 1075,
                    'first_name': 'Phoenix',
                    'last_name': 'Linden',
                    'position': [70.9247, 254.378, 38.7304],
                    'look_at': [-0.043753, -0.999042, 0],
                    'granters': [new UUID('a2e76fcd-9360-4f6d-a924-000000000003')],
                    'attachment_data':
                    [
                        {
                            'attachment_point': 2,
                            'item_id': new UUID('d6852c11-a74e-309a-0462-50533f1ef9b3'),
                            'asset_id': new UUID('c69b29b1-8944-58ae-a7c5-2ca7b23e22fb')
                        },
                        {
                            'attachment_point': 10,
                            'item_id': new UUID('ff852c22-a74e-309a-0462-50533f1ef900'),
                            'asset_id': new UUID('5868dd20-c25a-47bd-8b4c-dedc99ef9479')
                        }
                    ]
                }
            ]);

            sample = "" +
                "[\n" +
                "  {\n" +
                "    'creation-date':d\"2007-03-15T18:30:18Z\", \n" +
                "    'creator-id':u3c115e51-04f4-523c-9fa6-98aff1034730\n" +
                "  },\n" +
                "  s(10)\"0123456789\",\n" +
                "  \"Where's the beef?\",\n" +
                "  'Over here.',  \n" +
                "  b(158)\"default\n" +
                "{\n" +
                "    state_entry()\n" +
                "    {\n" +
                "        llSay(0, \"Hello, Avatar!\");\n" +
                "    }\n" +
                "\n" +
                "    touch_start(integer total_number)\n" +
                "    {\n" +
                "        llSay(0, \"Touched.\");\n" +
                "    }\n" +
                "}\",\n" +
                "  b64\"AABAAAAAAAAAAAIAAAA//wAAP/8AAADgAAAA5wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n" +
                "AABkAAAAZAAAAAAAAAAAAAAAZAAAAAAAAAABAAAAAAAAAAAAAAAAAAAABQAAAAEAAAAQAAAAAAAA\n" +
                "AAUAAAAFAAAAABAAAAAAAAAAPgAAAAQAAAAFAGNbXgAAAABgSGVsbG8sIEF2YXRhciEAZgAAAABc\n" +
                "XgAAAAhwEQjRABeVAAAABQBjW14AAAAAYFRvdWNoZWQuAGYAAAAAXF4AAAAIcBEI0QAXAZUAAEAA\n" +
                "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\" \n" +
                "]\n";

            assertEquals(LLSD.parseNotation(sample), [
              {
                  'creation-date': LLSD.parseISODate("2007-03-15T18:30:18Z"),
                  'creator-id': new UUID('3c115e51-04f4-523c-9fa6-98aff1034730')
              },
              "0123456789",
              "Where's the beef?",
              'Over here.',
              new Binary("ZGVmYXVsdAp7CiAgICBzdGF0ZV9lbnRyeSgpCiAgICB7CiAgICAgICAgbGxTYXkoMCwgIkhlbGxv" +
                "LCBBdmF0YXIhIik7CiAgICB9CgogICAgdG91Y2hfc3RhcnQoaW50ZWdlciB0b3RhbF9udW1iZXIp" +
                "CiAgICB7CiAgICAgICAgbGxTYXkoMCwgIlRvdWNoZWQuIik7CiAgICB9Cn0=", 'BASE64'),
              new Binary("AABAAAAAAAAAAAIAAAA//wAAP/8AAADgAAAA5wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" +
                "AABkAAAAZAAAAAAAAAAAAAAAZAAAAAAAAAABAAAAAAAAAAAAAAAAAAAABQAAAAEAAAAQAAAAAAAA" +
                "AAUAAAAFAAAAABAAAAAAAAAAPgAAAAQAAAAFAGNbXgAAAABgSGVsbG8sIEF2YXRhciEAZgAAAABc" +
                "XgAAAAhwEQjRABeVAAAABQBjW14AAAAAYFRvdWNoZWQuAGYAAAAAXF4AAAAIcBEI0QAXAZUAAEAA" +
                "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", 'BASE64')
            ]);

            bad_parse('unexpected end-of-string', Error, LLSD.parseNotation, '');
            bad_parse('expected integer', Error, LLSD.parseNotation, 'ibad');
            bad_parse('expected real', Error, LLSD.parseNotation, 'rcool');
            bad_parse('expected uuid', Error, LLSD.parseNotation, 'uwish');
            bad_parse('expected binary data', Error, LLSD.parseNotation, 'b(4)123');
            bad_parse('expected binary data', Error, LLSD.parseNotation, 'b(4)"123"');
            bad_parse('Invalid base16 sequence', Error, LLSD.parseNotation, 'b16"not hex"');
            bad_parse('Invalid base64 sequence', Error, LLSD.parseNotation, 'b64"12345"');
            bad_parse('Invalid byte value', Error, LLSD.parseNotation, 'b(1)"\uABCD"');
            bad_parse('unexpected binary base', Error, LLSD.parseNotation, 'b99"12345"');
            bad_parse('expected length', Error, LLSD.parseNotation, 'snope');
            bad_parse('expected string', Error, LLSD.parseNotation, 's(2)"1"');
            bad_parse('expected string', Error, LLSD.parseNotation, 's(2)"123"');
            bad_parse('expected string', Error, LLSD.parseNotation, '"unclosed');
            bad_parse('expected string', Error, LLSD.parseNotation, "'unclosed");
            bad_parse('expected quote', Error, LLSD.parseNotation, 'denial');
            bad_parse('expected date', Error, LLSD.parseNotation, 'd"blah"');
            bad_parse('expected quote', Error, LLSD.parseNotation, 'd"1999-01-02T21:09:33Z');
            bad_parse('unexpected end-of-string', Error, LLSD.parseNotation, '[');
            bad_parse('unexpected end-of-string', Error, LLSD.parseNotation, '[i5,');
            bad_parse('unexpected token: ,', Error, LLSD.parseNotation, '[,');
            bad_parse('unexpected end-of-string', Error, LLSD.parseNotation, '{');
            bad_parse('expected string', Error, LLSD.parseNotation, '{i5');
            bad_parse('expected colon', Error, LLSD.parseNotation, '{"key"');
            bad_parse('unexpected end-of-string', Error, LLSD.parseNotation, '{"key":');
            bad_parse('unexpected token: ,', Error, LLSD.parseNotation, '{"key":,');
            bad_parse('unexpected token: ,', Error, LLSD.parseNotation, '{,');
            bad_parse('unexpected token: g', Error, LLSD.parseNotation, 'go back!');
            bad_parse('expected end-of-string, saw: north', Error, LLSD.parseNotation, 'i5 north');
        },

        'Content types': function () {
            var sample = [123, 'abc'];
            assertEquals(sample, LLSD.parse('application/llsd+xml', "<llsd><array><integer>123</integer><string>abc</string></array></llsd>"));
            assertEquals(sample, LLSD.parse('application/llsd+json', '[123,"abc"]'));
            assertEquals(sample, LLSD.parse('application/llsd+binary', "WwAAAAJpAAAAe3MAAAADYWJjXQ=="));

            assertEquals(LLSD.format('application/llsd+xml', sample), "<llsd><array><integer>123</integer><string>abc</string></array></llsd>");
            assertEquals(LLSD.format('application/llsd+json', sample), '[123,"abc"]');
            assertEquals(LLSD.format('application/llsd+binary', sample).toString('BASE64'), "WwAAAAJpAAAAe3MAAAADYWJjXQ==");

            assertThrows('bogus content-type', Error, LLSD.parse, 'application/llsd+foo', 'foo foo foo');
            assertThrows('bogus content-type', Error, LLSD.format, 'application/llsd+foo', 'foo foo foo');
        },

        'Conversions to Undefined': function () {
            assertEquals('undefined', LLSD.asUndefined(123), null);
        },

        'Conversions to Boolean': function () {
            assertEquals('boolean', LLSD.asBoolean(true), true);
            assertEquals('boolean', LLSD.asBoolean(false), false);
            assertEquals('integer', LLSD.asBoolean(0), false);
            assertEquals('integer', LLSD.asBoolean(1), true);
            assertEquals('integer', LLSD.asBoolean(-123), true);
            assertEquals('real', LLSD.asBoolean(NaN), false);
            assertEquals('real', LLSD.asBoolean(0.0), false);
            assertEquals('real', LLSD.asBoolean(1.0), true);
            assertEquals('real', LLSD.asBoolean(Infinity), true);
            assertEquals('string', LLSD.asBoolean(''), false);
            assertEquals('string', LLSD.asBoolean('abc'), true);
            assertEquals('undefined', LLSD.asBoolean(not_defined), false);
        },

        'Conversions to Integer': function () {
            assertEquals('boolean', LLSD.asInteger(true), 1);
            assertEquals('boolean', LLSD.asInteger(false), 0);
            assertEquals('integer', LLSD.asInteger(0), 0);
            assertEquals('integer', LLSD.asInteger(123), 123);
            assertEquals('integer', LLSD.asInteger(-456), -456);
            assertEquals('real', LLSD.asInteger(NaN), 0);
            assertEquals('real', LLSD.asInteger(0), 0);
            assertEquals('real', LLSD.asInteger(12.34), 12);
            assertEquals('real', LLSD.asInteger(1e300), LLSD.MAX_INTEGER);
            assertEquals('real', LLSD.asInteger(Infinity), LLSD.MAX_INTEGER);
            assertEquals('real', LLSD.asInteger(-1e300), LLSD.MIN_INTEGER);
            assertEquals('real', LLSD.asInteger(-Infinity), LLSD.MIN_INTEGER);
            assertEquals('string', LLSD.asInteger("0"), 0);
            assertEquals('string', LLSD.asInteger("abc"), 0);
            assertEquals('string', LLSD.asInteger("-2147483648"), LLSD.MIN_INTEGER);
            assertEquals('string', LLSD.asInteger("2147483647"), LLSD.MAX_INTEGER);
            assertEquals('undefined', LLSD.asInteger(not_defined), 0);
        },

        'Conversions to Real': function () {
            assertEquals('boolean', LLSD.asReal(true), 1.0);
            assertEquals('boolean', LLSD.asReal(false), 0.0);
            assertEquals('real', LLSD.asReal(12.34), 12.34);
            assertEquals('real', LLSD.asReal(-1e300), -1e300);
            assertEquals('string', LLSD.asReal("0.0"), 0);
            assertEquals('string', LLSD.asReal("1e300"), 1e300);
            assertEquals('string', LLSD.asReal("Infinity"), Infinity);
            assertEquals('string', LLSD.asReal("+Infinity"), Infinity);
            assertEquals('string', LLSD.asReal("-Infinity"), -Infinity);
            assertEquals('string', LLSD.asReal("-Zero"), -0.0);
            assertEquals('string', LLSD.asReal("+Zero"), 0.0);
            assertEquals('string', LLSD.asReal("NaNS"), NaN);
            assertEquals('string', LLSD.asReal("NaNQ"), NaN);
            assertEquals('undefined', LLSD.asReal(not_defined), 0.0);
        },

        'Conversions to String': function () {
            assertEquals('boolean', LLSD.asString(true), 'true');
            assertEquals('boolean', LLSD.asString(false), '');
            assertEquals('string', LLSD.asString(''), '');
            assertEquals('string', LLSD.asString('abc123'), 'abc123');
            assertEquals('integer', LLSD.asString(0), '0');
            assertEquals('integer', LLSD.asString(123), '123');
            assertEquals('integer', LLSD.asString(-456), '-456');
            assertEquals('integer', LLSD.asString(-2147483648), '-2147483648');
            assertEquals('integer', LLSD.asString(2147483647), '2147483647');
            assertEquals('real', LLSD.asString(NaN), 'NaNS');
            assertEquals('real', LLSD.asString(-Infinity), '-Infinity');
            assertEquals('real', LLSD.asString(1e+300), '1e+300');
            assertEquals('real', LLSD.asString(6.0221415e+23), '6.0221415e+23');
            assertEquals('UUID', LLSD.asString(new UUID()), '00000000-0000-0000-0000-000000000000');
            assertEquals('UUID', LLSD.asString(new UUID('6bad258e-06f0-4a87-a659-493117c9c162')), '6bad258e-06f0-4a87-a659-493117c9c162');
            assertTrue('Date', /^1970-01-01T00:00:00(\.000)?Z$/.test(LLSD.asString(new Date(0))));
            assertEquals('URI', LLSD.asString(new URI('urn:foo:bar')), 'urn:foo:bar');
            assertEquals('undefined', LLSD.asString(not_defined), '');
        },

        'Conversions to UUID': function () {
            assertEquals('UUID', LLSD.asUUID(new UUID()), new UUID());
            assertEquals('UUID', LLSD.asUUID(new UUID('6bad258e-06f0-4a87-a659-493117c9c162')), new UUID('6bad258e-06f0-4a87-a659-493117c9c162'));
            assertEquals('string', LLSD.asUUID('foo bar'), new UUID('00000000-0000-0000-0000-000000000000'));
            assertEquals('string', LLSD.asUUID('6bad258e-06f0-4a87-a659-493117c9c162'), new UUID('6bad258e-06f0-4a87-a659-493117c9c162'));
            assertEquals('undefined', LLSD.asUUID(not_defined), new UUID('00000000-0000-0000-0000-000000000000'));
        },

        'Conversions to Date': function () {
            assertEquals('Date', LLSD.asDate(new Date(123456789)), new Date(123456789));
            assertEquals('string', LLSD.asDate('foo bar'), new Date(0));
            assertEquals('string', LLSD.asDate('2008-10-13T19:00:00.000Z'), new Date('Mon Oct 13 2008 12:00:00 GMT-0700'));
            assertEquals('undefined', LLSD.asDate(not_defined), new Date(0));
        },

        'Conversions to URI': function () {
            assertEquals('URI', LLSD.asURI(new URI('http://www.example.com')), new URI('http://www.example.com'));
            assertEquals('string', LLSD.asURI(''), new URI(''));
            assertEquals('string', LLSD.asURI('proto:foo%20bar'), new URI('proto:foo%20bar'));
            assertEquals('string', LLSD.asURI('foo%20bar'), new URI(''));
            assertEquals('string', LLSD.asURI('foo bar'), new URI(''));
            assertEquals('undefined', LLSD.asURI(not_defined), new URI(''));
        },

        'Conversions to Binary': function () {
            assertEquals('Binary', LLSD.asBinary(new Binary([0, 1, 2, 253, 254, 255])), new Binary([0, 1, 2, 253, 254, 255]));
            assertEquals('string', LLSD.asBinary('!@#$%'), new Binary());
            assertEquals('string', LLSD.asBinary('AQIDBA=='), new Binary([1, 2, 3, 4]));
            assertEquals('undefined', LLSD.asBinary(not_defined), new Binary());
        },

        'Helpers': function () {
            assertTrue(LLSD.isInt32(0));
            assertTrue(LLSD.isInt32(-0));
            assertTrue(LLSD.isInt32(123));
            assertTrue(LLSD.isInt32(-456));
            assertTrue(LLSD.isInt32(LLSD.MIN_INTEGER));
            assertTrue(LLSD.isInt32(LLSD.MAX_INTEGER));
            assertFalse(LLSD.isInt32(1.23));
            assertFalse(LLSD.isInt32(LLSD.MAX_INTEGER + 1));
            assertFalse(LLSD.isInt32(LLSD.MIN_INTEGER - 1));
            assertFalse(LLSD.isInt32(Infinity));
            assertFalse(LLSD.isInt32(-Infinity));
            assertTrue(LLSD.isInt32(0x7fffffff));
            assertFalse(LLSD.isInt32(0x80000000));
            assertFalse(LLSD.isInt32(0xffffffff));

            assertEquals(LLSD.type(true), 'boolean');
            assertEquals(LLSD.type(false), 'boolean');
            assertEquals(LLSD.type(123), 'integer');
            assertEquals(LLSD.type(123.456), 'real');
            assertEquals(LLSD.type('abc'), 'string');
            assertEquals(LLSD.type(new URI('http://example.com')), 'uri');
            assertEquals(LLSD.type(new Date(0)), 'date');
            assertEquals(LLSD.type(new UUID('00000000-0000-0000-0000-000000000000')), 'uuid');
            assertEquals(LLSD.type(new Binary([0, 1, 2, 3])), 'binary');
            assertEquals(LLSD.type([]), 'array');
            assertEquals(LLSD.type({}), 'map');
            assertEquals(LLSD.type(function () { }), 'undefined');
        }
    });
}());
