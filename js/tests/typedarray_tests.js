"use strict";

var runTests, assertTrue, assertEquals, assertThrows; // test_framework.js
var ArrayBuffer, 
    Int8Array, Uint8Array, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array,
    DataView; // typedarray.js
var DOMException; // DOM

(function () {

    function isPositiveZero(x) { return 1 / x === +Infinity; }
    function isNegativeZero(x) { return 1 / x === -Infinity; }

    function checkArray(typed_array, test) {
        assertEquals(typed_array.length, test.length);
        for (var i = 0; i < test.length; i += 1) {
            assertEquals(typed_array.get(i), test[i]);
        }
    }

    // Add a TypedArray.get(index) accessor if not present, for
    // testing native implementations.
    var array_types = [Int8Array, Uint8Array, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array];
    array_types.forEach(function(type) {
        if (type && typeof type.prototype.get !== 'function') {	
            type.prototype.get = function(idx) {
                return this[idx];
            };
        }
    });


    // Shim to work with older impls that use "slice" instead of "subarray"
    array_types.forEach(function(type) {
       if (type && typeof type.prototype.subarray !== 'function') {
           type.prototype.subarray = type.prototype.slice;
       }
    });


    // e.g. extractbits([0xff, 0x80, 0x00, 0x00], 23, 30); inclusive
    function extractbits(bytes, lo, hi) {
        var out = 0;
        bytes = bytes.slice(); // make a copy
        var lsb = bytes.pop(), sc = 0, sh = 0;

        for (; lo > 0;  lo--, hi--) {
            lsb >>= 1;
            if (++sc === 8) { sc = 0; lsb = bytes.pop(); }
        }

        for (; hi >= 0;  hi--) {
            out = out | (lsb & 0x01) << sh++;
            lsb >>= 1;
            if (++sc === 8) { sc = 0; lsb = bytes.pop(); }
        }

        return out;
    }


    runTests("Typed Array Unit Tests", 446, {

        'ArrayBuffer': function() {
            var b;

            assertEquals('no length', (new ArrayBuffer()).byteLength, 0);
            assertTrue('creation', b = new ArrayBuffer(0));
            assertTrue('creation', b = new ArrayBuffer(1));
            assertTrue('creation', b = new ArrayBuffer(123));

            b = new ArrayBuffer(123);
            assertEquals('length', b.byteLength, 123);

            assertThrows('negative length', RangeError, function() { return new ArrayBuffer(-1); });
            assertThrows('absurd length', RangeError, function() { return new ArrayBuffer(0x80000000); });
        },

        'ArrayBufferView': function() {
            var ab = new ArrayBuffer(48);
            var i32 = new Int32Array(ab, 16);
            i32.set([1, 2, 3, 4, 5, 6, 7, 8]);

            assertEquals(i32.buffer, ab);
            assertEquals(i32.byteOffset, 16);
            assertEquals(i32.byteLength, 32);

            var da = new DataView(i32.buffer, 8);
            assertEquals(da.buffer, ab);
            assertEquals(da.byteOffset, 8);
            assertEquals(da.byteLength, 40);
        },

        'TypedArrays': function() {
            var a;

            assertEquals('Int8Array.BYTES_PER_ELEMENT', Int8Array.BYTES_PER_ELEMENT, 1);
            a = new Int8Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 1);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 8);

            assertEquals('Uint8Array.BYTES_PER_ELEMENT', Uint8Array.BYTES_PER_ELEMENT, 1);
            a = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 1);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 8);

            assertEquals('Int16Array.BYTES_PER_ELEMENT', Int16Array.BYTES_PER_ELEMENT, 2);
            a = new Int16Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 2);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 16);

            assertEquals('Uint16Array.BYTES_PER_ELEMENT', Uint16Array.BYTES_PER_ELEMENT, 2);
            a = new Uint16Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 2);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 16);

            assertEquals('Int32Array.BYTES_PER_ELEMENT', Int32Array.BYTES_PER_ELEMENT, 4);
            a = new Int32Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 4);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 32);

            assertEquals('Uint32Array.BYTES_PER_ELEMENT', Uint32Array.BYTES_PER_ELEMENT, 4);
            a = new Uint32Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 4);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 32);

            assertEquals('Float32Array.BYTES_PER_ELEMENT', Float32Array.BYTES_PER_ELEMENT, 4);
            a = new Float32Array([1, 2, 3, 4, 5, 6, 7, 8]);
            assertEquals(a.BYTES_PER_ELEMENT, 4);
            assertEquals(a.byteOffset, 0);
            assertEquals(a.byteLength, 32);

            // Not all browsers implement Float64Array yet
            if (Float64Array) {
                assertEquals('Float64Array.BYTES_PER_ELEMENT', Float64Array.BYTES_PER_ELEMENT, 8);
                a = new Float64Array([1, 2, 3, 4, 5, 6, 7, 8]);
                assertEquals(a.BYTES_PER_ELEMENT, 8);
                assertEquals(a.byteOffset, 0);
                assertEquals(a.byteLength, 64);
            }
        },

        'typed array constructors': function() {


            checkArray(new Int8Array({ 'length': 3 }), [0, 0, 0]);
            var rawbuf = (new Uint8Array([0, 1, 2, 3, 4, 5, 6, 7])).buffer;

            var int8 = new Int8Array();
            assertEquals('no args', int8.length, 0);
            assertThrows('bogus length', RangeError, function() { return new Int8Array(-1); });
            assertThrows('bogus length', RangeError, function() { return new Int8Array(0x80000000); });

            int8 = new Int8Array(4);
            assertEquals(int8.BYTES_PER_ELEMENT, 1);
            assertEquals('length', int8.length, 4);
            assertEquals('length', int8.byteLength, 4);
            assertEquals('length', int8.byteOffset, 0);
            assertEquals('length, out of bounds', int8.get(-1), undefined);
            assertEquals('length, out of bounds', int8.get(4), undefined);

            int8 = new Int8Array([1, 2, 3, 4, 5, 6]);
            assertEquals('array', int8.length, 6);
            assertEquals('array', int8.byteLength, 6);
            assertEquals('array', int8.byteOffset, 0);
            assertEquals('array', int8.get(3), 4);
            assertEquals('array, out of bounds', int8.get(-1), undefined);
            assertEquals('array, out of bounds', int8.get(6), undefined);

            int8 = new Int8Array(rawbuf);
            assertEquals('buffer', int8.length, 8);
            assertEquals('buffer', int8.byteLength, 8);
            assertEquals('buffer', int8.byteOffset, 0);
            assertEquals('buffer', int8.get(7), 7);
            int8.set([111]);
            assertEquals('buffer', int8.get(0), 111);
            assertEquals('buffer, out of bounds', int8.get(-1), undefined);
            assertEquals('buffer, out of bounds', int8.get(8), undefined);

            int8 = new Int8Array(rawbuf, 2);
            assertEquals('buffer, byteOffset', int8.length, 6);
            assertEquals('buffer, byteOffset', int8.byteLength, 6);
            assertEquals('buffer, byteOffset', int8.byteOffset, 2);
            assertEquals('buffer, byteOffset', int8.get(5), 7);
            int8.set([112]);
            assertEquals('buffer', int8.get(0), 112);
            assertEquals('buffer, byteOffset, out of bounds', int8.get(-1), undefined);
            assertEquals('buffer, byteOffset, out of bounds', int8.get(6), undefined);

            int8 = new Int8Array(rawbuf, 8);
            assertEquals('buffer, byteOffset', int8.length, 0);

            assertThrows('invalid byteOffset', DOMException, function() { return new Int8Array(rawbuf, -1); });
            assertThrows('invalid byteOffset', DOMException, function() { return new Int8Array(rawbuf, 9); });
            assertThrows('invalid byteOffset', DOMException, function() { return new Int8Array(rawbuf, -1); });
            assertThrows('invalid byteOffset', RangeError, function() { return new Int32Array(rawbuf, 5); });

            int8 = new Int8Array(rawbuf, 2, 4);
            assertEquals('buffer, byteOffset, length', int8.length, 4);
            assertEquals('buffer, byteOffset, length', int8.byteLength, 4);
            assertEquals('buffer, byteOffset, length', int8.byteOffset, 2);
            assertEquals('buffer, byteOffset, length', int8.get(3), 5);
            int8.set([113]);
            assertEquals('buffer, byteOffset, length', int8.get(0), 113);
            assertEquals('buffer, byteOffset, length, out of bounds', int8.get(-1), undefined);
            assertEquals('buffer, byteOffset, length, out of bounds', int8.get(4), undefined);

            assertThrows('invalid byteOffset+length', DOMException, function() { return new Int8Array(rawbuf, 0, 9); });
            assertThrows('invalid byteOffset+length', DOMException, function() { return new Int8Array(rawbuf, 8, 1); });
            assertThrows('invalid byteOffset+length', DOMException, function() { return new Int8Array(rawbuf, 9, -1); });
        },

        'TypedArray clone constructor': function() {
            var src = new Int32Array([1, 2, 3, 4, 5, 6, 7, 8]);
            var dst = new Int32Array(src);
            checkArray(dst, [1, 2, 3, 4, 5, 6, 7, 8]);
            src.set([99]);
            checkArray(src, [99, 2, 3, 4, 5, 6, 7, 8]);
            checkArray(dst, [1, 2, 3, 4, 5, 6, 7, 8]);
        },

        'conversions': function() {
            var uint8 = new Uint8Array([1, 2, 3, 4]),
                uint16 = new Uint16Array(uint8.buffer),
                uint32 = new Uint32Array(uint8.buffer);

            // Note: can't probe individual bytes without endianness awareness
            checkArray(uint8, [1, 2, 3, 4]);
            uint16.set([0xffff]);
            checkArray(uint8, [0xff, 0xff, 3, 4]);
            uint16.set([0xeeee], 1);
            checkArray(uint8, [0xff, 0xff, 0xee, 0xee]);
            uint32.set([0x11111111]);
            assertEquals(uint16.get(0), 0x1111);
            assertEquals(uint16.get(1), 0x1111);
            checkArray(uint8, [0x11, 0x11, 0x11, 0x11]);
        },

        'signed/unsigned conversions': function() {

            var int8 = new Int8Array(1), uint8 = new Uint8Array(int8.buffer);
            uint8.set([123]);
            assertEquals('int8/uint8', int8.get(0), 123);
            uint8.set([161]);
            assertEquals('int8/uint8', int8.get(0), -95);
            int8.set([-120]);
            assertEquals('uint8/int8', uint8.get(0), 136);
            int8.set([-1]);
            assertEquals('uint8/int8', uint8.get(0), 0xff);

            var int16 = new Int16Array(1), uint16 = new Uint16Array(int16.buffer);
            uint16.set([3210]);
            assertEquals('int16/uint16', int16.get(0), 3210);
            uint16.set([49232]);
            assertEquals('int16/uint16', int16.get(0), -16304);
            int16.set([-16384]);
            assertEquals('uint16/int16', uint16.get(0), 49152);
            int16.set([-1]);
            assertEquals('uint16/int16', uint16.get(0), 0xffff);

            var int32 = new Int32Array(1), uint32 = new Uint32Array(int32.buffer);
            uint32.set([0x80706050]);
            assertEquals('int32/uint32', int32.get(0), -2140118960);
            int32.set([-2023406815]);
            assertEquals('uint32/int32', uint32.get(0), 0x87654321);
            int32.set([-1]);
            assertEquals('uint32/int32', uint32.get(0), 0xffffffff);
        },



        'IEEE754 single precision parsing': function() {

            function fromBytes(bytes) {
                var uint8 = new Uint8Array(bytes),
                    dv = new DataView(uint8.buffer);
                return dv.getFloat32(0);
            }

            assertEquals('Q-NaN', fromBytes([0xff, 0xff, 0xff, 0xff]), NaN);
            assertEquals('Q-NaN', fromBytes([0xff, 0xc0, 0x00, 0x01]), NaN);

            assertEquals('Indeterminate', fromBytes([0xff, 0xc0, 0x00, 0x00]), NaN);
            assertEquals('S-NaN', fromBytes([0xff, 0xbf, 0xff, 0xff]), NaN);
            assertEquals('S-NaN', fromBytes([0xff, 0x80, 0x00, 0x01]), NaN);

            assertEquals('-Infinity', fromBytes([0xff, 0x80, 0x00, 0x00]), -Infinity);

            assertEquals('-Normalized', fromBytes([0xff, 0x7f, 0xff, 0xff]), -3.4028234663852886E+38);
            assertEquals('-Normalized', fromBytes([0x80, 0x80, 0x00, 0x00]), -1.1754943508222875E-38);

            assertEquals('-Denormalized', fromBytes([0x80, 0x7f, 0xff, 0xff]), -1.1754942106924411E-38);
            assertEquals('-Denormalized', fromBytes([0x80, 0x00, 0x00, 0x01]), -1.4012984643248170E-45);

            assertTrue('-0', isNegativeZero(fromBytes([0x80, 0x00, 0x00, 0x00])));
            assertTrue('+0', isPositiveZero(fromBytes([0x00, 0x00, 0x00, 0x00])));

            assertEquals('+Denormalized', fromBytes([0x00, 0x00, 0x00, 0x01]), 1.4012984643248170E-45);
            assertEquals('+Denormalized', fromBytes([0x00, 0x7f, 0xff, 0xff]), 1.1754942106924411E-38);

            assertEquals('+Normalized', fromBytes([0x00, 0x80, 0x00, 0x00]), 1.1754943508222875E-38);
            assertEquals('+Normalized', fromBytes([0x7f, 0x7f, 0xff, 0xff]), 3.4028234663852886E+38);

            assertEquals('+Infinity', fromBytes([0x7f, 0x80, 0x00, 0x00]), +Infinity);

            assertEquals('S+NaN', fromBytes([0x7f, 0x80, 0x00, 0x01]), NaN);
            assertEquals('S+NaN', fromBytes([0x7f, 0xbf, 0xff, 0xff]), NaN);
            assertEquals('Q+NaN', fromBytes([0x7f, 0xc0, 0x00, 0x00]), NaN);
            assertEquals('Q+NaN', fromBytes([0x7f, 0xff, 0xff, 0xff]), NaN);
        },

        'IEEE754 single precision formatting': function() {

            function toBytes(v) {
                var uint8 = new Uint8Array(4), dv = new DataView(uint8.buffer);
                dv.setFloat32(0, v);
                var bytes = [];
                for (var i = 0; i < 4; i += 1) {
                    bytes.push(uint8.get(i));
                }
                return bytes;
            }

            assertEquals('-Infinity', toBytes(-Infinity), [0xff, 0x80, 0x00, 0x00]);

            assertEquals('-Normalized', toBytes(-3.4028234663852886E+38), [0xff, 0x7f, 0xff, 0xff]);
            assertEquals('-Normalized', toBytes(-1.1754943508222875E-38), [0x80, 0x80, 0x00, 0x00]);

            assertEquals('-Denormalized', toBytes(-1.1754942106924411E-38), [0x80, 0x7f, 0xff, 0xff]);
            assertEquals('-Denormalized', toBytes(-1.4012984643248170E-45), [0x80, 0x00, 0x00, 0x01]);

            assertEquals('-0', toBytes(-0), [0x80, 0x00, 0x00, 0x00]);
            assertEquals('+0', toBytes(0), [0x00, 0x00, 0x00, 0x00]);

            assertEquals('+Denormalized', toBytes(1.4012984643248170E-45), [0x00, 0x00, 0x00, 0x01]);
            assertEquals('+Denormalized', toBytes(1.1754942106924411E-38), [0x00, 0x7f, 0xff, 0xff]);

            assertEquals('+Normalized', toBytes(1.1754943508222875E-38), [0x00, 0x80, 0x00, 0x00]);
            assertEquals('+Normalized', toBytes(3.4028234663852886E+38), [0x7f, 0x7f, 0xff, 0xff]);

            assertEquals('+Infinity', toBytes(+Infinity), [0x7f, 0x80, 0x00, 0x00]);

            // Allow any NaN pattern (exponent all 1's, fraction non-zero)
            var nanbytes = toBytes(NaN),
                sign = extractbits(nanbytes, 31, 31),
                exponent = extractbits(nanbytes, 23, 30),
                fraction = extractbits(nanbytes, 0, 22);
            assertTrue('NaN', exponent === 255 && fraction !== 0);
        },


        'IEEE754 double precision parsing': function() {

            function fromBytes(bytes) {
                var uint8 = new Uint8Array(bytes),
                    dv = new DataView(uint8.buffer);
                return dv.getFloat64(0);
            }

            assertEquals('Q-NaN', fromBytes([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), NaN);
            assertEquals('Q-NaN', fromBytes([0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]), NaN);

            assertEquals('Indeterminate', fromBytes([0xff, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), NaN);
            assertEquals('S-NaN', fromBytes([0xff, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), NaN);
            assertEquals('S-NaN', fromBytes([0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]), NaN);

            assertEquals('-Infinity', fromBytes([0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), -Infinity);

            assertEquals('-Normalized', fromBytes([0xff, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), -1.7976931348623157E+308);
            assertEquals('-Normalized', fromBytes([0x80, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), -2.2250738585072014E-308);

            assertEquals('-Denormalized', fromBytes([0x80, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), -2.2250738585072010E-308);
            assertEquals('-Denormalized', fromBytes([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]), -4.9406564584124654E-324);

            assertTrue('-0', isNegativeZero(fromBytes([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])));
            assertTrue('+0', isPositiveZero(fromBytes([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])));

            assertEquals('+Denormalized', fromBytes([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]), 4.9406564584124654E-324);
            assertEquals('+Denormalized', fromBytes([0x00, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), 2.2250738585072010E-308);

            assertEquals('+Normalized', fromBytes([0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), 2.2250738585072014E-308);
            assertEquals('+Normalized', fromBytes([0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), 1.7976931348623157E+308);

            assertEquals('+Infinity', fromBytes([0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), +Infinity);

            assertEquals('S+NaN', fromBytes([0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]), NaN);
            assertEquals('S+NaN', fromBytes([0x7f, 0xf7, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), NaN);
            assertEquals('Q+NaN', fromBytes([0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]), NaN);
            assertEquals('Q+NaN', fromBytes([0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]), NaN);
        },

        'IEEE754 double precision formatting': function() {

            function toBytes(v) {
                var uint8 = new Uint8Array(8),
                    dv = new DataView(uint8.buffer);
                dv.setFloat64(0, v);
                var bytes = [];
                for (var i = 0; i < 8; i += 1) {
                    bytes.push(uint8.get(i));
                }
                return bytes;
            }

            assertEquals('-Infinity', toBytes(-Infinity), [0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);

            assertEquals('-Normalized', toBytes(-1.7976931348623157E+308), [0xff, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
            assertEquals('-Normalized', toBytes(-2.2250738585072014E-308), [0x80, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);

            assertEquals('-Denormalized', toBytes(-2.2250738585072010E-308), [0x80, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
            assertEquals('-Denormalized', toBytes(-4.9406564584124654E-324), [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);

            assertEquals('-0', toBytes(-0), [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
            assertEquals('+0', toBytes(0), [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);

            assertEquals('+Denormalized', toBytes(4.9406564584124654E-324), [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]);
            assertEquals('+Denormalized', toBytes(2.2250738585072010E-308), [0x00, 0x0f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);

            assertEquals('+Normalized', toBytes(2.2250738585072014E-308), [0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
            assertEquals('+Normalized', toBytes(1.7976931348623157E+308), [0x7f, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);

            assertEquals('+Infinity', toBytes(+Infinity), [0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);

            // Allow any NaN pattern (exponent all 1's, fraction non-zero)
            var nanbytes = toBytes(NaN),
                sign = extractbits(nanbytes, 63, 63),
                exponent = extractbits(nanbytes, 52, 62),
                fraction = extractbits(nanbytes, 0, 51);
            assertTrue('NaN', exponent === 2047 && fraction !== 0);
        },

        'Int32Array round trips': function() {
            var i32 = new Int32Array([0]);
            var data = [
                0,
                1,
                -1,
                123,
                -456,
                0x80000000 >> 0,
                0x7fffffff >> 0,
                0x12345678 >> 0,
                0x87654321 >> 0
            ];

            for (var i = 0; i < data.length; i += 1) {
                var datum = data[i];
                i32.set([datum]);
                assertEquals(datum, datum, i32.get(0));
            }
        },

        'Int16Array round trips': function() {
            var i16 = new Int16Array([0]);
            var data = [
                0,
                1,
                -1,
                123,
                -456,
                0xffff8000 >> 0,
                0x00007fff >> 0,
                0x00001234 >> 0,
                0xffff8765 >> 0
            ];

            for (var i = 0; i < data.length; i += 1) {
                var datum = data[i];
                i16.set([datum]);
                assertEquals(datum, datum, i16.get(0));
            }
        },

        'Int8Array round trips': function() {
            var i8 = new Int8Array([0]);
            var data = [
                0,
                1,
                -1,
                123,
                -45,
                0xffffff80 >> 0,
                0x0000007f >> 0,
                0x00000012 >> 0,
                0xffffff87 >> 0
            ];

            for (var i = 0; i < data.length; i += 1) {
                var datum = data[i];
                i8.set([datum]);
                assertEquals(datum, datum, i8.get(0));
            }
        },


        'IEEE754 single precision round trips': function() {

            var f32 = new Float32Array([0]);

            var data = [
                0,
                1,
                -1,
                123,
                -456,

                1.2,
                1.23,
                1.234,

                1.234e-30,
                1.234e-20,
                1.234e-10,
                1.234e10,
                1.234e20,
                1.234e30,

                3.1415,
                6.0221415e+23,
                6.6260693e-34,
                6.67428e-11,
                299792458,

                0,
                -0,
                Infinity,
                -Infinity,
                NaN
            ];

            // Round p to n binary places of binary
            function precision(n, p) {
                if (p >= 52 || isNaN(n) || n === 0 || n === Infinity || n === -Infinity) {
                    return n;
                }
                else {
                    var m = Math.pow(2, p - Math.floor(Math.log(n) / Math.LN2));
                    return Math.round(n * m) / m;
                }
            }
            function single(n) { return precision(n, 23); }

            for (var i = 0; i < data.length; i += 1) {
                var datum = data[i];
                f32.set([datum]);
                assertEquals('value: ' + datum, single(datum), single(f32.get(0)));
            }
        },

        'IEEE754 double precision round trips': function() {

            if (!Float64Array) return; // Not all browsers implement Float64Array yet


            var f64 = new Float64Array([0]);

            var data = [
                0,
                1,
                -1,
                123,
                -456,

                1.2,
                1.23,
                1.234,

                1.234e-30,
                1.234e-20,
                1.234e-10,
                1.234e10,
                1.234e20,
                1.234e30,

                3.1415,
                6.0221415e+23,
                6.6260693e-34,
                6.67428e-11,
                299792458,

                0,
                -0,
                Infinity,
                -Infinity,
                NaN
            ];

            for (var i = 0; i < data.length; i += 1) {
                var datum = data[i];
                f64.set([datum]);
                assertEquals(datum, datum, f64.get(0));
            }
        },


        'TypedArray setting': function() {

            var a = new Int32Array([1, 2, 3, 4, 5]);
            var b = new Int32Array(5);
            b.set(a);
            checkArray(b, [1, 2, 3, 4, 5]);
            assertThrows(DOMException, b.set.bind(b), a, 1);

            b.set(new Int32Array([99, 98]), 2);
            checkArray(b, [1, 2, 99, 98, 5]);

            b.set(new Int32Array([99, 98, 97]), 2);
            checkArray(b, [1, 2, 99, 98, 97]);

            assertThrows(DOMException, b.set.bind(b), new Int32Array([99, 98, 97, 96]), 2);

            //assertThrows(DOMException, b.set.bind(b), 12, 0);
            assertThrows(DOMException, b.set.bind(b), [101, 102, 103, 104], 4);

            //  ab = [ 0, 1, 2, 3, 4, 5, 6, 7 ]
            //  a1 = [ ^, ^, ^, ^, ^, ^, ^, ^ ]
            //  a2 =             [ ^, ^, ^, ^ ]
            var ab = new ArrayBuffer(8);
            var a1 = new Uint8Array(ab);
            for (var i = 0; i < a1.length; i += 1) { a1.set([i], i); }
            var a2 = new Uint8Array(ab, 4);
            a1.set(a2, 2);
            checkArray(a1, [0, 1, 4, 5, 6, 7, 6, 7]);
            checkArray(a2, [6, 7, 6, 7]);
        },


        'TypedArray.subarray': function() {

            var a = new Int32Array([1, 2, 3, 4, 5]);
            checkArray(a.subarray(3), [4, 5]);
            checkArray(a.subarray(1, 3), [2, 3]);
            checkArray(a.subarray(-3), [3, 4, 5]);
            checkArray(a.subarray(-3, -1), [3, 4]);
            checkArray(a.subarray(3, 2), []);
            checkArray(a.subarray(-2, -3), []);
            checkArray(a.subarray(4, 1), []);
            checkArray(a.subarray(-1, -4), []);
        },

        'DataView constructors': function() {

            var d = new DataView(new ArrayBuffer(8));

            d.setUint32(0, 0x12345678);
            assertEquals('big endian/big endian', d.getUint32(0), 0x12345678);

            d.setUint32(0, 0x12345678, true);
            assertEquals('little endian/little endian', d.getUint32(0, true), 0x12345678);

            d.setUint32(0, 0x12345678, true);
            assertEquals('little endian/big endian', d.getUint32(0), 0x78563412);

            d.setUint32(0, 0x12345678);
            assertEquals('big endian/little endian', d.getUint32(0, true), 0x78563412);

            assertThrows('no arguments', TypeError, function() { return new DataView(); });
            assertThrows('non-ArrayBuffer argument', TypeError, function() { return new DataView([]); });
            assertThrows('non-ArrayBuffer argument', TypeError, function() { return new DataView("bogus"); });
        },

        'DataView accessors': function() {
            var u = new Uint8Array(8), d = new DataView(u.buffer);
            checkArray(u, [0, 0, 0, 0, 0, 0, 0, 0]);

            d.setUint8(0, 255);
            checkArray(u, [0xff, 0, 0, 0, 0, 0, 0, 0]);

            d.setInt8(1, -1);
            checkArray(u, [0xff, 0xff, 0, 0, 0, 0, 0, 0]);

            d.setUint16(2, 0x1234);
            checkArray(u, [0xff, 0xff, 0x12, 0x34, 0, 0, 0, 0]);

            d.setInt16(4, -1);
            checkArray(u, [0xff, 0xff, 0x12, 0x34, 0xff, 0xff, 0, 0]);

            d.setUint32(1, 0x12345678);
            checkArray(u, [0xff, 0x12, 0x34, 0x56, 0x78, 0xff, 0, 0]);

            d.setInt32(4, -2023406815);
            checkArray(u, [0xff, 0x12, 0x34, 0x56, 0x87, 0x65, 0x43, 0x21]);

            d.setFloat32(2, 1.2E+38);
            checkArray(u, [0xff, 0x12, 0x7e, 0xb4, 0x8e, 0x52, 0x43, 0x21]);

            d.setFloat64(0, -1.2345678E+301);
            checkArray(u, [0xfe, 0x72, 0x6f, 0x51, 0x5f, 0x61, 0x77, 0xe5]);

            u.set([0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87]);
            assertEquals(d.getUint8(0), 128);
            assertEquals(d.getInt8(1), -127);
            assertEquals(d.getUint16(2), 33411);
            assertEquals(d.getInt16(3), -31868);
            assertEquals(d.getUint32(4), 2223343239);
            assertEquals(d.getInt32(2), -2105310075);
            assertEquals(d.getFloat32(2), -1.932478247535851e-37);
            assertEquals(d.getFloat64(0), -3.116851295377095e-306);

        },

        'DataView endian': function() {

            var rawbuf = (new Uint8Array([0, 1, 2, 3, 4, 5, 6, 7])).buffer;
            var d;

            d = new DataView(rawbuf);
            assertEquals('buffer', d.byteLength, 8);
            assertEquals('buffer', d.byteOffset, 0);
            assertThrows('bounds for buffer', DOMException, d.getUint8.bind(d), -2); // Chrome bug for index -1?
            assertThrows('bounds for buffer', DOMException, d.getUint8.bind(d), 8);
            assertThrows('bounds for buffer', DOMException, d.setUint8.bind(d), -2, 0);
            assertThrows('bounds for buffer', DOMException, d.setUint8.bind(d), 8, 0);

            d = new DataView(rawbuf, 2);
            assertEquals('buffer, byteOffset', d.byteLength, 6);
            assertEquals('buffer, byteOffset', d.byteOffset, 2);
            assertEquals('buffer, byteOffset', d.getUint8(5), 7);
            assertThrows('bounds for buffer, byteOffset', DOMException, d.getUint8.bind(d), -2);
            assertThrows('bounds for buffer, byteOffset', DOMException, d.getUint8.bind(d), 6);
            assertThrows('bounds for buffer, byteOffset', DOMException, d.setUint8.bind(d), -2, 0);
            assertThrows('bounds for buffer, byteOffset', DOMException, d.setUint8.bind(d), 6, 0);

            d = new DataView(rawbuf, 8);
            assertEquals('buffer, byteOffset', d.byteLength, 0);

            assertThrows('invalid byteOffset', DOMException, function() { return new DataView(rawbuf, -1); });
            assertThrows('invalid byteOffset', DOMException, function() { return new DataView(rawbuf, 9); });
            assertThrows('invalid byteOffset', DOMException, function() { return new DataView(rawbuf, -1); });

            d = new DataView(rawbuf, 2, 4);
            assertEquals('buffer, byteOffset, length', d.byteLength, 4);
            assertEquals('buffer, byteOffset, length', d.byteOffset, 2);
            assertEquals('buffer, byteOffset, length', d.getUint8(3), 5);
            assertThrows('bounds for buffer, byteOffset, length', DOMException, function() { return d.getUint8(-2); });
            assertThrows('bounds for buffer, byteOffset, length', DOMException, d.getUint8.bind(d), 4);
            assertThrows('bounds for buffer, byteOffset, length', DOMException, d.setUint8.bind(d), -2, 0);
            assertThrows('bounds for buffer, byteOffset, length', DOMException, d.setUint8.bind(d), 4, 0);

            assertThrows('invalid byteOffset+length', DOMException, function() { return new DataView(rawbuf, 0, 9); });
            assertThrows('invalid byteOffset+length', DOMException, function() { return new DataView(rawbuf, 8, 1); });
            assertThrows('invalid byteOffset+length', DOMException, function() { return new DataView(rawbuf, 9, -1); });
        },

        'Typed Array getters/setters': function() {

            // First, make sure this even basically works - it's ES5-only
            var a = new Uint8Array([123]);
            assertEquals(a.get(0), 123);
            a[0] = 66;
            if (a.get(0) !== 66) { return; } // Nope, Object.defineProperties or fallback not available

            var bytes = new Uint8Array([1, 2, 3, 4]),
                uint32s = new Uint32Array(bytes.buffer);

            assertEquals(bytes[1], 2);
            uint32s[0] = 0xffffffff;
            assertEquals(bytes[1], 0xff);
        }

    });
}());
