"use strict";
var runTests, assertTrue, assertFalse, assertEquals, assertThrows; // test_framework.js

(function () {

    runTests("ES5 Compatibility Shim Unit Tests", 117, {

        //----------------------------------------------------------------------
        // ES5 15.2 Object Objects
        //----------------------------------------------------------------------

        'Object.getPrototypeOf': function() {
            var Class = function() { };
            var obj = new Class();
            assertEquals(Object.getPrototypeOf(obj), Class.prototype);

            var proto = {};
            Class.prototype = proto;
            obj = new Class();
            assertEquals(Object.getPrototypeOf(obj), proto);

            //assertThrows(TypeError, Object.getPrototypeOf, 'not an object');
            assertThrows(TypeError, Object.getPrototypeOf, null);
        },

        /*
        
        'Object.getOwnPropertyDescriptor': function() {
            var obj = { abc: 123 };
            assertEquals(Object.getOwnPropertyDescriptor(obj, 'abc'), {
                value: 123,
                enumerable: true,
                writable: true,
                configurable: true
            });
            assertEquals(Object.getOwnPropertyDescriptor(obj, 'def'), undefined);

            var Class = function() { this.abc = 123; };
            Class.prototype = { 'def': 456 };
            obj = new Class();
            assertEquals(Object.getOwnPropertyDescriptor(obj, 'abc'), {
                value: 123,
                enumerable: true,
                writable: true,
                configurable: true
            });
            assertEquals(Object.getOwnPropertyDescriptor(obj, 'def'), undefined);

            assertThrows(TypeError, Object.getOwnPropertyDescriptor, 'not an object', 'xyz');
        },

        'Object.getOwnPropertyNames': function() {
            var obj = { abc: 123 };
            assertEquals(Object.getOwnPropertyNames(obj), ['abc']);

            var Class = function() { this.abc = 123; };
            Class.prototype = { 'def': 456 };
            obj = new Class();
            assertEquals(Object.getOwnPropertyNames(obj), ['abc']);

            assertThrows(TypeError, Object.getOwnPropertyNames, 'not an object');
        },

        'Object.create': function() {
            var Proto = { abc: 123 };
            var obj = Object.create(Proto, { def: { value: 456} });
            assertEquals(Object.getPrototypeOf(obj), Proto);
            assertEquals(obj.abc, 123);
            assertEquals(obj.def, 456);
            assertEquals(Object.getOwnPropertyNames(obj), ['def']);

            assertThrows(TypeError, Object.create, 'not an object');
            assertThrows(TypeError, Object.create, {}, 'not an object');
            assertThrows(TypeError, Object.create, 'not an object', {});
        },

        'Object.defineProperty': function() {
            var obj = { abc: 123 };
            Object.defineProperty(obj, 'def', { value: 456 });
            assertEquals(obj.abc, 123);
            assertEquals(obj.def, 456);
            assertEquals(Object.getOwnPropertyNames(obj).sort(), ['abc', 'def'].sort());

            assertThrows(TypeError, Object.defineProperty, 'not an object', 'name', {});
        },

        'Object.defineProperties': function() {
            var obj = { abc: 123 };
            Object.defineProperties(obj, { 'def': { value: 456 }, 'ghi': { value: 789} });
            assertEquals(obj.abc, 123);
            assertEquals(obj.def, 456);
            assertEquals(obj.ghi, 789);
            assertEquals(Object.getOwnPropertyNames(obj).sort(), ['abc', 'def', 'ghi'].sort());

            assertThrows(TypeError, Object.defineProperties, 'not an object', {});
        },

        'Object.seal': function() {
            var obj = { abc: 123 };
            assertEquals(Object.seal(obj), obj);
            assertThrows(TypeError, Object.seal, 'not an object');
        },

        'Object.freeze': function() {
            var obj = { abc: 123 };
            assertEquals(Object.freeze(obj), obj);
            assertThrows(TypeError, Object.freeze, 'not an object');
        },

        'Object.preventExtensions': function() {
            var obj = { abc: 123 };
            assertEquals(Object.preventExtensions(obj), obj);
            assertThrows(TypeError, Object.preventExtensions, 'not an object');
        },

        'Object.isSealed': function() {
            var obj = { abc: 123 };
            assertEquals(Object.isSealed(obj), false);
            assertThrows(TypeError, Object.isSealed, 'not an object');
        },

        'Object.isFrozen': function() {
            var obj = { abc: 123 };
            assertEquals(Object.isFrozen(obj), false);
            assertThrows(TypeError, Object.isFrozen, 'not an object');
        },

        'Object.isExtensible': function() {
            var obj = { abc: 123 };
            assertEquals(Object.isExtensible(obj), true);
            assertThrows(TypeError, Object.isExtensible, 'not an object');
        },
        */
        
        'Object.keys': function() {
            var obj = { abc: 123 };
            assertEquals(Object.keys(obj), ['abc']);

            var Class = function() { this.abc = 123; };
            Class.prototype = { 'def': 456 };
            obj = new Class();
            assertEquals(Object.keys(obj), ['abc']);

            assertThrows(TypeError, Object.keys, 'not an object');
        },

        //----------------------------------------------------------------------
        // ES5 15.3 Function Objects
        //----------------------------------------------------------------------

        'Function.prototype.bind': function() {

            function foo(a, b) {
                return [this.x, this.y, a, b];
            }

            var bound = foo.bind({ x: 1, y: 2 }, 3, 4);
            assertEquals(bound(), [1, 2, 3, 4]);

            var Class = function() { };
            Class.prototype.method = function(a, b, c) {
                return [this, a + 1, b + 2, c + 3];
            };
            var obj = new Class();

            bound = obj.method.bind(obj);
            assertEquals(bound(10, 11, 12), [obj, 11, 13, 15]);

            bound = obj.method.bind(obj, 20, 21);
            assertEquals(bound(22), [obj, 21, 23, 25]);
        },

        //----------------------------------------------------------------------
        // ES5 15.4 Array Objects
        //----------------------------------------------------------------------

        'Array.isArray': function() {
            assertTrue(Array.isArray([]));
            assertFalse(Array.isArray(1));
            assertFalse(Array.isArray('abc'));
            assertFalse(Array.isArray(null));
            assertFalse(Array.isArray());

            var Class = function() { return []; };
            var obj = new Class();
            assertTrue(Array.isArray(obj));
        },

        'Array.indexOf': function() {
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('a'), 0);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('b'), 1);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('c'), 2);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('d'), -1);

            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('a', 3), 3);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('b', 3), 4);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('c', 3), 5);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('d', 3), -1);

            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('a', 5), -1);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('b', 5), -1);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('c', 5), 5);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].indexOf('d', 5), -1);
        },

        'Array.lastIndexOf': function() {
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('a'), 3);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('b'), 4);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('c'), 5);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('d'), -1);

            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('a', 3), 3);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('b', 3), 1);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('c', 3), 2);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('d', 3), -1);

            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('a', 1), 0);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('b', 1), 1);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('c', 1), -1);
            assertEquals(['a', 'b', 'c', 'a', 'b', 'c'].lastIndexOf('d', 1), -1);
        },

        'Array.every': function() {

            var count = 0, sum = 0, data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            assertTrue(data.every(function(n) {
                count += 1;
                sum += n;
                return true;
            }));
            assertEquals(count, 10);
            assertEquals(sum, 55);

            count = 0;
            sum = 0;
            assertFalse(data.every(function(n) {
                count += 1;
                sum += n;
                return false;
            }));
            assertEquals(count, 1);
            assertEquals(sum, 1);

            var fake = { 0: 1, 1: 10, 2: 100, 3: 1000, length: 4 };
            count = 0;
            sum = 0;
            assertTrue(Array.prototype.every.call(fake, function(n) {
                count += 1;
                sum += n;
                return true;
            }));
            assertEquals(count, 4);
            assertEquals(sum, 1111);
        },

        'Array.some': function() {

            var count = 0, sum = 0, data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            assertTrue(data.some(function(n) {
                count += 1;
                sum += n;
                return true;
            }));
            assertEquals(count, 1);
            assertEquals(sum, 1);

            count = 0;
            sum = 0;
            assertFalse(data.some(function(n) {
                count += 1;
                sum += n;
                return false;
            }));
            assertEquals(count, 10);
            assertEquals(sum, 55);

            count = 0;
            sum = 0;
            assertTrue(data.some(function(n) {
                count += 1;
                sum += n;
                return n % 2 !== 1;
            }));
            assertEquals(count, 2);
            assertEquals(sum, 3);

            var fake = { 0: 1, 1: 10, 2: 100, 3: 1000, length: 4 };
            count = 0;
            sum = 0;
            assertTrue(Array.prototype.some.call(fake, function(n) {
                count += 1;
                sum += n;
                return n >= 100;
            }));
            assertEquals(count, 3);
            assertEquals(sum, 111);
        },

        'Array.forEach': function() {

            var count = 0, sum = 0, data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            data.forEach(function(n) {
                count += 1;
                sum += n;
            });
            assertEquals(count, 10);
            assertEquals(sum, 55);

            var fake = { 0: 1, 1: 10, 2: 100, 3: 1000, length: 4 };
            count = 0;
            sum = 0;
            Array.prototype.forEach.call(fake, function(n) {
                count += 1;
                sum += n;
            });
            assertEquals(count, 4);
            assertEquals(sum, 1111);
        },

        'Array.map': function() {
            var count = 0, sum = 0, data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            assertEquals(data.map(function(n) {
                count += 1;
                sum += n;
                return n * 2;
            }), [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]);
            assertEquals(count, 10);
            assertEquals(sum, 55);

            var fake = { 0: 1, 1: 10, 2: 100, 3: 1000, length: 4 };
            count = 0;
            sum = 0;
            assertEquals(Array.prototype.map.call(fake, function(n) {
                count += 1;
                sum += n;
                return n * 2;
            }), [2, 20, 200, 2000]);
            assertEquals(count, 4);
            assertEquals(sum, 1111);
        },

        'Array.filter': function() {
            var count = 0, sum = 0, data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            assertEquals(data.filter(function(n) {
                count += 1;
                sum += n;
                return n % 2;
            }), [1, 3, 5, 7, 9]);
            assertEquals(count, 10);
            assertEquals(sum, 55);

            var fake = { 0: 1, 1: 10, 2: 100, 3: 1000, length: 4 };
            count = 0;
            sum = 0;
            assertEquals(Array.prototype.filter.call(fake, function(n) {
                count += 1;
                sum += n;
                return n >= 100;
            }), [100, 1000]);
            assertEquals(count, 4);
            assertEquals(sum, 1111);
        },

        'Array.reduce': function() {
            assertEquals([1, 2, 3, 4].reduce(function(m, n) { return m + n; }), 10);
            assertEquals([1, 2, 3, 4].reduce(function(m, n) { return m + n; }, 5), 15);
            assertEquals(['a', 'b'].reduce(function(m, n) { return m + n; }), 'ab');
            assertEquals(['a', 'b'].reduce(function(m, n) { return m + n; }, 'c'), 'cab');
        },

        'Array.reduceRight': function() {
            assertEquals([1, 2, 3, 4].reduceRight(function(m, n) { return m + n; }), 10);
            assertEquals([1, 2, 3, 4].reduceRight(function(m, n) { return m + n; }, 5), 15);
            assertEquals(['a', 'b'].reduceRight(function(m, n) { return m + n; }), 'ba');
            assertEquals(['a', 'b'].reduceRight(function(m, n) { return m + n; }, 'c'), 'cba');
        },

        //----------------------------------------------------------------------
        // ES5 15.5 String Objects
        //----------------------------------------------------------------------

        'String.prototype.trim': function() {
            var ws = ['', ' ', '\t', '\r', '\n', '  ', '\r\n'];

            ws.forEach(function(w) {
                assertEquals((w + 'abc').trim(), 'abc');
                assertEquals(('abc' + w).trim(), 'abc');
                assertEquals((w + 'abc' + w).trim(), 'abc');
                assertEquals(('a' + w + 'bc').trim(), 'a' + w + 'bc');
            });
        },

        //----------------------------------------------------------------------
        // ES5 15.9 Date Objects
        //----------------------------------------------------------------------

        'Date.now': function() {
            var d = Date.now();
            assertTrue(typeof d === 'number');
            assertTrue(isFinite(d));
            assertTrue(d > 1273176325377);
        },

        'Date.prototype.toISOString': function() {
            var d = new Date();
            d.setUTCFullYear(1997);
            d.setUTCMonth(0);
            d.setUTCDate(12);
            d.setUTCHours(3);
            d.setUTCMinutes(45);
            d.setUTCSeconds(6);
            d.setUTCMilliseconds(789);

            // serializing milliseconds is optional (e.g. Safari 4)
            assertTrue(/^1997-01-12T03:45:06(\.789)?Z$/.test(d.toISOString()));

            d = new Date(0);
            assertTrue(/^1970-01-01T00:00:00(\.000)?Z$/.test(d.toISOString()));
        }
    });
} ());