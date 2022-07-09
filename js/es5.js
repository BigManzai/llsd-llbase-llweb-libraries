//----------------------------------------------------------------------
// Fallback/"polyfill" implementations of ECMAScript 5 for ECMAScript 3
//
// A copy of http://www.json.org/json2.js should be included as well
//
// NOTE: This replicates functionality identically in some cases, is
// a "close enough" approximation in others, and is merely a no-op
// with no side-effects in the rest. It also does not change the
// language semantics at all (e.g. strict mode). If that level of
// conformance is required, see:
// http://code.google.com/p/google-caja/wiki/DifferencesBetweenES5Over3AndES5
//----------------------------------------------------------------------

(function () {

    //----------------------------------------------------------------------
    // ES5 15.2 Object Objects
    //----------------------------------------------------------------------

    //
    // ES5 15.2.3 Properties of the Object Constructor
    //

    // ES5 15.2.3.2 Object.getPrototypeOf ( O )
    // From http://ejohn.org/blog/objectgetprototypeof/
    if (typeof Object.getPrototypeOf !== "function") {
        if (typeof "test".__proto__ === "object") {
            Object.getPrototypeOf = function (obj) {
                if (typeof obj !== 'object') { throw new TypeError(); }
                return obj.__proto__;
            };
        }
        else {
            Object.getPrototypeOf = function (obj) {
                if (typeof obj !== 'object') { throw new TypeError(); }
                return obj.constructor.prototype;
            };
        }
    }


    //    The following functions are excluded since the risk of incorrect 
    //    behavior overwhelms the value of an approximation. Code that
    //    may run in an ES3 environment should test for the existence 
    //    of these functions before calling.

    //    // ES5 15.2.3.3 Object.getOwnPropertyDescriptor ( O, P )
    //    if (typeof Object.getOwnPropertyDescriptor !== "function") {
    //        Object.getOwnPropertyDescriptor = function(obj, name) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            if (obj.hasOwnProperty(name)) {
    //                return {
    //                    value: obj[name],
    //                    enumerable: true,
    //                    writable: true,
    //                    configurable: true
    //                };
    //            }
    //        };
    //    }

    //    // ES5 15.2.3.4 Object.getOwnPropertyNames ( O )
    //    if (typeof Object.getOwnPropertyNames !== "function") {
    //        Object.getOwnPropertyNames = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            var props = [], p;
    //            for (p in obj) {
    //                if (obj.hasOwnProperty(p)) {
    //                    props.push(p);
    //                }
    //            }
    //            return props;
    //        };
    //    }

    //    // ES5 15.2.3.5 Object.create ( O [, Properties] )
    //    if (typeof Object.create !== "function") {
    //        Object.create = function(prototype, properties) {
    //            if (typeof prototype !== 'object') { throw new TypeError(); }
    //            var Ctor = function() { }, obj;
    //            Ctor.prototype = prototype;
    //            obj = new Ctor();
    //            if (arguments.length > 1) {
    //                if (typeof properties !== 'object') { throw new TypeError(); }
    //                Object.defineProperties(obj, properties);
    //            }
    //            return obj;
    //        };
    //    }

    // ES 15.2.3.6 Object.defineProperty ( O, P, Attributes )
    // Partial support for most common case - getters and setters
    if (Object.prototype.__defineGetter__ && !Object.defineProperty) {
        Object.defineProperty = function (obj, prop, desc) {
            if (typeof obj !== 'object') { throw new TypeError(); }
            if (desc.hasOwnProperty('get')) { obj.__defineGetter__(prop, desc.get); }
            if (desc.hasOwnProperty('set')) { obj.__defineSetter__(prop, desc.set); }
        };
    }

    //    // ES 15.2.3.7 Object.defineProperties ( O, Properties )
    //    if (typeof Object.defineProperties !== "function") {
    //        Object.defineProperties = function(obj, properties) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            for (var name in properties) {
    //                if (properties.hasOwnProperty(name)) {
    //                    Object.defineProperty(obj, name, properties[name]);
    //                }
    //            }
    //        };
    //    }

    //    // ES 15.2.3.8 Object.seal ( O )
    //    if (typeof Object.seal !== "function") {
    //        Object.seal = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            return obj;
    //        };
    //    }

    //    // ES 15.2.3.9 Object.freeze ( O )
    //    if (typeof Object.freeze !== "function") {
    //        Object.freeze = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            return obj;
    //        };
    //    }

    //    // 15.2.3.10 Object.preventExtensions ( O )
    //    if (typeof Object.preventExtensions !== "function") {
    //        Object.preventExtensions = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            return obj;
    //        };
    //    }

    //    // 15.2.3.11 Object.isSealed ( O )
    //    if (typeof Object.isSealed !== "function") {
    //        Object.isSealed = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            return false;
    //        };
    //    }

    //    // 15.2.3.12 Object.isFrozen ( O )
    //    if (typeof Object.isFrozen !== "function") {
    //        Object.isFrozen = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            return false;
    //        };
    //    }

    //    // 15.2.3.13 Object.isExtensible ( O )
    //    if (typeof Object.isExtensible !== "function") {
    //        Object.isExtensible = function(obj) {
    //            if (typeof obj !== 'object') { throw new TypeError(); }
    //            return true;
    //        };
    //    }


    // ES5 15.2.3.14 Object.keys ( O )
    if (!Object.keys) {
        Object.keys = function (o) {
            if (typeof o !== 'object') { throw new TypeError(); }
            var props = [], p;
            for (p in o) {
                if (Object.prototype.hasOwnProperty.call(o, p)) {
                    props.push(p);
                }
            }
            return props;
        };
    }


    //----------------------------------------------------------------------
    // ES5 15.3 Function Objects
    //----------------------------------------------------------------------

    //
    // ES5 15.3.4 Properties of the Function Prototype Object
    //

    // ES5 15.3.4.5 Function.prototype.bind ( thisArg [, arg1 [, arg2, ... ]] )
    // Inspired by http://www.prototypejs.org/api/function/bind
    if (!Function.prototype.bind) {
        Function.prototype.bind = function (thisArg) {

            var slice = [].slice,
                boundArgs = slice.call(arguments, 1),
                targetFunction = this,
                f;

            if (typeof targetFunction !== 'function') { throw new TypeError(); }

            f = function () {
                var extraArgs = slice.call(arguments);
                return targetFunction.apply(thisArg, boundArgs.concat(extraArgs));
            };
            // Can't set f.length without eval, alas

            return f;
        };
    }



    //----------------------------------------------------------------------
    // ES5 15.4 Array Objects
    //----------------------------------------------------------------------

    //
    // ES5 15.4.3 Properties of the Array Constructor
    //


    // ES5 15.4.3.2 Array.isArray ( arg )
    // From http://ejohn.org/blog/ecmascript-5-strict-mode-json-and-more/
    if (!Array.isArray) {
        Array.isArray = function (o) {
            return Object.prototype.toString.call(o) === "[object Array]";
        };
    }


    //
    // ES5 15.4.4 Properties of the Array Prototype Object
    //


    // ES5 15.4.4.14 Array.prototype.indexOf ( searchElement [ , fromIndex ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/indexOf
    if (!Array.prototype.indexOf) {
        Array.prototype.indexOf = function (searchElement /*, fromIndex */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (len === 0) { return -1; }

            var n = 0;
            if (arguments.length > 0) {
                n = Number(arguments[1]);
                if (isNaN(n)) {
                    n = 0;
                }
                else if (n !== 0 && n !== (1 / 0) && n !== -(1 / 0)) {
                    n = (n > 0 || -1) * Math.floor(Math.abs(n));
                }
            }

            if (n >= len) { return -1; }

            var k = n >= 0
                ? n
                : Math.max(len - Math.abs(n), 0);

            for (; k < len; k++) {
                if (k in t && t[k] === searchElement) {
                    return k;
                }
            }
            return -1;
        };
    }

    // ES5 15.4.4.15 Array.prototype.lastIndexOf ( searchElement [ , fromIndex ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/lastIndexOf
    if (!Array.prototype.lastIndexOf) {
        Array.prototype.lastIndexOf = function (searchElement /*, fromIndex*/) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (len === 0) { return -1; }

            var n = len;
            if (arguments.length > 1) {
                n = Number(arguments[1]);
                if (n !== n) {
                    n = 0;
                }
                else if (n !== 0 && n !== (1 / 0) && n !== -(1 / 0)) {
                    n = (n > 0 || -1) * Math.floor(Math.abs(n));
                }
            }

            var k = n >= 0
                ? Math.min(n, len - 1)
                : len - Math.abs(n);

            for (; k >= 0; k--) {
                if (k in t && t[k] === searchElement) {
                    return k;
                }
            }
            return -1;
        };
    }

    // ES5 15.4.4.16 Array.prototype.every ( callbackfn [ , thisArg ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/every
    if (!Array.prototype.every) {
        Array.prototype.every = function (fun /*, thisp */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof fun !== "function") { throw new TypeError(); }

            var thisp = arguments[1], i;
            for (i = 0; i < len; i++) {
                if (i in t && !fun.call(thisp, t[i], i, t)) {
                    return false;
                }
            }

            return true;
        };
    }

    // ES5 15.4.4.17 Array.prototype.some ( callbackfn [ , thisArg ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/some
    if (!Array.prototype.some) {
        Array.prototype.some = function (fun /*, thisp */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof fun !== "function") { throw new TypeError(); }

            var thisp = arguments[1], i;
            for (i = 0; i < len; i++) {
                if (i in t && fun.call(thisp, t[i], i, t)) {
                    return true;
                }
            }

            return false;
        };
    }

    // ES5 15.4.4.18 Array.prototype.forEach ( callbackfn [ , thisArg ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/forEach
    if (!Array.prototype.forEach) {
        Array.prototype.forEach = function (fun /*, thisp */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof fun !== "function") { throw new TypeError(); }

            var thisp = arguments[1], i;
            for (i = 0; i < len; i++) {
                if (i in t) {
                    fun.call(thisp, t[i], i, t);
                }
            }
        };
    }


    // ES5 15.4.4.19 Array.prototype.map ( callbackfn [ , thisArg ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Map
    if (!Array.prototype.map) {
        Array.prototype.map = function (fun /*, thisp */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof fun !== "function") { throw new TypeError(); }

            var res = []; res.length = len;
            var thisp = arguments[1], i;
            for (i = 0; i < len; i++) {
                if (i in t) {
                    res[i] = fun.call(thisp, t[i], i, t);
                }
            }

            return res;
        };
    }

    // ES5 15.4.4.20 Array.prototype.filter ( callbackfn [ , thisArg ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Filter
    if (!Array.prototype.filter) {
        Array.prototype.filter = function (fun /*, thisp */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof fun !== "function") { throw new TypeError(); }

            var res = [];
            var thisp = arguments[1], i;
            for (i = 0; i < len; i++) {
                if (i in t) {
                    var val = t[i]; // in case fun mutates this
                    if (fun.call(thisp, val, i, t)) {
                        res.push(val);
                    }
                }
            }

            return res;
        };
    }


    // ES5 15.4.4.21 Array.prototype.reduce ( callbackfn [ , initialValue ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Reduce
    if (!Array.prototype.reduce) {
        Array.prototype.reduce = function (fun /*, initialValue */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof fun !== "function") { throw new TypeError(); }

            // no value to return if no initial value and an empty array
            if (len === 0 && arguments.length === 1) { throw new TypeError(); }

            var k = 0;
            var accumulator;
            if (arguments.length >= 2) {
                accumulator = arguments[1];
            }
            else {
                do {
                    if (k in t) {
                        accumulator = t[k++];
                        break;
                    }

                    // if array contains no values, no initial value to return
                    if (++k >= len) { throw new TypeError(); }
                }
                while (true);
            }

            while (k < len) {
                if (k in t) {
                    accumulator = fun.call(undefined, accumulator, t[k], k, t);
                }
                k++;
            }

            return accumulator;
        };
    }


    // ES5 15.4.4.22 Array.prototype.reduceRight ( callbackfn [, initialValue ] )
    // From https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/ReduceRight
    if (!Array.prototype.reduceRight) {
        Array.prototype.reduceRight = function (callbackfn /*, initialValue */) {
            "use strict";

            if (this === void 0 || this === null) { throw new TypeError(); }

            var t = Object(this);
            var len = t.length >>> 0;
            if (typeof callbackfn !== "function") { throw new TypeError(); }

            // no value to return if no initial value, empty array
            if (len === 0 && arguments.length === 1) { throw new TypeError(); }

            var k = len - 1;
            var accumulator;
            if (arguments.length >= 2) {
                accumulator = arguments[1];
            }
            else {
                do {
                    if (k in this) {
                        accumulator = this[k--];
                        break;
                    }

                    // if array contains no values, no initial value to return
                    if (--k < 0) { throw new TypeError(); }
                }
                while (true);
            }

            while (k >= 0) {
                if (k in t) {
                    accumulator = callbackfn.call(undefined, accumulator, t[k], k, t);
                }
                k--;
            }

            return accumulator;
        };
    }


    //----------------------------------------------------------------------
    // ES5 15.5 String Objects
    //----------------------------------------------------------------------

    //
    // ES5 15.5.4 Properties of the String Prototype Object
    //


    // ES5 15.5.4.20 String.prototype.trim()
    if (typeof String.prototype.trim !== "function") {
        String.prototype.trim = function () {
            return this.replace(/^\s+/, '').replace(/\s+$/, '');
        };
    }


    //----------------------------------------------------------------------
    // ES5 15.9 Date Objects
    //----------------------------------------------------------------------


    //
    // ES 15.9.4 Properties of the Date Constructor
    //

    // ES5 15.9.4.4 Date.now ( )
    // From https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Global_Objects/Date/now
    if (typeof Date.now !== "function") {
        Date.now = function now() {
            return Number(new Date());
        };
    }


    //
    // ES5 15.9.5 Properties of the Date Prototype Object
    //

    // ES5 15.9.4.43 Date.prototype.toISOString ( )
    // Inspired by http://www.json.org/json2.js
    if (typeof Date.prototype.toISOString !== "function") {
        Date.prototype.toISOString = function () {
            function pad2(n) { return ('00' + n).slice(-2); }
            function pad3(n) { return ('000' + n).slice(-3); }

            return this.getUTCFullYear() + '-' +
                pad2(this.getUTCMonth() + 1) + '-' +
                pad2(this.getUTCDate()) + 'T' +
                pad2(this.getUTCHours()) + ':' +
                pad2(this.getUTCMinutes()) + ':' +
                pad2(this.getUTCSeconds()) + '.' +
                pad3(this.getUTCMilliseconds()) + 'Z';
        };
    }

    // ES5 15.9.5.44 Date.prototype.toJSON ( key )
    //
    // See "15.12 The JSON Object"


    //----------------------------------------------------------------------
    // ES5 15.12 The JSON Object
    //----------------------------------------------------------------------

    // Use a local copy of http://www.json.org/json2.js for:
    //    ES 15.12.2 JSON.parse()
    //    ES 15.12.3 JSON.stringify()
    //    Date.prototype.toJSON()
} ());
