var assert, assertEquals, assertTrue, assertFalse, assertThrows, fail, runTests;

(function () {

    // Deep compare for primitives, Arrays and Objects
    // Includes special cases for NaNs and Dates
    function compare(a, b) {

        function isNegativeZero(x) { return (x === 0) && (1 / x === -Infinity); }

        var i;

        if (typeof a !== typeof b) {
            return false;
        }
        else if (typeof a === 'number' && isNaN(a) && isNaN(b)) {
            return true;
        }
        else if (typeof a === 'number' && a === 0 && b === 0) {
            return isNegativeZero(a) === isNegativeZero(b);
        }
        else if (typeof a !== 'object') {
            return a === b;
        }
        else if (a === null || b === null) {
            return a === b;
        }
        else if (a instanceof Date && b instanceof Date) {
            return Number(a) === Number(b);
        }
        else if (a instanceof Array && b instanceof Array) {
            if (a.length !== b.length) { return false; }
            for (i = 0; i < a.length; i += 1) {
                if (!compare(a[i], b[i])) { return false; }
            }
            return true;
        }
        else {
            var aks = Object.keys(a).sort(), bks = Object.keys(b).sort();
            if (aks.length !== bks.length) { return false; }
            for (i = 0; i < aks.length; i += 1) {
                var ak = aks[i], bk = bks[i];
                if (ak !== bk) { return false; }
                if (!compare(a[ak], b[bk])) { return false; }
            }
            return true;
        }
    }


    function AssertionError(message) {
        this.name = 'AssertionError';
        this.message = message;
    }

    var assertion_count, test_count, failure_count;

    function _assert(comment, booleanValue) {
        assertion_count += 1;
        if (!booleanValue) {
            throw new AssertionError(comment ? comment : 'Assertion failed');
        }
    }

    assert = function (booleanValue) {
        var comment, args = [].slice.call(arguments);
        if (args.length > 1) { comment = args.shift(); booleanValue = args.shift(); }
        _assert(comment, booleanValue);
    };

    assertEquals = function (value1, value2) {
        var comment, args = [].slice.call(arguments);
        if (args.length > 2) { comment = args.shift(); value1 = args.shift(); value2 = args.shift(); }
        _assert(comment, compare(value1, value2));
    };

    assertTrue = function (value) {
        var comment, args = [].slice.call(arguments);
        if (args.length > 1) { comment = args.shift(); value = args.shift(); }
        _assert(comment, !!value);
    };

    assertFalse = function (value) {
        var comment, args = [].slice.call(arguments);
        if (args.length > 1) { comment = args.shift(); value = args.shift(); }
        _assert(comment, !value);
    };


    //
    // Usage:
    //   assertThrows(DivisionByZeroError, MyMath.div, 1, 0);
    //   assertThrows(Error, function() { /* logic here... */ });
    //   assertThrows(Error, obj.method.bind(obj), arg0, arg1);
    //
    // NOTE: the second argument will be called with the global
    // object as 'this', so using 'obj.method' will probably not
    // do what you think. Use a function() wrapper or use
    // obj.method.bind(obj) instead.
    //
    assertThrows = function (type, func, arg0, arg1, arg2) {
        var comment, args = [].slice.call(arguments);

        if (typeof args[0] === 'string') {
            comment = args.shift();
            type = args.shift();
            func = args.shift();
        }
        else {
            type = args.shift();
            func = args.shift();
        }

        if (typeof func !== 'function') {
            throw new AssertionError('assertThrows argument not callable');
        }

        comment = comment || "";

        try {
            func.apply(null, args);
        }
        catch (e) {
            if (type) {
                _assert(comment + " Expected to throw: " + type.toString() + " saw: " + e, e instanceof type);
            }
            else {
                _assert(comment, true);
            }
            return;
        }

        _assert(comment + " Expected to throw: " + type.toString(), false);
    };


    fail = function (comment) {
        _assert(comment, false);
    };

    function htmlEscape(string) {
        return string.toString().replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    }

    runTests = function (name, expected, tests) {

        var div = document.createElement('div');
        div.className = 'test_set';
        document.documentElement.appendChild(div);

        div.innerHTML += '<div class="test_title">' + htmlEscape(name) + '</div>'

        test_count = 0;
        failure_count = 0;
        assertion_count = 0;

        // Run tests
        var keys = Object.keys(tests);
        for (var i = 0; i < keys.length; i += 1) {
            var key = keys[i];
            var test = tests[key];

            try {
                test_count += 1;
                test();
                div.innerHTML += '.';
            }
            catch (e) {
                failure_count += 1;
                div.innerHTML += 
                    '<div class="test_failure">' +
                    htmlEscape(key) + " - " + htmlEscape(e.name) + " - " + htmlEscape(e.message) +
                    '</div>';
            }
        }

        // Summary
        div.innerHTML += '<hr>';
        div.innerHTML += '<div class="test_summary">Tests: ' + test_count + ' (' + assertion_count + ' assertions) </div>';
        div.innerHTML += '<div class="test_summary">Failures: ' + failure_count + '</div>';
        if (assertion_count !== expected) {
            div.innerHTML += '<div class="test_failure">Expected ' + expected 
                + ' assertions, saw: ' + assertion_count + '</div>';
        }

        if (assertion_count === expected && failure_count === 0) {
            div.innerHTML += '<div class="test_success">All tests passed</div>';
        }
    };


} ());
