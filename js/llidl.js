/*
$LicenseInfo:firstyear=2010&license=mit$

Copyright (c) 2010, Linden Research, Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
$/LicenseInfo$
*/

// Based on the ABNF in http://tools.ietf.org/html/draft-hamrick-vwrap-type-system-00

// LLIDL Validation for ECMAScript
// Joshua Bell
// Linden Research, Inc.

// Usage:
//
//     var suite = LLIDL.parse_suite(definitions); // Throws if invalid
//     suite.match_request(name, value); // boolean - request matches exactly
//     suite.match_response(name, value); // boolean - response matches exactly
//     suite.valid_request(name, value); // boolean - request matches or has additional data
//     suite.valid_response(name, value); // boolean - response matches or has additional data
//
//     var value = LLIDL.parse_value(spec); // Throws if invalid
//     value.match(actual); // boolean - matches exactly
//     value.valid(actual); // boolean - matches or has additional data
//     value.has_additional(actual); // boolean - matches and has additional data
//     value.incompatible(actual); // boolean - incompatible
//
//     All of the value.XXX(actual) methods take a second optional parameter
//     which is an array of variant definitions, which can be parsed with:
//     var vardefs = [ LLIDL.parse_variantdef(vd1), LLIDL.parse_variantdef(vd2) ]
//     This exists primarily for unit testing.

//
// Example:
//   <script type="text/javascript" src=".../llsd.js"></script>
//   <script type="text/javascript" src=".../llidl.js"></script>
//   <script type="text/javascript">
//       try {
//           var llidl = "%% uuidToString -> uuid <- string";
//           var suite = LLIDL.parse_suite(llidl);
//
//           var success = suite.valid_request( "ff9a71eb-7414-4bf8-866e-a70ddeb7c3ce" )
//                      && suite.valid_response( "hello world!" );
//       }
//       catch (e) {
//           if (e instanceof LLIDLException) {
//               window.alert("LLIDL parsing error: " + e.message
//           }
//       }
//   </script>

var LLSD, URI, UUID, Binary; // llsd.js

// To show unit test results, insert this into HTML page:
//   <script type="text/javascript">LLIDL.Test();</script>


// Export a global LLIDLException type that callers can catch
var LLIDLException;
if (!LLIDLException) {
    LLIDLException = function(message) {
        this.name = 'LLIDLException';
        this.message = message;
    };
    LLIDLException.prototype = new Error();
}

var LLIDL;
if (!LLIDL) { (function () {
    "use strict";

    LLIDL = {};

    ////////////////////////////////////////////////////////////
    //
    // Utilities
    //
    ////////////////////////////////////////////////////////////

    // Pass-through function to throw exception if something is awry
    function required(o, e) { if (!o) { throw new LLIDLException(e); } return o; }

    ////////////////////////////////////////////////////////////
    //
    // Constants
    //
    ////////////////////////////////////////////////////////////


    var MATCHED = 4,
        CONVERTED = 3,
        DEFAULTED = 2.2,
        ADDITIONAL = 2.1,
        MIXED = 1,
        INCOMPATIBLE = 0;


    ////////////////////////////////////////////////////////////
    //
    // Parser
    //
    ////////////////////////////////////////////////////////////

    function Parser(s) {
        this.string = s;
    }

    Parser.prototype.eof = function () {
        return (this.string.length === 0);
    };

    Parser.prototype.match = function () {
        var i, opt;
        for (i = 0; i < arguments.length; i += 1) {
            opt = arguments[i];
            if (this.string.length >= opt.length && this.string.substring(0, opt.length) === opt) {
                this.string = this.string.substring(opt.length);
                return opt;
            }
        }
        return false;
    };

    Parser.prototype.matchRegex = function (pattern) {
        var re = new RegExp("^" + pattern),
            m = this.string.match(re);
        if (m) {
            this.string = this.string.substring(m[0].length);
            return m[0];
        }
        return false;
    };

    ////////////////////////////////////////////////////////////
    //
    // Terminals
    //
    ////////////////////////////////////////////////////////////

    // newline         = lf / cr / (cr lf)
    Parser.prototype.parse_newline = function () {
        return this.match('\r\n', '\r', '\n');
    };

    // comment         = ";" *char newline
    Parser.prototype.parse_comment = function () {
        if (!this.match(';')) { return; }
        var comment = "", c;
        while (true) {
            // ECMAScript strings are UTF-16; to match U+10000-U+10FFFF
            // need to match the UTF-16 surrogate pair encoding
            c = this.matchRegex("[\\u0009\\u0020-\\uD7FF\\uE000-\\uFFFD]|[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]");
            if (!c) { break; }
            comment += c;
        }
        required(this.parse_newline(), "expected newline");
        return comment;
    };

    // s               = *( tab / newline / sp / comment )
    Parser.prototype.parse_s = function () {
        var s = "", c;
        while (!this.eof()) {
            c = this.match('\t') ||
                this.parse_newline() ||
                this.match(' ') ||
                this.parse_comment();

            if (c) {
                s += c;
            }
            else {
                break;
            }
        }
        return s;
    };

    // name            = name_start *name_continue
    // name_start      = id_start    / "_"
    // name_continue   = id_continue / "_" / "/"
    // id_start        = %x0041-005A / %x0061-007A ; ALPHA
    // id_continue     = id_start / %x0030-0039    ; DIGIT
    Parser.prototype.parse_name = function () {
        return this.matchRegex("[A-Za-z_][A-Za-z0-9_/]*");
    };

    ////////////////////////////////////////////////////////////
    //
    // LLIDL Value Types
    //
    ////////////////////////////////////////////////////////////

    //
    // The following block contains:
    //
    // * Parser methods forming a recursive descent parser
    //   for LLIDL. These are roughly of the form:
    //     var res = parser.parse_<TypeName>()
    //   Returns a <TypeName>Matcher object
    //
    // * Class definitions for various types, which expose:
    //     <TypeName>Matcher.compare(data, variantdefs)
    //   This recursively validates the data against the
    //   IDL specification. (VariantMatcher definitions are
    //   necessary as LLIDL may specify named variants.)
    //

    // value           =  type / array / map / selector / variant
    function ValueMatcher() { }

    ValueMatcher.prototype.valid = function (value, variants) {
        return this.compare(value, variants) > INCOMPATIBLE;
    };
    ValueMatcher.prototype.match = function (value, variants) {
        return this.compare(value, variants) > ADDITIONAL;
    };
    ValueMatcher.prototype.has_additional = function (value, variants) {
        var result = this.compare(value, variants);
        return MIXED <= result && result <= ADDITIONAL;
    };
    ValueMatcher.prototype.incompatible = function (value, variants) {
        return this.compare(value, variants) === INCOMPATIBLE;
    };

    Parser.prototype.parse_value = function () {
        var v;
        v = this.parse_type();
        if (v) { return v; }
        v = this.parse_array();
        if (v) { return v; }
        v = this.parse_map();
        if (v) { return v; }
        v = this.parse_selector();
        if (v) { return v; }
        v = this.parse_variant();
        if (v) { return v; }

        return false;
    };

    // type            =  "undef"
    // type            =/ "string"
    // type            =/ "bool"
    // type            =/ "int"
    // type            =/ "real"
    // type            =/ "date"
    // type            =/ "uri"
    // type            =/ "uuid"
    // type            =/ "binary"
    function TypeMatcher(t) { this.t = t; }
    TypeMatcher.prototype = new ValueMatcher();
    TypeMatcher.prototype.toString = function () { return "[type: " + this.t + "]"; };
    Parser.prototype.parse_type = function () {
        var m = this.match("undef", "string", "bool", "int", "real", "date", "uri", "uuid", "binary");
        if (m) {
            return new TypeMatcher(m);
        }
        return false;
    };
    TypeMatcher.prototype.compare = function (value, variants) {
        switch (this.t) {
            case "undef": return MATCHED;

            case "string":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'boolean': return CONVERTED;
                    case 'integer': return CONVERTED;
                    case 'real': return CONVERTED;
                    case 'string': return MATCHED;
                    case 'date': return CONVERTED;
                    case 'uri': return CONVERTED;
                    case 'uuid': return CONVERTED;
                    case 'binary': return CONVERTED;
                }
                return INCOMPATIBLE;

            case "bool":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'boolean': return MATCHED;
                    case 'integer': return value === 0 || value === 1 ? CONVERTED : INCOMPATIBLE;
                    case 'real': return value === 0.0 || value === 1.0 ? CONVERTED : INCOMPATIBLE;
                    case 'string': return value === "" || value === "true" ? CONVERTED : INCOMPATIBLE;
                }
                return INCOMPATIBLE;

            case "int":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'boolean': return CONVERTED;
                    case 'integer': return MATCHED;
                    case 'real': return LLSD.asInteger(value) === value ? CONVERTED : INCOMPATIBLE;
                    case 'string':
                        if (value === "") { return DEFAULTED; }
                        value = LLSD.parseFloat(value);
                        return LLSD.asInteger(value) === value ? CONVERTED : INCOMPATIBLE;
                }
                return INCOMPATIBLE;

            case "real":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'boolean': return CONVERTED;
                    case 'integer': return CONVERTED;
                    case 'real': return MATCHED;
                    case 'string': return value === "" ? DEFAULTED : typeof LLSD.parseFloat(value) === 'number' ? CONVERTED : INCOMPATIBLE;
                }
                return INCOMPATIBLE;

            case "date":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'string': try { return value === "" ? DEFAULTED : LLSD.parseISODate(value) ? CONVERTED : INCOMPATIBLE; } catch (e1) { } return INCOMPATIBLE;
                    case 'date': return MATCHED;
                }
                return INCOMPATIBLE;

            case "uri":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'string': try { return value === "" ? DEFAULTED : new URI(value) ? CONVERTED : INCOMPATIBLE; } catch (e2) { } return INCOMPATIBLE;
                    case 'uri': return MATCHED;
                }
                return INCOMPATIBLE;

            case "uuid":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'string': try { return value === "" ? DEFAULTED : new UUID(value) ? CONVERTED : INCOMPATIBLE; } catch (e3) { } return INCOMPATIBLE;
                    case 'uuid': return MATCHED;
                }
                return INCOMPATIBLE;

            case "binary":
                switch (LLSD.type(value)) {
                    case 'undefined': return DEFAULTED;
                    case 'string': try { return new Binary(value, 'BASE64') ? CONVERTED : INCOMPATIBLE; } catch (e4) { } return INCOMPATIBLE;
                    case 'binary': return MATCHED;
                }
                return INCOMPATIBLE;
        }
    };


    // array           =  "[" s value-list s "]"
    // array           =/ "[" s value-list s "..." s "]"
    // value-list      = value [ s "," [ s value-list ] ]
    function ArrayMatcher() { this.list = []; this.repeats = false; }
    ArrayMatcher.prototype = new ValueMatcher();
    ArrayMatcher.prototype.toString = function () { return "[array: " + this.list.join(", ") + (this.repeats ? "..." : "") + "]"; };
    Parser.prototype.parse_array = function () {
        var a, value;

        if (!this.match("[")) {
            return false;
        }
        a = new ArrayMatcher();
        this.parse_s(); // s

        // value-list
        value = required(this.parse_value(), "empty array");

        while (true) { // [ s "," [ s value-list ] ]
            a.list.push(value);
            this.parse_s();
            if (!this.match(",")) { break; }

            this.parse_s();
            value = this.parse_value();
            if (!value) { break; }
        }

        this.parse_s();

        if (this.match("...")) {
            a.repeats = true;
        }

        this.parse_s();

        required(this.match("]"), "expected close bracket");

        return a;
    };
    ArrayMatcher.prototype.compare = function (value, variants) {

        if (LLSD.type(value) === 'undefined') {
            value = [];
        }

        // See if required types are present
        if (LLSD.type(value) !== 'array') {
            return INCOMPATIBLE;
        }

        var result = MATCHED,
            max = Math.max(value.length, this.list.length),
            i, j;

        for (i = 0; i < max; i += 1) {
            if (!this.repeats && i >= this.list.length) {
                // more than permitted members
                result = Math.min(ADDITIONAL, result);
                break;
            }
            else {

                j = i % this.list.length; // offset within LLIDL list
                if (i >= value.length) {
                    result = Math.min(this.list[j].compare(null), result);
                }
                else {
                    result = Math.min(this.list[j].compare(value[i]), result);
                }
            }
        }

        return result;
    };


    // map             =  "{" s member-list s "}"
    // map             =/ "{" s "$" s ":" s value s "}"
    // member-list     = member [ s "," [ s member-list ] ]
    // member          = name s ":" s value
    function MapMatcher() { this.members = {}; }
    MapMatcher.prototype = new ValueMatcher();
    MapMatcher.prototype.toString = function () { return "[map: " + this.members + "]"; };
    Parser.prototype.parse_map = function () {
        if (!this.match("{")) {
            return false;
        }
        var m = new MapMatcher(),
            name, value;

        this.parse_s();

        if (this.match("$")) {
            this.parse_s();
            required(this.match(':'), "expected colon");
            this.parse_s();
            value = required(this.parse_value(), "expected value");
            m.members.$ = value;
        }
        else {
            // member-list

            name = required(this.parse_name(), "empty map");

            while (true) {
                this.parse_s();
                required(this.match(':'), "expected colon");
                this.parse_s();
                value = required(this.parse_value(), "expected value");
                m.members[name] = value;

                this.parse_s();
                if (!this.match(",")) { break; }
                this.parse_s();

                name = this.parse_name();
                if (!name) { break; }
            }
        }

        this.parse_s();
        required(this.match("}"), "expected close bracket");

        return m;
    };
    MapMatcher.prototype.compare = function (value, variants) {

        if (LLSD.type(value) === 'undefined') {
            value = {};
        }

        if (LLSD.type(value) !== 'map') {
            return INCOMPATIBLE;
        }

        var req, val, key, name,
            result = MATCHED;

        if (this.members.$) {
            // { $ : value } - all map values must conform
            req = this.members.$;
            for (key in value) {
                if (value.hasOwnProperty(key)) {
                    val = value[key];
                    result = Math.min(req.compare(val, variants), result);
                }
            }
        }
        else {
            // Require all named members
            for (name in this.members) {
                if (this.members.hasOwnProperty(name)) {

                    if (value.hasOwnProperty(name)) {
                        req = this.members[name];
                        val = value[name];
                        result = Math.min(req.compare(val, variants), result);
                    }
                    else {
                        result = Math.min(DEFAULTED, result);
                    }
                }
            }
            for (name in value) {
                if (value.hasOwnProperty(name)) {
                    if (!this.members.hasOwnProperty(name)) {
                        result = Math.min(ADDITIONAL, result);
                    }
                }
            }
        }

        return result;
    };


    // selector        =  quote name quote
    // selector        =/ "true" / "false"
    // selector        =/ 1*digit
    function SelectorMatcher(value) { this.value = value; }
    SelectorMatcher.prototype = new ValueMatcher();
    SelectorMatcher.prototype.toString = function () { return "[selector: " + this.value + "]"; };
    Parser.prototype.parse_selector = function () {

        var s;
        if (this.match('"')) {
            s = required(this.parse_name(), "expected name in quotes");
            required(this.match('"'), "expected close quote");
            return new SelectorMatcher(s);
        }

        s = this.match("true", "false");
        if (s) { return new SelectorMatcher(s === "true"); }

        s = this.matchRegex("[0-9]+");
        if (s) { return new SelectorMatcher(parseInt(s, 10)); }

        return false;
    };
    SelectorMatcher.prototype.compare = function (value, variants) {
        // See if value is an exact match
        switch (LLSD.type(this.value)) {
            case 'string':
                switch (LLSD.type(value)) {
                    case 'undefined': return this.value === "" ? DEFAULTED : INCOMPATIBLE;
                    case 'string': return this.value === value ? MATCHED : INCOMPATIBLE;
                }
                return INCOMPATIBLE;

            case 'boolean':
                switch (LLSD.type(value)) {
                    case 'undefined': return this.value ? INCOMPATIBLE : DEFAULTED;
                    case 'boolean': return this.value === value ? MATCHED : INCOMPATIBLE;
                    case 'integer': return (this.value ? 1 : 0) === value ? CONVERTED : INCOMPATIBLE;
                    case 'real': return (this.value ? 1.0 : 0.0) === value ? CONVERTED : INCOMPATIBLE;
                    case 'string': return (this.value ? "true" : "") === value ? CONVERTED : INCOMPATIBLE;
                }
                return INCOMPATIBLE;

            case 'integer':
                switch (LLSD.type(value)) {
                    case 'undefined': return this.value === 0 ? DEFAULTED : INCOMPATIBLE;
                    case 'boolean': return this.value === (value ? 1 : 0) ? CONVERTED : INCOMPATIBLE;
                    case 'integer': return this.value === value ? MATCHED : INCOMPATIBLE;
                    case 'real': return this.value === LLSD.asInteger(value) ? CONVERTED : INCOMPATIBLE;
                    case 'string':
                        if (value === "") { return this.value === 0 ? DEFAULTED : INCOMPATIBLE; }
                        value = LLSD.parseFloat(value);
                        return this.value === LLSD.asInteger(value) ? CONVERTED : INCOMPATIBLE;
                }
                return INCOMPATIBLE;
        }
    };


    // variant         = "&" name
    function VariantMatcher(name) { this.name = name; }
    VariantMatcher.prototype = new ValueMatcher();
    VariantMatcher.prototype.toString = function () { return "[variant: " + this.name + "]"; };
    Parser.prototype.parse_variant = function () {
        if (!this.match("&")) { return false; }
        var n = required(this.parse_name(), "expected variant name");
        return new VariantMatcher(n);
    };
    VariantMatcher.prototype.compare = function (value, variants) {

        var result = INCOMPATIBLE,
            i, vd;

        // loop over possible variant definitions and pick the best match
        for (i = 0; i < variants.length; i += 1) {
            vd = variants[i];
            if (this.name === vd.name) {
                result = Math.max(vd.value.compare(value, variants), result);
            }
        }

        return result;
    };

    ////////////////////////////////////////////////////////////
    //
    // LLIDL Resource Types
    //
    ////////////////////////////////////////////////////////////

    //
    // The following block contains:
    //
    // * Parser methods forming a recursive descent parser
    //   for LLIDL. These are roughly of the form:
    //     var res = parser.parse_<ResourceType>()
    //   Returns a <ResourceType> object
    //
    // * Class definitions for various types, which expose:
    //     <ResourceType>.valid_request(data, variantdefs)
    //     <ResourceType>.valid_response(data, variantdefs)
    //

    // variant-def     = "&" name s "=" s value
    function VariantDef(name, value) { this.name = name; this.value = value; }
    VariantDef.prototype.toString = function () { return "[variant-def " + this.name + " = " + this.value + "]"; };
    Parser.prototype.parse_variantdef = function () {
        var name, value;

        if (!this.match("&")) { return false; }
        name = required(this.parse_name(), "expected variant name");
        this.parse_s();
        required(this.match("="), "expected equals sign");
        this.parse_s();
        value = required(this.parse_value(), "expected variant value");
        return new VariantDef(name, value);
    };

    // res-name        = "%%" s name
    Parser.prototype.parse_resname = function () {
        if (!this.match("%%")) { return false; }
        this.parse_s();
        return required(this.parse_name(), "expected resource name");
    };

    // res-get         = "<<" s value
    function ResourceGet(value) { this.value = value; }
    ResourceGet.prototype.toString = function () { return "[res-get << " + this.value + "]"; };
    Parser.prototype.parse_resget = function () {
        if (!this.match("<<")) { return false; }
        this.parse_s();
        var value = required(this.parse_value(), "expected value");
        return new ResourceGet(value);
    };
    ResourceGet.prototype.compare_request = function (request, variants) {
        return INCOMPATIBLE;
    };
    ResourceGet.prototype.compare_response = function (response, variants) {
        return this.value.compare(response, variants);
    };

    // res-getput      = "<>" s value
    function ResourceGetPut(value) { this.value = value; }
    ResourceGetPut.prototype.toString = function () { return "[res-getput <> " + this.value + "]"; };
    Parser.prototype.parse_resgetput = function () {
        if (!this.match("<>")) { return false; }
        this.parse_s();
        var value = required(this.parse_value(), "expected value");
        return new ResourceGetPut(value);
    };
    ResourceGetPut.prototype.compare_request = function (request, variants) {
        return this.value.compare(request, variants);
    };
    ResourceGetPut.prototype.compare_response = function (response, variants) {
        return this.value.compare(response, variants);
    };

    // res-getputdel   = "<x>" s value
    function ResourceGetPutDel(value) { this.value = value; }
    ResourceGetPutDel.prototype.toString = function () { return "[res-getputdel <x> " + this.value + "]"; };
    Parser.prototype.parse_resgetputdel = function () {
        if (!this.match("<x>")) { return false; }
        this.parse_s();
        var value = required(this.parse_value(), "expected value");
        return new ResourceGetPutDel(value);
    };
    ResourceGetPutDel.prototype.compare_request = function (request, variants) {
        return this.value.compare(request, variants);
    };
    ResourceGetPutDel.prototype.compare_response = function (response, variants) {
        return this.value.compare(response, variants);
    };

    // res-post        = res-request s res-response
    function ResourcePost(request, response) { this.request = request; this.response = response; }
    ResourcePost.prototype.toString = function () { return "[res-post -> " + this.request + " <- " + this.response + "]"; };
    Parser.prototype.parse_respost = function () {
        var request, response;
        request = this.parse_resrequest();
        if (!request) { return false; }

        this.parse_s();
        response = required(this.parse_resresponse(), "expected res-response");
        return new ResourcePost(request, response);
    };
    ResourcePost.prototype.compare_request = function (request, variants) {
        return this.request.compare(request, variants);
    };
    ResourcePost.prototype.compare_response = function (response, variants) {
        return this.response.compare(response, variants);
    };

    // res-request     = "->" s value
    Parser.prototype.parse_resrequest = function () {
        if (!this.match("->")) { return false; }
        this.parse_s();
        return required(this.parse_value(), "expected value");
    };

    // res-response    = "<-" s value
    Parser.prototype.parse_resresponse = function () {
        if (!this.match("<-")) { return false; }
        this.parse_s();
        return required(this.parse_value(), "expected value");
    };

    // res-transaction = res-get / res-getput / res-getputdel / res-post
    Parser.prototype.parse_restransaction = function () {
        var t;
        t = this.parse_resget();
        if (t) { return t; }
        t = this.parse_resgetput();
        if (t) { return t; }
        t = this.parse_resgetputdel();
        if (t) { return t; }
        t = this.parse_respost();
        if (t) { return t; }
        return false;
    };

    // resource-def    = res-name s res-transaction
    function ResourceDef(name, transaction) { this.name = name; this.transaction = transaction; }
    ResourceDef.prototype.toString = function () { return "[resource-def " + this.name + " " + this.transaction + "]"; };
    Parser.prototype.parse_resourcedef = function () {
        var name, transaction;
        name = this.parse_resname();
        if (!name) { return false; }

        this.parse_s();
        transaction = required(this.parse_restransaction(), "expected transaction");
        return new ResourceDef(name, transaction);
    };
    ResourceDef.prototype.compare_request = function (request, variants) {
        return this.transaction.compare_request(request, variants);
    };
    ResourceDef.prototype.compare_response = function (response, variants) {
        return this.transaction.compare_response(response, variants);
    };

    // definitions     = *( s / variant-def / resource-def )
    function Suite() { this.variantdefs = []; this.resourcedefs = {}; }
    Parser.prototype.parse_definitions = function () {
        /*jslint continue: true*/
        var defs = new Suite(),
            d;

        while (!this.eof()) {
            if (this.parse_s()) { continue; }

            d = this.parse_variantdef();
            if (d) { defs.variantdefs.push(d); continue; }

            d = this.parse_resourcedef();
            if (d) { defs.resourcedefs[d.name] = d; continue; }
            
            if (!d) { break; }
        }

        required(this.eof(), "expected end of input");

        return defs;
    };
    Suite.prototype.get_rd = function (name) {
        return required(this.resourcedefs[name], "No matching resource definition found");
    };
    Suite.prototype.valid_request = function (name, value) {
        return this.get_rd(name).compare_request(value, this.variantdefs) > INCOMPATIBLE;
    };
    Suite.prototype.valid_response = function (name, value) {
        return this.get_rd(name).compare_response(value, this.variantdefs) > INCOMPATIBLE;
    };
    Suite.prototype.match_request = function (name, value) {
        return this.get_rd(name).compare_request(value, this.variantdefs) > ADDITIONAL;
    };
    Suite.prototype.match_response = function (name, value) {
        return this.get_rd(name).compare_response(value, this.variantdefs) > ADDITIONAL;
    };


    ////////////////////////////////////////////////////////////
    //
    // Functions
    //
    ////////////////////////////////////////////////////////////

    // Expose to the world beyond the closure

    if (typeof LLIDL.parse_suite !== 'function') {
        LLIDL.parse_suite = function (llidl) {
            var parser = new Parser(llidl),
                suite = parser.parse_definitions();
            if (!suite) { throw new LLIDLException("expected suite"); }
            if (!parser.eof()) { throw new LLIDLException("expected end of input"); }
            return suite;
        };
    }

    if (typeof LLIDL.parse_value !== 'function') {
        LLIDL.parse_value = function (llidl) {
            var parser = new Parser(llidl),
                value = parser.parse_value();
            if (!value) { throw new LLIDLException("expected value"); }
            if (!parser.eof()) { throw new LLIDLException("expected end of input"); }
            return value;
        };
    }

    if (typeof LLIDL.parse_variantdef !== 'function') {
        LLIDL.parse_variantdef = function (llidl) {
            var parser = new Parser(llidl),
                vardef = parser.parse_variantdef();
            if (!vardef) { throw new LLIDLException("expected variant definition"); }
            if (!parser.eof()) { throw new LLIDLException("expected end of input"); }
            return vardef;
        };
    }

} ()); }

