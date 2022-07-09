#!/usr/bin/perl -w

# @file 01.new_types.t
# @brief Unit tests for LLSD type creation and conversions
#
# $LicenseInfo:firstyear=2010&license=mit$
#
# Copyright (c) 2010, Linden Research, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# $/LicenseInfo$


use strict;
use warnings;
use Test::More tests => 97;

use lib qw( ./lib/ . );
BEGIN { use_ok( 'LLSD' ); }


#--------------------------------
# Creation tests
#--------------------------------

ok(new LLSD::Boolean('true'), 'boolean creation: from "true"');
ok(new LLSD::Boolean('bollocks'), 'boolean creation: from "bollocks"');
ok(!new LLSD::Boolean('false'), 'boolean creation: from "false"');
ok(!new LLSD::Boolean('0'), 'boolean creation: from "0"');

is(new LLSD::Integer(0 + 'nan'), 0, 'integer creation: from NaN');


#--------------------------------
# Constants tests
#--------------------------------

ok(LLSD::True, 'True constant');
ok(!LLSD::False, 'False constant');
# This test won't work, alas
#ok(!defined LLSD::Undefined, 'Undefined constant');

is(new LLSD::Boolean(1), LLSD::True, 'true is true');
is(new LLSD::Boolean(0), LLSD::False, 'false is false');
ok(new LLSD::Boolean('bollocks'), 'boolean creation: from "bollocks"');
ok(!new LLSD::Boolean('false'), 'boolean creation: from "false"');

is(LLSD::UUID::Nil, '00000000-0000-0000-0000-000000000000', 'nil uuid');
is(LLSD::UUID::Nil, new LLSD::UUID('00000000-0000-0000-0000-000000000000'), 'nil uuid');

#--------------------------------
# Conversion tests
#--------------------------------
# NOTE: Type conversions are dictated in the LLSD Internet Draft. These
# conversions do not necessarily match the construction rules, which can
# be implementation specific, e.g. to match native "truthiness".
#--------------------------------

# 2.1.2.  Boolean
ok(new LLSD::Integer('123')->asBoolean(), 'boolean conversion: from integer');
ok(!new LLSD::Integer('0')->asBoolean(), 'boolean conversion: from integer');

ok(new LLSD::Real(123.45)->asBoolean(), 'boolean conversion: from real');
ok(new LLSD::Real(-12e34)->asBoolean(), 'boolean conversion: from real');
ok(!new LLSD::Real(0)->asBoolean(), 'boolean conversion: from real');
ok(!new LLSD::Real('NaN')->asBoolean(), 'boolean conversion: from real');
ok(new LLSD::Real('Inf')->asBoolean(), 'boolean conversion: from real');

ok(new LLSD::String('true')->asBoolean(), 'boolean conversion: from string');
#ok(!new LLSD::String('false')->asBoolean(), 'boolean conversion: from string');
ok(!new LLSD::String('')->asBoolean(), 'boolean conversion: from string');

# 2.1.3.  Integer

is(LLSD::True->asInteger(), 1, 'integer conversion: from boolean');
is(LLSD::False->asInteger(), 0, 'integer conversion: from boolean');

is(new LLSD::Real(123)->asInteger(), 123, 'integer conversion: from real');
is(new LLSD::Real(-456)->asInteger(), -456, 'integer conversion: from real');
is(new LLSD::Real(123.45)->asInteger(), 123, 'integer conversion: from real');
is(new LLSD::Real(-12e34)->asInteger(), -2147483648, 'integer conversion: from real');
is(new LLSD::Real(12e34)->asInteger(), 2147483647, 'integer conversion: from real');
is(new LLSD::Real(0)->asInteger(), 0, 'integer conversion: from real');
is(new LLSD::Real('NaN')->asInteger(), 0, 'integer conversion: from real');
is(new LLSD::Real('Inf')->asInteger(), 2147483647, 'integer conversion: from real');

is(new LLSD::String('123')->asInteger(), 123, 'integer conversion: from string');
is(new LLSD::String('-456')->asInteger(), -456, 'integer conversion: from string');
is(new LLSD::String('123.45')->asInteger(), 123, 'integer conversion: from string');
is(new LLSD::String('-12e34')->asInteger(), -2147483648, 'integer conversion: from string');
is(new LLSD::String('12e34')->asInteger(), 2147483647, 'integer conversion: from string');
is(new LLSD::String('0')->asInteger(), 0, 'integer conversion: from string');
is(new LLSD::String('NaN')->asInteger(), 0, 'integer conversion: from string');
is(new LLSD::String('Inf')->asInteger(), 2147483647, 'integer conversion: from string');

# 2.1.4.  Real

is(LLSD::True->asReal(), 1, 'real conversion: from boolean');
is(LLSD::False->asReal(), 0, 'real conversion: from boolean');

is(new LLSD::Integer(123)->asReal(), 123, 'real conversion: from integer');
is(new LLSD::Integer(-456)->asReal(), -456, 'real conversion: from integer');
is(new LLSD::Integer(0)->asReal(), 0, 'real conversion: from integer');
is(new LLSD::Integer('NaN')->asReal(), 0, 'real conversion: from integer');
is(new LLSD::Integer('Inf')->asReal(), 2147483647, 'real conversion: from integer');

is(new LLSD::String('123')->asReal(), 123, 'real conversion: from string');
is(new LLSD::String('-456')->asReal(), -456, 'real conversion: from string');
is(new LLSD::String('123.45')->asReal(), 123.45, 'real conversion: from string');
is(new LLSD::String('-12e34')->asReal(), -12.e34, 'real conversion: from string');
is(new LLSD::String('12e34')->asReal(), 12e34, 'real conversion: from string');
is(new LLSD::String('0')->asReal(), 0, 'real conversion: from string');
is(new LLSD::String('NaN')->asReal(), 0 + 'nan', 'real conversion: from string');
is(new LLSD::String('Inf')->asReal(), 0 + 'inf', 'real conversion: from string');
is(new LLSD::String('-Inf')->asReal(), 0 - 'inf', 'real conversion: from string');


# 2.1.5.  String

is(LLSD::True->asString(), 'true', 'string conversion: from boolean');
is(LLSD::False->asString(), '', 'string conversion: from boolean');

is(new LLSD::Integer(123)->asString(), '123', 'string conversion: from integer');
is(new LLSD::Integer(-456)->asString(), '-456', 'string conversion: from integer');
is(new LLSD::Integer(0)->asString(), '0', 'string conversion: from integer');
is(new LLSD::Integer('NaN')->asString(), '0', 'string conversion: from integer');
is(new LLSD::Integer('Inf')->asString(), '2147483647', 'string conversion: from integer');

is(new LLSD::Real(123)->asString(), '123', 'string conversion: from real');
is(new LLSD::Real(-456)->asString(), '-456', 'string conversion: from real');
is(new LLSD::Real(123.45)->asString(), '123.45', 'string conversion: from real');
is(new LLSD::Real(-12e34)->asString(), '-1.2e+35', 'string conversion: from real');
is(new LLSD::Real(12e34)->asString(), '1.2e+35', 'string conversion: from real');
is(new LLSD::Real(0)->asString(), '0', 'string conversion: from real');
is(new LLSD::Real(-0.0)->asString(), '-0.0', 'string conversion: from real');
is(new LLSD::Real('NaN')->asString(), 'NaN', 'string conversion: from real');
is(new LLSD::Real('Inf')->asString(), 'Infinity', 'string conversion: from real');
is(new LLSD::Real('-Inf')->asString(), '-Infinity', 'string conversion: from real');

is(new LLSD::UUID('FF9A71EB-7414-4BF8-866E-A70DDEB7C3CF')->asString(),
            'ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf', 'string conversion: from uuid');
is(new LLSD::UUID()->asString(),
            '00000000-0000-0000-0000-000000000000', 'string conversion: from uuid');

is(new LLSD::Date('1974-04-04T12:34:56.789Z')->asString(),
            '1974-04-04T12:34:56.789Z', 'string conversion: from date');
is(new LLSD::Date()->asString(), '1970-01-01T00:00:00.000Z', 'string conversion: from date');

is(new LLSD::URI('http://example.org/foo/bar')->asString(),
            'http://example.org/foo/bar', 'string conversion: from URI');
is(new LLSD::URI('urn:example:foo:bar')->asString(),
            'urn:example:foo:bar', 'string conversion: from URI');
is(new LLSD::URI()->asString(), '', 'string conversion: from URI');

is(new LLSD::Binary("\x01\x02\x03\x04")->asString(), 'AQIDBA==', 'binary conversion: from binary');

# 2.1.6.  UUID
my $null_uuid = new LLSD::UUID('00000000-0000-0000-0000-000000000000');
is(new LLSD::String('')->asUUID(), $null_uuid, 'uuid conversion: from string');
is(new LLSD::String('abc')->asUUID(), $null_uuid, 'uuid conversion: from string');
is(new LLSD::String('FF9A71EB-7414-4BF8-866E-A70DDEB7C3CF')->asUUID(),
            new LLSD::UUID('ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf'), 'uuid conversion: from string');

# 2.1.7.  Date
my $epoch = new LLSD::Date('1970-01-01T00:00:00.000Z');
is(new LLSD::String('')->asDate(), 0, 'date conversion: from string');
is(new LLSD::String('abc')->asDate(), 0, 'date conversion: from string');
is(new LLSD::String('1974-04-04T12:34:56.789Z')->asDate(),
            134310896.789, 'date conversion: from string');

# 2.1.8.  URI
my $null_uri = new LLSD::URI('');
is(new LLSD::String('')->asURI(), $null_uri, 'uri conversion: from string');
is(new LLSD::String(':foo')->asURI(), $null_uri, 'uri conversion: from string');
is(new LLSD::String('http://example.com/foo?bar')->asURI(),
            new LLSD::URI('http://example.com/foo?bar'), 'uri conversion: from string');

# 2.1.9.  Binary
is(new LLSD::String('AQIDBA==')->asBinary(), "\x01\x02\x03\x04", 'binary conversion: from string');


# Other

is(new LLSD::String('hello') . ' world', 'hello world', 'string to string conversions in expr');
is(new LLSD::Integer(1234) + new LLSD::Real(12.34), 1234 + 12.34, 'integer to number conversions in expr');
is('' . new LLSD::Date('1974-04-04T12:34:56.789Z'), '1974-04-04T12:34:56.789Z', 'date to string conversions in expr');




#--------------------------------
# Empty Subclass Tests
#--------------------------------

package Foo;
use LLSD;
our @ISA = ('LLSD::Integer');
package main;

is(LLSD::type(Foo->new), 'integer', 'LLSD::Integer empty subclass test');
is(111+Foo->new(123), 234, 'LLSD::Integer empty subclass overload test');
