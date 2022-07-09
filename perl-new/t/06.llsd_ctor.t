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
use Test::More tests => 96;

use lib qw( ./lib/ . );
BEGIN { use_ok( 'LLSD' ); }


#--------------------------------
# Creation tests
#--------------------------------

ok(new LLSD('true', 'boolean'), 'boolean creation: from "true"');
ok(new LLSD('bollocks', 'boolean'), 'boolean creation: from "bollocks"');
ok(!new LLSD('false', 'boolean'), 'boolean creation: from "false"');
ok(!new LLSD('0', 'boolean'), 'boolean creation: from "0"');

is(new LLSD(0 + 'nan', 'integer'), 0, 'integer creation: from NaN');


#--------------------------------
# Constants tests
#--------------------------------

ok(LLSD::True, 'True constant');
ok(!LLSD::False, 'False constant');

# Fails because it implicitly converts to string
#ok(!defined LLSD::Undefined, 'Undefined constant'); 
is(LLSD::Undefined->type, 'undefined', 'Undefined constant');

is(new LLSD(1, 'boolean'), LLSD::True, 'true is true');
is(new LLSD(0, 'boolean'), LLSD::False, 'false is false');
ok(new LLSD('bollocks', 'boolean'), 'boolean creation: from "bollocks"');
ok(!new LLSD('false', 'boolean'), 'boolean creation: from "false"');

#--------------------------------
# Conversion tests
#--------------------------------
# NOTE: Type conversions are dictated in the LLSD Internet Draft. These
# conversions do not necessarily match the construction rules, which can
# be implementation specific, e.g. to match native "truthiness".
#--------------------------------

# 2.1.2.  Boolean
ok(new LLSD('123', 'integer')->asBoolean(), 'boolean conversion: from integer');
ok(!new LLSD('0', 'integer')->asBoolean(), 'boolean conversion: from integer');

ok(new LLSD(123.45, 'real')->asBoolean(), 'boolean conversion: from real');
ok(new LLSD(-12e34, 'real')->asBoolean(), 'boolean conversion: from real');
ok(!new LLSD(0, 'real')->asBoolean(), 'boolean conversion: from real');
ok(!new LLSD('NaN', 'real')->asBoolean(), 'boolean conversion: from real');
ok(new LLSD('Inf', 'real')->asBoolean(), 'boolean conversion: from real');

ok(new LLSD('true', 'string')->asBoolean(), 'boolean conversion: from string');
#ok(!new LLSD('false', 'string')->asBoolean(), 'boolean conversion: from string');
ok(!new LLSD('', 'string')->asBoolean(), 'boolean conversion: from string');

# 2.1.3.  Integer

is(LLSD::True->asInteger(), 1, 'integer conversion: from boolean');
is(LLSD::False->asInteger(), 0, 'integer conversion: from boolean');

is(new LLSD(123, 'real')->asInteger(), 123, 'integer conversion: from real');
is(new LLSD(-456, 'real')->asInteger(), -456, 'integer conversion: from real');
is(new LLSD(123.45, 'real')->asInteger(), 123, 'integer conversion: from real');
is(new LLSD(-12e34, 'real')->asInteger(), -2147483648, 'integer conversion: from real');
is(new LLSD(12e34, 'real')->asInteger(), 2147483647, 'integer conversion: from real');
is(new LLSD(0, 'real')->asInteger(), 0, 'integer conversion: from real');
is(new LLSD('NaN', 'real')->asInteger(), 0, 'integer conversion: from real');
is(new LLSD('Inf', 'real')->asInteger(), 2147483647, 'integer conversion: from real');

is(new LLSD('123', 'string')->asInteger(), 123, 'integer conversion: from string');
is(new LLSD('-456', 'string')->asInteger(), -456, 'integer conversion: from string');
is(new LLSD('123.45', 'string')->asInteger(), 123, 'integer conversion: from string');
is(new LLSD('-12e34', 'string')->asInteger(), -2147483648, 'integer conversion: from string');
is(new LLSD('12e34', 'string')->asInteger(), 2147483647, 'integer conversion: from string');
is(new LLSD('0', 'string')->asInteger(), 0, 'integer conversion: from string');
is(new LLSD('NaN', 'string')->asInteger(), 0, 'integer conversion: from string');
is(new LLSD('Inf', 'string')->asInteger(), 2147483647, 'integer conversion: from string');

# 2.1.4.  Real

is(LLSD::True->asReal(), 1, 'real conversion: from boolean');
is(LLSD::False->asReal(), 0, 'real conversion: from boolean');

is(new LLSD(123, 'integer')->asReal(), 123, 'real conversion: from integer');
is(new LLSD(-456, 'integer')->asReal(), -456, 'real conversion: from integer');
is(new LLSD(0, 'integer')->asReal(), 0, 'real conversion: from integer');
is(new LLSD('NaN', 'integer')->asReal(), 0, 'real conversion: from integer');
is(new LLSD('Inf', 'integer')->asReal(), 2147483647, 'real conversion: from integer');

is(new LLSD('123', 'string')->asReal(), 123, 'real conversion: from string');
is(new LLSD('-456', 'string')->asReal(), -456, 'real conversion: from string');
is(new LLSD('123.45', 'string')->asReal(), 123.45, 'real conversion: from string');
is(new LLSD('-12e34', 'string')->asReal(), -12.e34, 'real conversion: from string');
is(new LLSD('12e34', 'string')->asReal(), 12e34, 'real conversion: from string');
is(new LLSD('0', 'string')->asReal(), 0, 'real conversion: from string');
is(new LLSD('NaN', 'string')->asReal(), 0 + 'nan', 'real conversion: from string');
is(new LLSD('Inf', 'string')->asReal(), 0 + 'inf', 'real conversion: from string');
is(new LLSD('-Inf', 'string')->asReal(), 0 - 'inf', 'real conversion: from string');


# 2.1.5.  String

is(LLSD::True->asString(), 'true', 'string conversion: from boolean');
is(LLSD::False->asString(), '', 'string conversion: from boolean');

is(new LLSD(123, 'integer')->asString(), '123', 'string conversion: from integer');
is(new LLSD(-456, 'integer')->asString(), '-456', 'string conversion: from integer');
is(new LLSD(0, 'integer')->asString(), '0', 'string conversion: from integer');
is(new LLSD('NaN', 'integer')->asString(), '0', 'string conversion: from integer');
is(new LLSD('Inf', 'integer')->asString(), '2147483647', 'string conversion: from integer');

is(new LLSD(123, 'real')->asString(), '123', 'string conversion: from real');
is(new LLSD(-456, 'real')->asString(), '-456', 'string conversion: from real');
is(new LLSD(123.45, 'real')->asString(), '123.45', 'string conversion: from real');
is(new LLSD(-12e34, 'real')->asString(), '-1.2e+35', 'string conversion: from real');
is(new LLSD(12e34, 'real')->asString(), '1.2e+35', 'string conversion: from real');
is(new LLSD(0, 'real')->asString(), '0', 'string conversion: from real');
is(new LLSD(-0.0, 'real')->asString(), '-0.0', 'string conversion: from real');
is(new LLSD('NaN', 'real')->asString(), 'NaN', 'string conversion: from real');
is(new LLSD('Inf', 'real')->asString(), 'Infinity', 'string conversion: from real');
is(new LLSD('-Inf', 'real')->asString(), '-Infinity', 'string conversion: from real');

is(new LLSD('FF9A71EB-7414-4BF8-866E-A70DDEB7C3CF', 'uuid')->asString(),
            'ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf', 'string conversion: from uuid');
is(new LLSD('', 'uuid')->asString(),
            '00000000-0000-0000-0000-000000000000', 'string conversion: from uuid');

is(new LLSD('1974-04-04T12:34:56.789Z', 'date')->asString(),
            '1974-04-04T12:34:56.789Z', 'string conversion: from date');
is(new LLSD('', 'date')->asString(), '1970-01-01T00:00:00.000Z', 'string conversion: from date');

is(new LLSD('http://example.org/foo/bar', 'uri')->asString(),
            'http://example.org/foo/bar', 'string conversion: from URI');
is(new LLSD('urn:example:foo:bar', 'uri')->asString(),
            'urn:example:foo:bar', 'string conversion: from URI');
is(new LLSD('', 'uri')->asString(), '', 'string conversion: from URI');

is(new LLSD("\x01\x02\x03\x04", 'binary')->asString(), 'AQIDBA==', 'binary conversion: from binary');

# 2.1.6.  UUID
my $null_uuid = new LLSD('00000000-0000-0000-0000-000000000000', 'uuid');
is(new LLSD('', 'string')->asUUID(), $null_uuid, 'uuid conversion: from string');
is(new LLSD('abc', 'string')->asUUID(), $null_uuid, 'uuid conversion: from string');
is(new LLSD('FF9A71EB-7414-4BF8-866E-A70DDEB7C3CF', 'string')->asUUID(),
            new LLSD('ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf', 'uuid'), 'uuid conversion: from string');

# 2.1.7.  Date
my $epoch = new LLSD('1970-01-01T00:00:00.000Z', 'date');
is(new LLSD('', 'string')->asDate(), 0, 'date conversion: from string');
is(new LLSD('abc', 'string')->asDate(), 0, 'date conversion: from string');
is(new LLSD('1974-04-04T12:34:56.789Z', 'string')->asDate(), 134310896.789, 'date conversion: from string');

# 2.1.8.  URI
my $null_uri = new LLSD('', 'uri');
is(new LLSD('', 'string')->asURI(), $null_uri, 'uri conversion: from string');
is(new LLSD(':foo', 'string')->asURI(), $null_uri, 'uri conversion: from string');
is(new LLSD('http://example.com/foo?bar', 'string')->asURI(),
            new LLSD('http://example.com/foo?bar', 'uri'), 'uri conversion: from string');

# 2.1.9.  Binary
is(new LLSD('AQIDBA==', 'string')->asBinary(), "\x01\x02\x03\x04", 'binary conversion: from string');


# Other

is(new LLSD('hello', 'string') . ' world', 'hello world', 'string to string conversions in expr');
is(new LLSD(1234, 'integer') + new LLSD(12.34, 'real'), 1234 + 12.34, 'integer to number conversions in expr');
is('' . new LLSD('1974-04-04T12:34:56.789Z', 'date'), '1974-04-04T12:34:56.789Z', 'date to string conversions in expr');




#--------------------------------
# Empty Subclass Tests
#--------------------------------

package Foo;
use LLSD;
our @ISA = ('LLSD');
package main;

is(LLSD::type(Foo->new(1, 'integer')), 'integer', 'LLSD empty subclass test');
is(111+Foo->new(123, 'integer'), 234, 'LLSD empty subclass overload test');
