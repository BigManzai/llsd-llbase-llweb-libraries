#!/usr/bin/perl -w

# @file 07.json_serialization.t
# @brief Unit tests for JSON parsing/formatting of LLSD type classes
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
use Test::More tests => 49;

use lib qw( ./lib/ . );

BEGIN { use_ok( 'LLSD' ); }


sub formatok($$$)
{
    my ($llsd, $expected, $msg) = @_;
    is_deeply($expected, LLSD::format_json($llsd), $msg);
}

sub parseok($$$)
{
    my ($json, $expected, $msg) = @_;
    is_deeply($expected, LLSD::parse_json($json), $msg);
}

sub bothok($$$)
{
    my ($llsd, $json, $message) = @_;
    parseok($json, $llsd, $message);
    formatok($llsd, $json, $message);
}

sub parsefail($$$)
{
    my ($json, $expected, $msg) = @_;

    eval {
        LLSD::parse_json($json);
    };

    like($@, $expected, $msg);
}





# Type tests

# Normal values

bothok(LLSD::Undefined, 'null', 'undefined');

bothok(LLSD::True, 'true', 'true');
bothok(LLSD::False, 'false', 'false');

bothok(new LLSD::Integer(0), '0', 'integer - zero');
bothok(new LLSD::Integer(123), '123', 'positive integer');
bothok(new LLSD::Integer(-456), '-456', 'negative integer');
bothok(new LLSD::Integer(299792458), '299792458', 'integer - speed of light');

bothok(new LLSD::Real(3.1415), '3.1415', 'real - pi');
# mismatch in least-significant bits on some platforms
#bothok(new LLSD::Real(6.0221415e+23), '', 'real - Avogadro\'s constant');
bothok(new LLSD::Real(6.6260693e-34), '6.6260693e-34', 'real - Planck\'s constant');
bothok(new LLSD::Real(6.67428e-11), '6.67428e-11', 'real - gravitational constant');
bothok(new LLSD::Real(0.0), '0', 'real - zero');

formatok(new LLSD::Real(-0.0), '-0.0', 'real - negative zero');
formatok(new LLSD::Real('inf'), '"Infinity"', 'real - positive infinity');
formatok(new LLSD::Real('-inf'), '"-Infinity"', 'real - negative infinity');
formatok(new LLSD::Real('nan'), '"NaN"', 'real - not a number');

formatok(new LLSD::String("abc"), '"abc"', 'string');

formatok([], '[]', 'empty array');
formatok([new LLSD::Integer(1)], '[1]', 'array - one element');
formatok([new LLSD::Integer(1),new LLSD::Integer(2),new LLSD::Integer(3)], '[1,2,3]', 'array - several elements');
formatok([new LLSD::Integer(1),new LLSD::String("abc"),new LLSD::Integer(2),new LLSD::String("def")], '[1,"abc",2,"def"]', 'array - heterogeneous');

bothok({}, '{}', 'empty map');
bothok({"abc" => new LLSD::Integer(1)}, '{"abc":1}', 'map - one element');
bothok({"abc" => new LLSD::Integer(1), "def" => new LLSD::String("ghi")}, '{"abc":1,"def":"ghi"}', 'map - several elements');

# Whitespace

parseok(' { "k1" : 1 , "k2" : "2" , "k3" : [ 0 ] } ', {k1=>1, k2=>"2", k3=>[0]}, 'whitespace');


# Test UTF-8 encoding

bothok("z", "\"\N{U+007a}\"", 'z');
bothok("\N{U+00A2}", "\"\N{U+00A2}\"", 'cent');
bothok("\N{U+6C34}", "\"\N{U+6C34}\"", 'water');
bothok("\N{U+1D11E}", "\"\\uD834\\uDD1E\"", 'G-clef');

# Binary type

sub binis($$$) { is(LLSD::parse_json($_[0])->asBinary, $_[1], $_[2]); }
binis('"AAEC/f7/"', "\x00\x01\x02\xFD\xFE\xFF", 'binary');
binis('"AAEC  \t\r\n   /f7/"', "\x00\x01\x02\xFD\xFE\xFF", 'binary, with whitespace');


