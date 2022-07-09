#!/usr/bin/perl -w

# @file 04.binary_serialization.t
# @brief Unit tests for Binary parsing/formatting of LLSD type classes
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
use Test::More tests => 76;

use lib qw( ./lib/ . );

BEGIN { use_ok( 'LLSD' ); }


sub formatok($$$)
{
    my ($llsd, $expected, $msg) = @_;
    is_deeply($expected, LLSD::format_binary($llsd), $msg);
}

sub parseok($$$)
{
    my ($binary, $expected, $msg) = @_;
    is_deeply($expected, LLSD::parse_binary($binary), $msg);
}

sub bothok($$$)
{
    my ($llsd, $binary, $message) = @_;
    parseok($binary, $llsd, $message);
    formatok($llsd, $binary, $message);
}

sub parsefail($$$)
{
    my ($binary, $expected, $msg) = @_;

    eval {
        LLSD::parse_binary($binary);
    };

    like($@, $expected, $msg);
}


my $sample_octets = pack('C*',

    # array, 3 elements
    0x5B, 0x00, 0x00, 0x00, 0x03,

    # integer 42
    0x69, 0x00, 0x00, 0x00, 0x2A,

    # uuid "6bad258e-06f0-4a87-a659-493117c9c162"
    0x75, 0x6B, 0xAD, 0x25, 0x8E, 0x06, 0xF0, 0x4A, 0x87, 0xA6, 0x59, 0x49,
    0x31, 0x17, 0xC9, 0xC1, 0x62,

    # map, 4 elements
    0x7B, 0x00, 0x00, 0x00, 0x04,

    # key "higgs_boson_reset_mass"
    0x6B, 0x00, 0x00, 0x00, 0x15, 0x68, 0x69, 0x67, 0x67, 0x73, 0x5F, 0x62,
    0x6F, 0x73, 0x6F, 0x6E, 0x5F, 0x72, 0x65, 0x73, 0x74, 0x5f, 0x6d, 0x61,
    0x73, 0x73,

    # undefined
    0x21,

    # key "hot"
    0x6B, 0x00, 0x00, 0x00, 0x03, 0x68, 0x6F, 0x74,

    # string "cold"
    0x73, 0x00, 0x00, 0x00, 0x04, 0x63, 0x6F, 0x6C, 0x64,

    # key "info_page"
    0x6B, 0x00, 0x00, 0x00, 0x09, 0x69, 0x6E, 0x66, 0x6F, 0x5F, 0x70, 0x61,
    0x67, 0x65,

    # uri "https://example.org/r/6bad258e-06f0-4a87-a659-493117c9c162"
    0x6C, 0x00, 0x00, 0x00, 0x3A, 0x68, 0x74, 0x74, 0x70, 0x73, 0x3A, 0x2f,
    0x2F, 0x65, 0x78, 0x61, 0x6D, 0x70, 0x6C, 0x65, 0x2E, 0x6F, 0x72, 0x67,
    0x2F, 0x72, 0x2F, 0x36, 0x62, 0x61, 0x64, 0x32, 0x35, 0x38, 0x65, 0x2D,
    0x30, 0x36, 0x66, 0x30, 0x2D, 0x34, 0x61, 0x38, 0x37, 0x2D, 0x61, 0x36,
    0x35, 0x39, 0x2D, 0x34, 0x39, 0x33, 0x31, 0x31, 0x37, 0x63, 0x39, 0x63,
    0x31, 0x36, 0x32,

    # key "status_report_due_by"
    0x6B, 0x00, 0x00, 0x00, 0x14, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x5F,
    0x72, 0x65, 0x70, 0x6F, 0x72, 0x74, 0x5F, 0x64, 0x75, 0x65, 0x5F, 0x62,
    0x79,

    # date 2008-10-13T19:00:00Z
    0x64, 0x41, 0xD2, 0x3C, 0xE6, 0xAC, 0x00, 0x00, 0x00,

    # end map
    0x7D,

    # end array
    0x5D);

my $sample_data = new LLSD([
    new LLSD::Integer(42),
    new LLSD::UUID('6bad258e-06f0-4a87-a659-493117c9c162'),
    {
        'higgs_boson_rest_mass' => LLSD::Undefined,
        'hot' => 'cold',
        'info_page' => new LLSD::URI('https://example.org/r/6bad258e-06f0-4a87-a659-493117c9c162'),
        'status_report_due_by' => new LLSD::Date('2008-10-13T19:00:00Z')
    }
]);


# NOTE: Not required by the spec, but this library outputs
# maps sorted by key order under binary serialization, for
# ease of testing

bothok($sample_data, $sample_octets, 'spec sample');

# Test error cases
parsefail(pack('C*'), qr/Unexpected EOF/, 'Unexpected end of data');

parsefail(pack('C*', 91, 0, 0, 0, 1, 105, 0, 0, 0, 1), qr/Unexpected EOF/, 'Unexpected end of data');

parsefail(pack('C*', 91, 0, 0, 0, 0, 93 + 1), qr/Expected ]/, 'bad end tag');
parsefail(pack('C*', 91, 0, 0, 0, 1, 105, 0, 0, 0, 1, 66), qr/Expected ]/,'Expected array close tag');

parsefail(pack('C*',123, 0, 0, 0, 1, 0, 0, 0, 1, 97, 105, 0, 0, 0, 1, 125), qr/Expected key/, 'Expected map key tag');
parsefail(pack('C*',123, 0, 0, 0, 1, 107, 0, 0, 0, 1, 97, 105, 0, 0, 0, 1), qr/Unexpected EOF/, 'Unexpected end of data');
parsefail(pack('C*',123, 0, 0, 0, 0, 125 + 1), qr/Expected }/, 'bad end tag');
parsefail(pack('C*',123, 0, 0, 0, 1, 107, 0, 0, 0, 1, 97, 105, 0, 0, 0, 1, 66), qr/Expected }/, 'Expected map close tag');

parsefail(pack('C*',115, 0, 0, 0, 3, 97, 98), qr/Unexpected EOF/, 'Unexpected end of data');
parsefail(pack('C*', 91, 0, 0, 0, 1, 105, 0, 0, 0, 1, 93, 66), qr/Unexpected continuation/, 'Unexpected continuation of binary data');

parsefail(pack('C*',  0), qr/Unknown tag/, 'bad tag');
parsefail(pack('C*', 66), qr/Unknown tag/, 'Unexpected tag');


# Type tests

# Normal values

bothok(LLSD::Undefined, pack('C*', 33), 'undefined');

bothok(LLSD::True, pack('C*', 49), 'true');
bothok(LLSD::False, pack('C*', 48), 'false');

bothok(new LLSD::Integer(0), pack('C*', 105,0,0,0,0), 'integer - zero');
bothok(new LLSD::Integer(123), pack('C*', 105,0,0,0,123), 'positive integer');
bothok(new LLSD::Integer(-456), pack('C*', 105,255,255,254,56), 'negative integer');
bothok(new LLSD::Integer(299792458), pack('C*', 105,17,222,120,74), 'integer - speed of light');

bothok(new LLSD::Real(3.1415), pack('C*', 114,64,9,33,202,192,131,18,111), 'real - pi');
# mismatch in least-significant bits on some platforms
#bothok(new LLSD::Real(6.0221415e+23), pack('C*', 114,68,223,225,134,12,17,108,0), 'real - Avogadro\'s constant');
bothok(new LLSD::Real(6.6260693e-34), pack('C*', 114,57,11,134,11,162,197,175,88), 'real - Planck\'s constant');
bothok(new LLSD::Real(6.67428e-11), pack('C*', 114,61,210,88,155,101,151,55,209), 'real - gravitational constant');
bothok(new LLSD::Real(0.0), pack('C*', 114,0,0,0,0,0,0,0,0), 'real - zero');

bothok(new LLSD::Real(-0.0), pack('C*', 114, 0x80, 0x00, 0, 0, 0, 0, 0, 0), 'real - negative zero');
bothok(new LLSD::Real('inf'), pack('C*', 114, 0x7F, 0xF0, 0, 0, 0, 0, 0, 0), 'real - positive infinity');
bothok(new LLSD::Real('-inf'), pack('C*', 114, 0xFF, 0xF0, 0, 0, 0, 0, 0, 0), 'real - negative infinity');
bothok(new LLSD::Real('nan'), pack('C*', 114, 0x7F, 0xF8, 0, 0, 0, 0, 0, 0), 'real - not a number');

bothok(new LLSD::String("abc"), pack('C*', 115,0,0,0,3,97,98,99), 'string');

bothok([], pack('C*', 91,0,0,0,0,93), 'empty array');
bothok([new LLSD::Integer(1)], pack('C*', 91,0,0,0,1,105,0,0,0,1,93), 'array - one element');
bothok([new LLSD::Integer(1),new LLSD::Integer(2),new LLSD::Integer(3)], pack('C*', 91,0,0,0,3,105,0,0,0,1,105,0,0,0,2,105,0,0,0,3,93), 'array - several elements');
bothok([new LLSD::Integer(1),new LLSD::String("abc"),new LLSD::Integer(2),new LLSD::String("def")], pack('C*', 91,0,0,0,4,105,0,0,0,1,115,0,0,0,3,97,98,99,105,0,0,0,2,115,0,0,0,3,100,101,102,93), 'array - heterogeneous');

bothok({}, pack('C*', 123,0,0,0,0,125), 'empty map');
bothok({"abc" => new LLSD::Integer(1)}, pack('C*', 123,0,0,0,1,107,0,0,0,3,97,98,99,105,0,0,0,1,125), 'map - one element');
bothok({"abc" => new LLSD::Integer(1), "def" => new LLSD::String("ghi")}, pack('C*', 123,0,0,0,2,107,0,0,0,3,97,98,99,105,0,0,0,1,107,0,0,0,3,100,101,102,115,0,0,0,3,103,104,105,125), 'map - several elements');

# Test UTF-8 encoding

bothok("z", pack("C*", 115, 0, 0, 0, 1, 0x7a), 'z');
bothok("\xA2", pack("C*", 115, 0, 0, 0, 2, 0xc2,0xa2), 'cent');
bothok("\N{U+6C34}", pack("C*", 115, 0, 0, 0, 3, 0xe6,0xb0,0xb4), 'water');
bothok("\N{U+1D11E}", pack("C*", 115, 0, 0, 0, 4, 0xf0,0x9d,0x84,0x9e), 'G-clef');

parsefail(pack('C*', 115, 0, 0, 0, 1, 0xC0), qr/Invalid UTF-8/, 'bad utf8');
parsefail(pack('C*', 115, 0, 0, 0, 1, 0xC1), qr/Invalid UTF-8/, 'bad utf8');
parsefail(pack('C*', 115, 0, 0, 0, 1, 0xF5), qr/Invalid UTF-8/, 'bad utf8');
parsefail(pack('C*', 115, 0, 0, 0, 1, 0xF8), qr/Invalid UTF-8/, 'bad utf8');
parsefail(pack('C*', 115, 0, 0, 0, 1, 0xFC), qr/Invalid UTF-8/, 'bad utf8');
parsefail(pack('C*', 115, 0, 0, 0, 1, 0xFE), qr/Invalid UTF-8/, 'bad utf8');
parsefail(pack('C*', 115, 0, 0, 0, 1, 0xFF), qr/Invalid UTF-8/, 'bad utf8');

