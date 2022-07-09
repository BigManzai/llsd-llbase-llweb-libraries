#!/usr/bin/perl -w

# @file 08.notation_serialization.t
# @brief Unit tests for Notation parsing/formatting of LLSD type classes
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
use Test::More tests => 59;

use lib qw( ./lib/ . );

use MIME::Base64;

BEGIN { use_ok( 'LLSD' ); }


sub formatok($$$)
{
    my ($llsd, $expected, $msg) = @_;
    is_deeply(LLSD::format_notation($llsd), $expected, $msg);
}

sub parseok($$$)
{
    my ($notation, $expected, $msg) = @_;
    is_deeply(LLSD::parse_notation($notation), $expected, $msg);
}

sub bothok($$$)
{
    my ($notation, $llsd, $message) = @_;
    parseok($notation, $llsd, $message);
    formatok($llsd, $notation, $message);
}

sub parsefail($$$)
{
    my ($notation, $expected, $msg) = @_;

    eval {
        LLSD::parse_notation($notation);
    };

    like($@, $expected, $msg);
}



# true
parseok('1', LLSD::True, '1 is true');
parseok('t', LLSD::True, 't is true');
parseok('T', LLSD::True, 'T is true');
bothok('true', LLSD::True, 'true is true');
parseok('TRUE', LLSD::True, 'TRUE is true');

# false
parseok('0', LLSD::False, '0 is false');
parseok('f', LLSD::False, 'f is false');
parseok('F', LLSD::False, 'F is false');
bothok('false', LLSD::False, 'false is false');
parseok('FALSE', LLSD::False, 'FALSE is false');

# binary
sub binis($$$) { is(LLSD::parse_notation($_[0])->asBinary, $_[1], $_[2]); }

my $o1 = pack('C*', 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233);
my $o2 = pack('C*', 0, 1, 2, 253, 254, 255);
binis('b64"AAEBAgMFCA0VIjdZkOk="', $o1, 'b64 decode');
binis('b64"AAEC  \t\r\n   /f7/"', $o2, 'b64 decode with whitespace');
binis('b16"000101020305080d1522375990e9"', $o1, 'b16 decode');
binis('b16"000101020  \t\r\n   305080d1522375990e9"', $o1, 'b16 decode with whitespace');
binis("b(14)\"\x00\x01\x01\x02\x03\x05\x08\x0d\x15\x22\x37\x59\x90\xe9\"", $o1, 'b64(len) decode');

# string
parseok('"foo\\"ba\\\\r"', 'foo"ba\\r', 'double quoted string');
bothok("'foo\\'ba\\\\r'", "foo'ba\\r", 'single quoted string');
parseok('s(8)"foo"ba\\r"', 'foo"ba\\r', 'length-specified string');

# date
bothok('d"1970-01-01T00:00:00.000Z"', new LLSD::Date(0), 'date - thousandths');
parseok('d"1970-01-01T00:00:00.00Z"', new LLSD::Date(0), 'date - hundredths');
parseok('d"1970-01-01T00:00:00.0Z"', new LLSD::Date(0), 'date - tenths');
parseok('d"1970-01-01T00:00:00.Z"', new LLSD::Date(0), 'date - decimal');
parseok('d"1970-01-01T00:00:00Z"', new LLSD::Date(0), 'date - no millis');

my $sample = "" .
    "[\n" .
    "  {'destination':'http://secondlife.com'}, \n" .
    "  {'version':i1}, \n" .
    "  {\n" .
    "    'agent_id':u3c115e51-04f4-523c-9fa6-98aff1034730, \n" .
    "    'session_id':u2c585cec-038c-40b0-b42e-a25ebab4d132, \n" .
    "    'circuit_code':i1075, \n" .
    "    'first_name':'Phoenix', \n" .
    "    'last_name':'Linden',\n" .
    "    'position':[r70.9247,r254.378,r38.7304], \n" .
    "    'look_at':[r-0.043753,r-0.999042,r0], \n" .
    "    'granters':[ua2e76fcd-9360-4f6d-a924-000000000003],\n" .
    "    'attachment_data':\n" .
    "    [\n" .
    "      {\n" .
    "        'attachment_point':i2,\n" .
    "        'item_id':ud6852c11-a74e-309a-0462-50533f1ef9b3,\n" .
    "        'asset_id':uc69b29b1-8944-58ae-a7c5-2ca7b23e22fb\n" .
    "      },\n" .
    "      {\n" .
    "        'attachment_point':i10, \n" .
    "        'item_id':uff852c22-a74e-309a-0462-50533f1ef900,\n" .
    "        'asset_id':u5868dd20-c25a-47bd-8b4c-dedc99ef9479\n" .
    "      }\n" .
    "    ]\n" .
    "  }\n" .
    "]\n";

parseok($sample, [
           { 'destination' => 'http://secondlife.com' },
           { 'version' => 1 },
           {
               'agent_id' => new LLSD::UUID('3c115e51-04f4-523c-9fa6-98aff1034730'),
               'session_id' => new LLSD::UUID('2c585cec-038c-40b0-b42e-a25ebab4d132'),
               'circuit_code' => 1075,
               'first_name' => 'Phoenix',
               'last_name' => 'Linden',
               'position' => [70.9247, 254.378, 38.7304],
               'look_at' => [-0.043753, -0.999042, 0],
               'granters' => [new LLSD::UUID('a2e76fcd-9360-4f6d-a924-000000000003')],
               'attachment_data' =>
                   [
                    {
                        'attachment_point' => 2,
                        'item_id' => new LLSD::UUID('d6852c11-a74e-309a-0462-50533f1ef9b3'),
                        'asset_id' => new LLSD::UUID('c69b29b1-8944-58ae-a7c5-2ca7b23e22fb')
                    },
                    {
                        'attachment_point' => 10,
                        'item_id' => new LLSD::UUID('ff852c22-a74e-309a-0462-50533f1ef900'),
                        'asset_id' => new LLSD::UUID('5868dd20-c25a-47bd-8b4c-dedc99ef9479')
                    }
                   ]
           }
       ], 'sample #1');

$sample = "" .
    "[\n" .
    "  {\n" .
    "    'creation-date':d\"2007-03-15T18:30:18Z\", \n" .
    "    'creator-id':u3c115e51-04f4-523c-9fa6-98aff1034730\n" .
    "  },\n" .
    "  s(10)\"0123456789\",\n" .
    "  \"Where's the beef?\",\n" .
    "  'Over here.',  \n" .
    "  b(158)\"default\n" .
    "{\n" .
    "    state_entry()\n" .
    "    {\n" .
    "        llSay(0, \"Hello, Avatar!\");\n" .
    "    }\n" .
    "\n" .
    "    touch_start(integer total_number)\n" .
    "    {\n" .
    "        llSay(0, \"Touched.\");\n" .
    "    }\n" .
    "}\",\n" .
    "  b64\"AABAAAAAAAAAAAIAAAA//wAAP/8AAADgAAAA5wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n" .
    "AABkAAAAZAAAAAAAAAAAAAAAZAAAAAAAAAABAAAAAAAAAAAAAAAAAAAABQAAAAEAAAAQAAAAAAAA\n" .
    "AAUAAAAFAAAAABAAAAAAAAAAPgAAAAQAAAAFAGNbXgAAAABgSGVsbG8sIEF2YXRhciEAZgAAAABc\n" .
    "XgAAAAhwEQjRABeVAAAABQBjW14AAAAAYFRvdWNoZWQuAGYAAAAAXF4AAAAIcBEI0QAXAZUAAEAA\n" .
    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\" \n" .
    "]\n";

parseok($sample, [
           {
               'creation-date' => new LLSD::Date("2007-03-15T18:30:18Z"),
               'creator-id' => new LLSD::UUID('3c115e51-04f4-523c-9fa6-98aff1034730')
           },
           "0123456789",
           "Where's the beef?",
           'Over here.',
           new LLSD::Binary(decode_base64(
                                "ZGVmYXVsdAp7CiAgICBzdGF0ZV9lbnRyeSgpCiAgICB7CiAgICAgICAgbGxTYXkoMCwgIkhlbGxv" .
                                "LCBBdmF0YXIhIik7CiAgICB9CgogICAgdG91Y2hfc3RhcnQoaW50ZWdlciB0b3RhbF9udW1iZXIp" .
                                "CiAgICB7CiAgICAgICAgbGxTYXkoMCwgIlRvdWNoZWQuIik7CiAgICB9Cn0=")),
           new LLSD::Binary(decode_base64(
                                "AABAAAAAAAAAAAIAAAA//wAAP/8AAADgAAAA5wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" .
                                "AABkAAAAZAAAAAAAAAAAAAAAZAAAAAAAAAABAAAAAAAAAAAAAAAAAAAABQAAAAEAAAAQAAAAAAAA" .
                                "AAUAAAAFAAAAABAAAAAAAAAAPgAAAAQAAAAFAGNbXgAAAABgSGVsbG8sIEF2YXRhciEAZgAAAABc" .
                                "XgAAAAhwEQjRABeVAAAABQBjW14AAAAAYFRvdWNoZWQuAGYAAAAAXF4AAAAIcBEI0QAXAZUAAEAA" .
                                "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"))
       ], 'sample #2');

parsefail('', qr/unexpected EOF/i, 'empty string');
parsefail('ibad', qr/expected digit/i, 'bad integer');
parsefail('rcool', qr/expected digit/i, 'bad real');
parsefail('uwish', qr/unexpected EOF/i, 'bad uuid');
parsefail('b(4)123', qr/expected quote/i, 'unquoted binary');
parsefail('b(4)"123"', qr/expected/i, 'short binary');
parsefail('b16"not hex"', qr/Invalid base16/i, 'invalid base16');
parsefail('b64"12345"', qr/Invalid base64/i, 'invalid base64');
parsefail("b(1)'\N{U+ABCD}'", qr/Invalid byte value/i, 'invalid octet');
parsefail('b99"12345"', qr/unexpected binary base/i, 'bad binary base');
parsefail('snope', qr/expected '\('/i, 'bogus string');
parsefail('s(2)"1"', qr/expected/i, 'short string length');
parsefail('s(2)"123"', qr/expected/i, 'long string length');
parsefail('"unclosed', qr/expected/i, 'unterminated string');
parsefail("'unclosed", qr/expected/i, 'unterminated string');
parsefail('denial', qr/expected/i, 'unquoted date');
parsefail('d"1999-01-02T21:09:33Z', qr/expected/i, 'unterminated date');
parsefail('[', qr/unexpected EOF/i, 'unclosed array');
parsefail('[i5,', qr/unexpected EOF/i, 'unclosed array');
parsefail('[,', qr/unexpected token/i, 'missing array value');
parsefail('{', qr/expected string/i, 'unclosed map');
parsefail('{i5', qr/expected string/i, 'unclosed map');
parsefail('{"key"', qr/expected ':'/i, 'missing map value');
parsefail('{"key":', qr/unexpected EOF/i, 'missing map value');
parsefail('{"key":,', qr/unexpected token: ,/i, 'missing map value');
parsefail('{,', qr/expected string/i, 'missing map key/value');
parsefail('go back!', qr/unexpected token: g/i, 'bogus value');
parsefail('i5 north', qr/unexpected continuation of data/i, 'extra data');

# Whitespace

parseok(' { "k1" : i1 , "k2" : "2" , "k3" : [ i0 ] } ', {k1=>1, k2=>"2", k3=>[0]}, 'whitespace');
