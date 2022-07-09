#!/usr/bin/perl -w

# @file 03.parse_xml.t
# @brief Unit tests for XML parsing LLSD type classes 
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
use Test::More tests => 18;

use lib qw( ./lib/ . );

# Tests for LLSD-XML parsing

BEGIN { use_ok( 'LLSD' ); }

sub parsefail($$$)
{
    my ($xml, $expected, $msg) = @_;

    eval {
        LLSD::parse_xml($xml);
    };

    like($@, $expected, $msg);
}

sub parseok($$$)
{
    my ($xml, $expected, $msg) = @_;
    is_deeply($expected, LLSD::parse_xml($xml));
}


parsefail('<llsd><map><key>foo</key></map></llsd>', qr/expected value/, 'missing value');
parsefail('<llsd><map><string>bar</string></map></llsd>', qr/expected key/, 'missing key');
parsefail('<foo/>', qr/does not have root element llsd/, 'root');
parsefail('<llsd><foo>bar</foo></llsd>', qr/invalid type/i, 'invalid type');
parsefail('<llsd><undef>something</undef></llsd>', qr/error/i, 'invalid undef');
parsefail('<llsd><boolean>something</boolean></llsd>', qr/invalid format for boolean/i, 'invalid boolean');
parsefail('<llsd><real>something</real></llsd>', qr/invalid format for real/i, 'invalid real');
parsefail('<llsd><integer>something</integer></llsd>', qr/invalid format for integer/i, 'invalid integer');
parsefail('<llsd><uuid>something</uuid></llsd>', qr/invalid format for uuid/i, 'invalid uuid');
parsefail('<llsd><date>something</date></llsd>', qr/invalid format for date/i, 'invalid date');

parseok('<llsd><boolean>true</boolean></llsd>', LLSD::True, 'boolean true');
parseok('<llsd><boolean>1</boolean></llsd>', LLSD::True, 'boolean 1');
parseok('<llsd><boolean>false</boolean></llsd>', LLSD::False, 'boolean false');
parseok('<llsd><boolean>0</boolean></llsd>', LLSD::False, 'boolean 0');
parseok('<llsd><boolean/></llsd>', LLSD::False, 'boolean empty');


sub binis($$$) { is(LLSD::parse_xml($_[0])->asBinary, $_[1], $_[2]); }
binis("<llsd><binary>AAEC/f7/</binary></llsd>", "\x00\x01\x02\xFD\xFE\xFF", 'binary');
binis("<llsd><binary>AAEC  \t\r\n   /f7/</binary></llsd>", "\x00\x01\x02\xFD\xFE\xFF", 'binary, with whitespace');
