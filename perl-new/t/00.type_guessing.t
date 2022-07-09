#!/usr/bin/perl -w

# @file 00.create_guess_old.t
# @brief Unit tests for LLSD type guessing
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

# Tests for LLSD-XML generation using type-guessing code
# (for interop with legacy systems that produce POD instead
# of LLSD types)

BEGIN { use_ok( 'LLSD' ); }

# Simple scalars

xis("Hello!", "<llsd><string>Hello!</string></llsd>", "Scalar string");
xis(12, "<llsd><integer>12</integer></llsd>", "Scalar int");
xis("8ae15a12-7005-4958-a39c-b01cbf54794c","<llsd><uuid>8ae15a12-7005-4958-a39c-b01cbf54794c</uuid></llsd>","Scalar UUID");
xis(12.34, "<llsd><real>12.34</real></llsd>", "Scalar real");
xis("http://cpan.org/", "<llsd><string>http://cpan.org/</string></llsd>", "Scalar URI");
xis("12.34.56", "<llsd><string>12.34.56</string></llsd>", "Scalar bad real");
xis([], "<llsd><array/></llsd>", "Empty array");
xis({}, "<llsd><map/></llsd>", "Empty hash");
xis(undef, "<llsd><undef/></llsd>", "Undef");
xis([1,"hello","8ae15a12-7005-4958-a39c-b01cbf54794c"],
    "<llsd><array><integer>1</integer><string>hello</string><uuid>8ae15a12-7005-4958-a39c-b01cbf54794c</uuid></array></llsd>",
    "Basic list");
xis([0,"Nulls!",-1,undef,"B"],
    "<llsd><array><integer>0</integer><string>Nulls!</string><integer>-1</integer><undef/><string>B</string></array></llsd>",
    "List with nulls and negatives");
xis([1,["h",undef,"s"],"t",["b",["a",4]]],
    "<llsd><array><integer>1</integer><array><string>h</string><undef/><string>s</string></array><string>t</string><array><string>b</string>"
    ."<array><string>a</string><integer>4</integer></array></array></array></llsd>",
    "List of lists");
xis({"a"=>12,"b"=>undef},"<llsd><map><key>a</key><integer>12</integer><key>b</key><undef/></map></llsd>","Basic hash");
xis({"a"=>{"b"=>12,"c"=>{"d"=>4},"e"=>undef},"f"=>23.2},
    "<llsd><map><key>a</key><map><key>e</key><undef/><key>c</key><map><key>d</key><integer>4</integer></map><key>b</key>"
    ."<integer>12</integer></map><key>f</key><real>23.2</real></map></llsd>", "Hash of hashes");
xis([1,2,q(NaN),q(Infinity)], "<llsd><array><integer>1</integer><integer>2</integer><real>NaN</real><real>Infinity</real></array></llsd>");
xis('1970-01-01T00:00:00Z', '<llsd><date>1970-01-01T00:00:00Z</date></llsd>', 'Date');
xis('1999-12-31T23:59:59.999Z', '<llsd><date>1999-12-31T23:59:59.999Z</date></llsd>', 'Date');

sub xis {
    my $guess_types = 1;
    my $xml = LLSD::format_xml($_[0], $guess_types);
    chomp $xml;
    is($xml, $_[1], $_[2]);
}
