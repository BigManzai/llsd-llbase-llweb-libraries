#!/usr/bin/perl -w

# @file 02.new_types_xml.t
# @brief Unit tests for XML serialization LLSD type classes 
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
use Test::More tests => 51;

use lib qw( ./lib/ . );
BEGIN { use_ok( 'LLSD' ); }


sub xis {
    my $xml = LLSD::format_xml($_[0]);
    chomp $xml;
    is($xml, $_[1], $_[2]);
}

xis(LLSD::URI->new('http://lindenlab.com/'),'<llsd><uri>http://lindenlab.com/</uri></llsd>','URI http://lindenlab.com/ as URI');
xis(LLSD::Boolean->new('http://lindenlab.com/'),'<llsd><boolean>true</boolean></llsd>','URI http://lindenlab.com/ as Boolean');
xis(LLSD::String->new('http://lindenlab.com/'),'<llsd><string>http://lindenlab.com/</string></llsd>','URI http://lindenlab.com/ as String');
xis(LLSD::UUID->new('http://lindenlab.com/'),'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>','URI http://lindenlab.com/ as UUID');
xis(LLSD::Date->new('http://lindenlab.com/'),'<llsd><date>1970-01-01T00:00:00.000Z</date></llsd>','URI http://lindenlab.com/ as Date');
xis(LLSD::Real->new('http://lindenlab.com/'),'<llsd><real>0</real></llsd>','URI http://lindenlab.com/ as Real');
xis(LLSD::Integer->new('http://lindenlab.com/'),'<llsd><integer>0</integer></llsd>','URI http://lindenlab.com/ as Integer');
xis(LLSD::URI->new(1),'<llsd><uri></uri></llsd>','1 as URI');
xis(LLSD::Boolean->new(1),'<llsd><boolean>true</boolean></llsd>','1 as Boolean');
xis(LLSD::String->new(1),'<llsd><string>1</string></llsd>','1 as String');
xis(LLSD::UUID->new(1),'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>','1 as UUID');
xis(LLSD::Date->new(1),'<llsd><date>1970-01-01T00:00:01.000Z</date></llsd>','1 as Date');
xis(LLSD::Real->new(1),'<llsd><real>1</real></llsd>','1 as Real');
xis(LLSD::Integer->new(1),'<llsd><integer>1</integer></llsd>','1 as Integer');
xis(LLSD::URI->new('Yarbles!'),'<llsd><uri></uri></llsd>','String Yarbles! as URI');
xis(LLSD::Boolean->new('Yarbles!'),'<llsd><boolean>true</boolean></llsd>','String Yarbles! as Boolean');
xis(LLSD::String->new('Yarbles!'),'<llsd><string>Yarbles!</string></llsd>','String Yarbles! as String');
xis(LLSD::UUID->new('Yarbles!'),'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>','String Yarbles! as UUID');
xis(LLSD::Date->new('Yarbles!'),'<llsd><date>1970-01-01T00:00:00.000Z</date></llsd>','String Yarbles! as Date');
xis(LLSD::Real->new('Yarbles!'),'<llsd><real>0</real></llsd>','String Yarbles! as Real');
xis(LLSD::Integer->new('Yarbles!'),'<llsd><integer>0</integer></llsd>','String Yarbles! as Integer');
xis(LLSD::URI->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><uri></uri></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as URI');
xis(LLSD::Boolean->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><boolean>true</boolean></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as Boolean');
xis(LLSD::String->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><string>8ae15a12-7005-4958-a39c-b01cbf54794c</string></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as String');
xis(LLSD::UUID->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><uuid>8ae15a12-7005-4958-a39c-b01cbf54794c</uuid></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as UUID');
xis(LLSD::Date->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><date>1970-01-01T00:00:00.000Z</date></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as Date');
xis(LLSD::Real->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><real>0</real></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as Real');
xis(LLSD::Integer->new('8ae15a12-7005-4958-a39c-b01cbf54794c'),'<llsd><integer>0</integer></llsd>','UUID 8ae15a12-7005-4958-a39c-b01cbf54794c as Integer');
xis(LLSD::URI->new('1974-04-04T12:34:56.789Z'),'<llsd><uri></uri></llsd>','Date 1974-04-04T12:34:56.789Z as URI');
xis(LLSD::Boolean->new('1974-04-04T12:34:56.789Z'),'<llsd><boolean>true</boolean></llsd>','Date 1974-04-04T12:34:56.789Z as Boolean');
xis(LLSD::String->new('1974-04-04T12:34:56.789Z'),'<llsd><string>1974-04-04T12:34:56.789Z</string></llsd>','Date 1974-04-04T12:34:56.789Z as String');
xis(LLSD::UUID->new('1974-04-04T12:34:56.789Z'),'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>','Date 1974-04-04T12:34:56.789Z as UUID');
xis(LLSD::Date->new('1974-04-04T12:34:56.789Z'),'<llsd><date>1974-04-04T12:34:56.789Z</date></llsd>','Date 1974-04-04T12:34:56.789Z as Date');
xis(LLSD::Real->new('1974-04-04T12:34:56.789Z'),'<llsd><real>0</real></llsd>','Date 1974-04-04T12:34:56.789Z as Real');
xis(LLSD::Integer->new('1974-04-04T12:34:56.789Z'),'<llsd><integer>0</integer></llsd>','Date 1974-04-04T12:34:56.789Z as Integer');
xis(LLSD::URI->new('34.56'),'<llsd><uri></uri></llsd>','Real 34.56 as URI');
xis(LLSD::Boolean->new('34.56'),'<llsd><boolean>true</boolean></llsd>','Real 34.56 as Boolean');
xis(LLSD::String->new('34.56'),'<llsd><string>34.56</string></llsd>','Real 34.56 as String');
xis(LLSD::UUID->new('34.56'),'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>','Real 34.56 as UUID');
xis(LLSD::Date->new('34.56'),'<llsd><date>1970-01-01T00:00:34.560Z</date></llsd>','Real 34.56 as Date');
xis(LLSD::Real->new('34.56'),'<llsd><real>34.56</real></llsd>','Real 34.56 as Real');
xis(LLSD::Integer->new('34.56'),'<llsd><integer>34</integer></llsd>','Real 34.56 as Integer');
xis(LLSD::URI->new('12'),'<llsd><uri></uri></llsd>','Integer 12 as URI');
xis(LLSD::Boolean->new('12'),'<llsd><boolean>true</boolean></llsd>','Integer 12 as Boolean');
xis(LLSD::String->new('12'),'<llsd><string>12</string></llsd>','Integer 12 as String');
xis(LLSD::UUID->new('12'),'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>','Integer 12 as UUID');
xis(LLSD::Date->new('12'),'<llsd><date>1970-01-01T00:00:12.000Z</date></llsd>','Integer 12 as Date');
xis(LLSD::Real->new('12'),'<llsd><real>12</real></llsd>','Integer 12 as Real');
xis(LLSD::Integer->new('12'),'<llsd><integer>12</integer></llsd>','Integer 12 as Integer');

xis({'first' => new LLSD('Nan', 'string'),
     'last' => new LLSD('Infinity', 'string')},
    '<llsd><map><key>first</key><string>Nan</string><key>last</key><string>Infinity</string></map></llsd>',
    'Ensure number-like explicit strings serialize as strings');
