#!/usr/bin/perl -w

# @file 05.serializers.t
# @brief Unit tests for object-oriented serialization interfaces
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
use Test::More tests => 7;

use lib qw( ./lib/ . );

BEGIN { 
    use_ok( 'LLSD' );
    use_ok( 'LLSD::XMLSerializer' );
    use_ok( 'LLSD::BinarySerializer' );
    use_ok( 'LLSD::NotationSerializer' );
    }


my $sample_data = [
    new LLSD::Integer(42),
    new LLSD::UUID('6bad258e-06f0-4a87-a659-493117c9c162'),
    {
        'higgs_boson_rest_mass' => LLSD::Undefined,
        'hot' => 'cold',
        'info_page' => new LLSD::URI('https://example.org/r/6bad258e-06f0-4a87-a659-493117c9c162'),
        'status_report_due_by' => new LLSD::Date('2008-10-13T19:00:00Z')
    }
];


sub sertest($$) {
    my ($serializer, $message) = @_;
    my $serialized = $serializer->format($sample_data);
    is_deeply($sample_data, $serializer->parse($serialized), $message);
}

sertest(new LLSD::BinarySerializer(), 'binary round trip');
sertest(new LLSD::XMLSerializer(), 'xml round trip');
sertest(new LLSD::NotationSerializer(), 'notation round trip');
