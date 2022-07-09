#!/usr/bin/perl -w

# @file 09.round_trips.t
# @brief Round tripping various types through different serializations
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
use Test::More tests => 1903;

use lib qw( ./lib/ . );


BEGIN { 
    use_ok( 'LLSD' );
    use_ok( 'LLSD::XMLSerializer' );
    use_ok( 'LLSD::BinarySerializer' );
    use_ok( 'LLSD::NotationSerializer' );
    use_ok( 'LLSD::NotationSerializer' );
    use_ok( 'LLSD::JSONSerializer' );
    }


my @values = (

    # Undefined
    LLSD::Undefined,

    # Boolean
    LLSD::True,
    LLSD::False,

    # Integer
    new LLSD::Integer(),
    new LLSD::Integer(0),
    new LLSD::Integer(123),
    new LLSD::Integer(-456),
    new LLSD::Integer( 0x7fffffff),
    new LLSD::Integer(-0x80000000),

    # Real
    new LLSD::Real(),
    new LLSD::Real(0),
    new LLSD::Real(12.34),
    new LLSD::Real(-98.76),
    new LLSD::Real(0.000001),
    new LLSD::Real(-0.000001),
    new LLSD::Real(1e308),
    new LLSD::Real(-1e308),
    new LLSD::Real(1e-290),
    new LLSD::Real(-1e-290),
    new LLSD::Real('inf'), # Positive Infinity
    new LLSD::Real('-inf'), # Negative Infinity
    new LLSD::Real(-1/'inf'), # negative 0
    new LLSD::Real('nan'), # Not-a-number

    # String
    new LLSD::String(),
    new LLSD::String(""),
    new LLSD::String("abc123"),
    new LLSD::String("\N{U+00A2}"), # cent
    new LLSD::String("\N{U+6C34}"), # water
    new LLSD::String("\N{U+1D11E}"), # G-clef

    # URI
    new LLSD::URI(),
    new LLSD::URI(""),
    new LLSD::URI("http://example.com"),
    new LLSD::URI("http://example.com/~this(is)a\%20t_e-s.t/?query#fragment"),
    new LLSD::URI("mailto:test\@example.com"),

    # UUID
    new LLSD::UUID(),
    new LLSD::UUID(""),
    new LLSD::UUID("00000000-0000-0000-0000-000000000000"),
    new LLSD::UUID("ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf"),

    # Binary
    new LLSD::Binary(),
    new LLSD::Binary(""),
    new LLSD::Binary("\x00\x01\x02\xfd\xfe\xff"),
    new LLSD::Binary(pack("C*", 0..255)),

    # Date
    new LLSD::Date(),
    new LLSD::Date(""),
    new LLSD::Date("1970-01-01T00:00:00.000Z"),
    new LLSD::Date("2010-12-18T05:26:39.000Z"),

    # Array
    new LLSD::Array(
        LLSD::Undefined,
        LLSD::True,
        LLSD::False,
        new LLSD::Integer(123),
        new LLSD::Real(12.34),
        new LLSD::String("abc123"),
        new LLSD::URI("http://example.com"),
        new LLSD::UUID("ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf"),
        new LLSD::Binary("\x00\x01\x02\xfd\xfe\xff"),
        new LLSD::Date("2010-12-18T05:26:39.000Z"),
        ),

    # Map
    new LLSD::Array(
        'undef' => LLSD::Undefined,
        'true' => LLSD::True,
        'false' => LLSD::False,
        'integer' => new LLSD::Integer(123),
        'real' => new LLSD::Real(12.34),
        'string' => new LLSD::String("abc123"),
        'uri' => new LLSD::URI("http://example.com"),
        'uuid' => new LLSD::UUID("ff9a71eb-7414-4bf8-866e-a70ddeb7c3cf"),
        'binary' => new LLSD::Binary("\x00\x01\x02\xfd\xfe\xff"),
        'date' => new LLSD::Date("2010-12-18T05:26:39.000Z"),
        )
    );

my @serializers = (
    new LLSD::BinarySerializer,
    new LLSD::XMLSerializer,
    new LLSD::JSONSerializer,
    new LLSD::NotationSerializer,
    );


sub escape($) {
    join('', map { 
        0x20 <= $_ and $_ <= 0x7e ? chr($_) : sprintf('\x%02x', $_, $_);
         } (map {ord} split(//, $_[0])));
}

#
# Spec issues:
#
sub skip_test($) {

    # Original: Real(NaN)    ->asBoolean == false
    # --JSON--> String("NaN")->asBoolean == true
    return 1 if $_[0] =~ /LLSD::JSONSerializer: Infinity \(real\) - asBinary/;
    
    # Original: Real(inf)         ->asBinary  == ''
    # --JSON--> String("Infinity")->asBinary == '"w\xe2\x9e+r'        
    return 1 if $_[0] =~ /LLSD::JSONSerializer: NaN \(real\) - asBoolean/;

    # Original: URI(*)   ->asBoolean == false
    # --JSON--> String(*)->asBoolean == true
    return 1 if $_[0] =~ /LLSD::JSONSerializer: .* \(uri\) - asBoolean/;

    # Original: UUID(*) ->asBoolean  == false
    # --JSON--> String(*)->asBoolean == true
    return 1 if $_[0] =~ /LLSD::JSONSerializer: .* \(uuid\) - asBoolean/;

    # Original: Date(*) ->asBoolean  == false
    # --JSON--> String(*)->asBoolean == true
    return 1 if $_[0] =~ /LLSD::JSONSerializer: .* \(date\) - asBoolean/;

    # Original: Binary(*) ->asBoolean  == false
    # --JSON--> String(*)->asBoolean == true
    return 1 if $_[0] =~ /LLSD::JSONSerializer: .* \(binary\) - asBoolean/;

    # Original: Date(*) ->asReal  == time
    # --JSON--> String(*)->asReal == 0
    return 1 if $_[0] =~ /LLSD::JSONSerializer: .* \(date\) - asReal/;
}

foreach my $serializer (@serializers) {
    foreach my $value (@values) {

        my $msg = sprintf('%s: %s (%s) - ', ref $serializer, escape($value), $value->type);

        my $out = eval { $serializer->format($value) };
        if ($@) { fail("format: $msg $@"); next; }

        my $parsed = eval { $serializer->parse($out) };
        if ($@) { fail("parse: $msg $@"); next; }

        #print STDERR $msg, "\n";
        #print STDERR "value: ".escape($value)."\n";
        #print STDERR "parsed: ".escape($parsed)."\n";

        sub compare($$$) {
            my ($orig, $new, $msg) = @_;
            
            is_deeply($orig, $new, $msg) unless skip_test($msg);
        }

        compare($value, $parsed, $msg . 'round trip');
        compare($value->asUndefined, $parsed->asUndefined, $msg . 'asUndefined');
        compare($value->asBoolean, $parsed->asBoolean, $msg . 'asBoolean');
        compare($value->asInteger, $parsed->asInteger, $msg . 'asInteger');
        compare($value->asReal, $parsed->asReal, $msg . 'asReal');
        compare($value->asString, $parsed->asString, $msg . 'asString');
        compare($value->asURI, $parsed->asURI, $msg . 'asURI');
        compare($value->asUUID, $parsed->asUUID, $msg . 'asUUID');
        compare($value->asBinary, $parsed->asBinary, $msg . 'asBinary');
        compare($value->asDate, $parsed->asDate, $msg . 'asDate');
    }
}

