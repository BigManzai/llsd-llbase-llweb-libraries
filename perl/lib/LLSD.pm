#!/usr/bin/perl -w

# @file LLSD.pm
# @brief LLSD abstract type system
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

package LLSD;

=head1 NAME

LLSD - Abstract type system implementation for Perl

=head1 SYNOPSIS

    use LLSD;

    # Types

    my $llsd = {
        url => new LLSD::URI('http://example.com'),
        values => [ new LLSD::Integer(1), new LLSD::Integer(2) ],
        flag => LLSD::True
    };

    print new LLSD::Integer(1) + new LLSD::Integer(2); # -> 3


    # Conversion

    my $basic = LLSD($pod); # all scalars will be strings
    my $guess = LLSD($pod, 1); # Integer, Real, Date, and UUID are guessed

    my $string = new LLSD('-inf')->asString(); # "-Infinity"


    # Serialization Functions

    my $data = LLSD::parse_xml_file('myfile.xml');
    my $data = LLSD::parse_xml('<llsd><integer>123</integer></llsd>');
    my $xml = LLSD::format_xml($llsd, GUESS);

    my $data = LLSD::parse_binary_file('myfile.bin');
    my $data = LLSD::parse_binary("i\x00\x00\x00\x7b");
    my $bin = LLSD::format_binary($llsd, GUESS);


    # Object-Oriented Serialization

    my $serializer = new LLSD::XMLSerializer({guess_types=>GUESS});
    my $serializer = new LLSD::BinarySerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);


=head1 DESCRIPTION

LLSD is an abstract type system for REStful data services. It provides
a small number of commonly used simple types (strings, integers, reals
UUIDs, URIs, Dates, and Binary data) and complex types (maps, arrays)
common to most modern programming languages.

The type system provides well defined conversion rules between types
and default values.

Several serialization formats are defined for LLSD, including XML,
Binary and JSON.

This module provides functionality for LLSD serialization (reading and
writing) and maintaining types in Perl. For example, it is possible
to create a data structure with typed integers and strings and ensure
they are serialized with the correct LLSD types to an XML or Binary stream.
Thanks to the L<overload> module, the typed values can be used in nearly
all cases where scalars might be used.

=cut

use strict;
use warnings;
use Exporter;
use Scalar::Util qw(looks_like_number);

our $VERSION = 2.00;
our @ISA = qw(Exporter);
our @EXPORT = qw();
our @EXPORT_OK = qw(LLSD True False Undefined);

=head1 CONSTANTS

These are exportable, but not exported by default.

    LLSD::True
    LLSD::False
        These are LLSD::Boolean values.

    LLSD::Undefined
        This is an alias for undef.

=cut

# LLSD Constants
my $true_val = bless \(my $tv = 1), 'LLSD::Boolean';
my $false_val = bless \(my $fv = 0), 'LLSD::Boolean';
sub True() { return $true_val; }
sub False() { return $false_val; }
sub Undefined() { return undef; }

my $MIN_INT32 = -2147483648; # -0x80000000
my $MAX_INT32 =  2147483647; #  0x7fffffff
sub _toInt32($) {
    # *NOTE: Intentionally treats non-number scalars and NaNs as 0
    my $n = shift;
    $n = 0 + $n;
    return $MIN_INT32 if $n <= $MIN_INT32;
    return $MAX_INT32 if $n >= $MAX_INT32;
    return 0 + sprintf('%d', $n); # sprintf uses round-to-even
}

=head1 FUNCTIONS

    $type = LLSD::type( value[, GUESS_TYPES] )

Returns the LLSD type for a Perl value. If the optional
second parameter is truthy, then scalars will be inspected
to determine if they match numbers (integer, real), dates,
or UUIDs. Otherwise, scalars will be considered strings.
Unknown reference types will be undefined.

The return value will be one of: C<"array">, C<"map">, C<"string">,
C<"boolean">, C<"integer">, C<"real">, C<"uuid">, C<"uri">,
C<"date">, C<"binary">.

=cut

sub type($;$) {
    my ($value, $guess) = @_;

    return "undef" unless defined $value;

    # use UNIVERSAL::isa (rather than hash-of-type names) to allow subclasses
    return 'map' if UNIVERSAL::isa($value, 'HASH');
    return 'array' if UNIVERSAL::isa($value, 'ARRAY');

    return 'string' if UNIVERSAL::isa($value, 'LLSD::String');
    return 'boolean' if UNIVERSAL::isa($value, 'LLSD::Boolean');
    return 'integer' if UNIVERSAL::isa($value, 'LLSD::Integer');
    return 'real' if UNIVERSAL::isa($value, 'LLSD::Real');
    return 'uuid' if UNIVERSAL::isa($value, 'LLSD::UUID');
    return 'uri' if UNIVERSAL::isa($value, 'LLSD::URI');
    return 'date' if UNIVERSAL::isa($value, 'LLSD::Date');
    return 'binary' if UNIVERSAL::isa($value, 'LLSD::Binary');

    # For any other non-reference type:
    return "undef" if ref($value);

    if ($guess) {
        # If we feel lucky, other possible scalar types
        # scalar types
        return "uuid" if $value =~ m/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;
        return "date" if $value =~ m/^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z$/;
        return $value =~ m/^-?\d+$/ ? "integer" : "real" if looks_like_number($value);
    }

    # Otherwise, scalars are:
    return "string";
}


=pod

    $llsd = LLSD::LLSD( $data [, GUESS_TYPES] );
    $llsd = new LLSD( $data [, GUESS_TYPES] );

Recursively traverse the supplied data structure
and produce a corresponding structure of LLSD-typed
values, optionally guessing for scalars. Both forms
are equivalent.

=cut

#
# Recursively convert Plain Old Data structures
# into LLSD-typed structures
#
sub LLSD($;$); # Forward declaration for recursion
sub LLSD($;$) {
    my ($value, $guess_types) = @_;

    my $type = LLSD::type($value, $guess_types);

    return undef if $type eq "undef";

    return new LLSD::String($value) if $type eq "string";
    return new LLSD::Boolean($value) if $type eq "boolean";
    return new LLSD::Integer($value) if $type eq "integer";
    return new LLSD::Real($value) if $type eq "real";
    return new LLSD::URI($value) if $type eq "uri";
    return new LLSD::UUID($value) if $type eq "uuid";
    return new LLSD::Binary($value) if $type eq "binary";
    return new LLSD::Date($value) if $type eq "date";

    # recurse if structured
    if ($type eq "map") {
        my $map = {};
        foreach my $key (keys %$value) {
            $$map{$key} = LLSD($$value{$key}, $guess_types);
        }
        return $map;
    }

    if ($type eq "array") {
        my $array = [];
        foreach my $val (@$value) {
            push(@$array, LLSD($val, $guess_types));
        }
        return $array;
    }

    # This should never happen, as LLSD::type always returns something
    die "Unexpected value: ", $value, " type: ", ref $value, "\n";
}

# Allow both:
#    my $foo = new LLSD($bar)
#    my $foo = LLSD($bar)

sub new($$) {
    my $class = shift;
    my ($arg) = @_;

    return LLSD($arg)
}


###########################
#
# Custom LLSD type classes
#
###########################

=head1 TYPES

To represent the LLSD type system, several new type
classes are included.

All types offer the following conversion methods:

    $llsd->asUndefined()
    $llsd->asBoolean()
    $llsd->asInteger()
    $llsd->asReal()
    $llsd->asString()
    $llsd->asUUID()
    $llsd->asURI()
    $llsd->asBinary()
    $llsd->asDate()

If the type does not support the conversion, the
result is undef. Otherwise, the result will be a scalar
value.

All types also offer appropriate implicit conversions
to either boolean, string, or numeric values via the
L<overload> module, so they may be used in place of
scalar values.

=over

=cut

# *TODO: Should these return an LLSD-typed value?

# ------------------------------------------------------------
package LLSD::Type; # Base type for LLSD type classes
use strict;
use warnings;

# Default explicit conversions for LLSD type classes
sub asUndefined($) { return undef; }
sub asBoolean($) { return undef; }
sub asInteger($) { return undef; }
sub asReal($) { return undef; }
sub asString($) { return undef; }
sub asUUID($) { return undef; }
sub asURI($) { return undef; }
sub asBinary($) { return undef; }
sub asDate($) { return undef; }

# ------------------------------------------------------------
package LLSD::String;
use strict;
use warnings;
use MIME::Base64;
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = '' unless defined $arg;

    my $self = "" . $arg; # string scalar
    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload '""' => sub { "" . ${$_[0]}; }, fallback => 1;

# Conversions
sub asString($) { return ${$_[0]}; }
sub asBoolean($) { return ${$_[0]} eq 'true'; } # *TODO: Spec update pending?
sub asReal($) { return 0 + ${$_[0]}; }
sub asInteger($) { return 0 + LLSD::Integer->new(${$_[0]}); }
sub asBinary($) { return decode_base64(${$_[0]}); } # *TODO: pending spec update
sub asUUID($) { return LLSD::UUID->new(${$_[0]}) || LLSD::UUID->new(); }
sub asURI($) { return LLSD::URI->new(${$_[0]}) || LLSD::URI->new(); }
sub asDate($) { return LLSD::Date->new(${$_[0]}) || LLSD::Date->new(); }

=item LLSD::String

    $llsd = new LLSD::String();
    $llsd = new LLSD::String($value);

The default is the empty string.

An implicit string conversion is provided.

The following explicit conversions are supported:

    $llsd->asBoolean()
        Truthy if "true", falsy otherwise.

    $llsd->asInteger()
        Coerced to a 32-bit signed integer numeric scalar, 0 if invalid.

    $llsd->asReal()
        Coerced to a Perl numeric scalar, a NaN value if invalid.

    $llsd->asString()
        Returns itself.

    $llsd->asUUID()
        String's value as a UUID, or the default LLSD::UUID if invalid.

    $llsd->asURI()
        URI corresponding to the string, or the default LLSD::URI if invalid.

    $llsd->asBinary()
        Result of base64-decoding the string, or default LLSD::Binary if invalid.

    $llsd->asDate()
        Result of interpreting the string as ISO 8601 Date Time value, or default LLSD::Date if invalid.

=cut

# ------------------------------------------------------------
package LLSD::Boolean;
use strict;
use warnings;
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = 0 unless defined $arg;
    $arg = 0 if $arg =~ /^false$/i;

    # don't actually return new instances
    return $arg ? LLSD::True : LLSD::False;
}
use overload
    'bool' => sub { 0 + ${$_[0]}; },
    fallback => 1;

# Conversions
sub asBoolean($) { return !!${$_[0]}; }
sub asInteger($) { return ${$_[0]} ? 1 : 0; }
sub asReal($) { return ${$_[0]} ? 1 : 0; }
sub asString($) { return ${$_[0]} ? 'true' : ''; } # *TODO: Spec update pending?

=item LLSD::Boolean

    $llsd = new LLSD::Boolean();
    $llsd = new LLSD::Boolean($value);

Always either LLSD::True or LLSD::False. The default is LLSD::False.

An implicit boolean conversion is provided.

The following explicit conversions are supported:

    $llsd->asBoolean()
        Returns itself.

    $llsd->asInteger()
        1 or 0.

    $llsd->asReal()
        1.0 or 0.0.

    $llsd->asString()
        'true' or ''.

=cut


# ------------------------------------------------------------
package LLSD::Integer;
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = 0 unless defined $arg and looks_like_number($arg);

    my $self = LLSD::_toInt32($arg); # numeric scalar, clamped to 32 bits
    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload
    '0+' => sub { 0 + ${$_[0]}; },
    '""' => sub { '' . ${$_[0]}; },
    fallback => 1;

# Conversions
sub asInteger($) { return ${$_[0]}; }
sub asBoolean($) { return ${$_[0]} != 0; }
sub asReal($) { return ${$_[0]}; }
sub asString($) { return '' . ${$_[0]}; }

=item LLSD::Integer

    $llsd = new LLSD::Integer();
    $llsd = new LLSD::Integer($value);

The default is 0.

An implicit numeric conversion is provided.

The following explicit conversions are supported:

    $llsd->asBoolean()
        Truthy if non-zero, falsy otherwise.

    $llsd->asInteger()
        Returns its value.

    $llsd->asReal()
        Returns its value.

    $llsd->asString()
        Returns Perl's normal stringification of the value.

=cut

# ------------------------------------------------------------
package LLSD::Real;
use strict;
use warnings;
use Scalar::Util qw(looks_like_number);
our @ISA = qw(LLSD::Type);

sub _is_nan($) { $_[0] != $_[0]; }
sub _is_negative_zero($) { $_[0] == 0 and sprintf('%+.f', $_[0]) eq '-0'; }

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = 0 unless defined $arg
        and looks_like_number($arg);

    my $self; # numeric scalar
    if (_is_negative_zero($arg)) {
        $self = $arg; # avoid arithmetic to preserve negative zero
    }
    else {
        $self = 0 + $arg; # force conversion
    }

    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload
    '0+' => sub { ${$_[0]}; },
    '""' => sub { '' . ${$_[0]}; },
    fallback => 1;

# Conversions
sub asReal($) { return ${$_[0]}; }
sub asBoolean { my $n = ${$_[0]}; return $n != 0 && !_is_nan($n); }
sub asInteger($) { return LLSD::_toInt32(${$_[0]}); }
sub asString($) {
    my $n = ${$_[0]};
    return "-0.0" if _is_negative_zero($n);
    return "NaN" if _is_nan($n);
    return "Infinity" if $n == 'inf';
    return "-Infinity" if $n == '-inf';
    return '' . $n;
}

=item LLSD::Real

    $llsd = new LLSD::Real();
    $llsd = new LLSD::Real($value);

The default is 0. Negative zero, positive and negative
infinities, and not-a-number are supported.

An implicit numeric conversion is provided.

The following explicit conversions are supported:

    $llsd->asBoolean()
        Truthy if non-zero, falsy otherwise.

    $llsd->asInteger()
        Returns its value coerced to a 32-bit signed
        integer.

    $llsd->asReal()
        Returns its value.

    $llsd->asString()
        Returns Perl's normal stringification of the value, with
        special cases for negative zero ("-0.0"), positive infinity
        ("Infinity"), negative infinity ("-Infinity") and not-a-number
        ("NaN").

=cut

# ------------------------------------------------------------
package LLSD::URI;
use strict;
use warnings;
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = '' unless defined $arg;

    # undef it not a valid URI
    return undef
        unless $arg =~ m/^(|([A-Za-z][A-Za-z0-9+\-.]*):([A-Za-z0-9\-._~:\/?\#\[\]\@!\$&\'()*+,;=]|%[A-F0-9a-f]{2})+)$/;

    my $self = "" . $arg; # string scalar
    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload '""' => sub { "" . ${$_[0]}; }, fallback => 1;

# Conversions
sub asURI($) { return ${$_[0]}; }
sub asString($) { return ${$_[0]}; }

=item LLSD::URI

    $llsd = new LLSD::URI();
    $llsd = new LLSD::URI($value);

The default is the empty URI. If a value is supplied it must
must match the URI productions from RFC 2396; otherwise, undef
is returned.

An implicit string conversion is provided.

The following explicit conversions are supported:

    $llsd->asURI()
        Returns its value.

    $llsd->asString()
        Returns its value.

=cut

# ------------------------------------------------------------
package LLSD::UUID;
use strict;
use warnings;
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = '00000000-0000-0000-0000-000000000000' unless defined $arg;

    # undef if not a valid UUID
    return undef
        unless $arg =~ m/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

    my $self = "" . lc($arg); # string scalar
    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload '""' => sub { "" . ${$_[0]}; }, fallback => 1;

# Conversions
sub asUUID($) { return ${$_[0]}; }
sub asString($) { return ${$_[0]}; }

sub octets($) {
    return map { hex($_) } (${$_[0]} =~ m/([0-9a-f]{2})/ig);
}

my $nil_uuid = LLSD::UUID->new();
sub Nil() { return $nil_uuid; }

=item LLSD::UUID

    $llsd = new LLSD::UUID();
    $llsd = new LLSD::UUID($value);
    $llsd = LLSD::UUID::Nil;

The default is the nil UUID (all zeros).

If a value is supplied it must must match the UUID productions
from RFC 4122 or undef is returned.

An implicit string conversion is provided.

The following explicit conversions are supported:

    $llsd->asUUID()
        Returns its value, in canonical lowercase form.

    $llsd->asString()
        Returns its value, in canonical lowercase form.

    $llsd->octets()
        Returns a 16-element array of numbers corresponding
        to the octet values of the UUID.

=cut


# ------------------------------------------------------------
package LLSD::Binary;
use strict;
use warnings;
use MIME::Base64;
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    # default if no parameter supplied
    $arg = '' unless defined $arg;

    my $self = "" . $arg; # binary string scalar
    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload '""' => sub { "" . ${$_[0]}; }, fallback => 1;

# Conversions
sub asBinary($) { return ${$_[0]}; }
sub asString($) { # *TODO: pending spec update
    my $enc = encode_base64(${$_[0]});
    $enc =~ s/\s+//g; # Eliminate embedded/trailing whitespace
    return $enc;
}

=item LLSD::Binary

    $llsd = new LLSD::Binary();
    $llsd = new LLSD::Binary($value);

A special type of string, understood to represent a sequence
of octets rather than characters.

An implicit string conversion is provided, which returns
the binary data base64-encoded.

The following explicit conversions are supported:

    $llsd->asBinary()
        Returns the data as a Perl string.

    $llsd->asString()
        Returns the data, encoded via base64.

=cut


# ------------------------------------------------------------
package LLSD::Date;
use strict;
use warnings;
use Date::Parse qw(str2time);
use Scalar::Util qw(looks_like_number);
our @ISA = qw(LLSD::Type);

sub new($;$) {
    my $class = shift;
    my ($arg) = @_;

    my $self; # will be a numeric scalar

    if (!defined $arg) {
        # 1970-01-01T00:00:00Z
        $self = 0;
    }
    elsif (looks_like_number($arg)) {
        # Number (epoch time)
        $self = 0 + $arg;
    }
    elsif ($arg =~ m/^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z$/) {
        # YYYY-MM-DDThh:mm:ss[.mmm]Z
        $self = str2time($arg);
    }
    else {
        return undef;
    }

    my $ref = \$self;
    bless $ref, $class;
    return $ref;
}
use overload
    '0+' => sub { 0 + ${$_[0]}; },
    '""' => sub { $_[0]->asString(); },
    fallback => 1;

# Conversions
sub asDate($) { return ${$_[0]}; }
sub asString($) {
    my ($sec, $min, $hour, $mday, $mon, $year) = gmtime(${$_[0]});
    return sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",
                   $year+1900, $mon+1, $mday, $hour, $min, $sec);
}


=item LLSD::Date

    $llsd = new LLSD::Date();
    $llsd = new LLSD::Date($value);

The input value can be an ISO 8601 date/time string
("YYYY-MM-DDThh:mm:ss[.fff]Z") or a floating point
epoch value.

An implicit string conversion is provided, which
returns an ISO 8601 UTC date/time string.

An implicit numeric conversion is provided, which
returns a time stamp (seconds since the Unix epoch)

The following explicit conversions are supported:

    $llsd->asDate()
        Returns the seconds since the Unix epoch.

    $llsd->asString()
        Returns the ISO 8601 date/time string.

=cut

# ------------------------------------------------------------

=back

=head1 SERIALIZATION

=head2 Binary Serialization

=over

=item Object-oriented interface

    my $serializer = new LLSD::BinarySerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);

=cut


package LLSD::BinarySerializer;
use strict;
use warnings;
use IO::String;

my $is_big_endian = unpack("h*", pack("s", 1)) =~ /01/;
sub hton($) {
    return $is_big_endian ? $_[0] : scalar reverse($_[0]);
}
sub ntoh($) {
    return $is_big_endian ? $_[0] : scalar reverse($_[0]);
}


sub new($;$) {
    my $class = shift;
    my $self = shift || {};
    bless $self, $class;
    return $self;
}

sub format($$) {
    my ($self, $data) = @_;

    my $bindata;
    my $io = new IO::String($bindata);
    _format_value($self, $data, $io);
    return $bindata
}


sub _format_value($$$) {
    my ($self, $data, $io) = @_;
    my $type = LLSD::type($data, $self->{'guess_types'});

    if ($type eq 'undef') {
        print $io '!';
    }
    elsif ($type eq 'boolean') {
        print $io $data ? '1' : '0';
    }
    elsif ($type eq 'integer') {
        print $io 'i', hton(pack('l', $data));
    }
    elsif ($type eq 'real') {
        print $io 'r', hton(pack('d', $data));
    }
    elsif ($type eq 'string') {
        print $io 's';
        $self->_format_string($data, $io);
    }
    elsif ($type eq 'uuid') {
        $data = new LLSD::UUID($data); # in case it was guessed
        print $io 'u', pack('CCCCCCCCCCCCCCCC', $data->octets());
    }
    elsif ($type eq 'date') {
        $data = new LLSD::Date($data); # in case it was guessed
        print $io 'd', hton(pack('d', 0 + $data));
    }
    elsif ($type eq 'uri') {
        print $io 'l';
        $self->_format_string($data, $io);
    }
    elsif ($type eq 'binary') {
        use bytes;
        my $bin = '' . $data;
        print $io 'b', pack('N', length($bin)), $bin;
    }
    elsif ($type eq 'array') {
        print $io '[';
        print $io pack('N', scalar @{$data});

        foreach my $item (@{$data}) {
            $self->_format_value($item, $io);
        }
        print $io ']';
    }
    elsif ($type eq 'map') {
        my @keys = sort keys %{$data}; # sorted for test stability
        print $io '{';
        print $io pack('N', scalar @keys);

        foreach my $key (@keys) {
            print $io 'k';
            $self->_format_string($key, $io);

            $self->_format_value($data->{$key}, $io);
        }
        print $io '}';
    }
}

sub _format_string($$$) {
    my ($self, $string, $io) = @_;

    utf8::encode($string);
    print $io pack('N', length($string)), $string;
}

sub parsefile($$) {
    my ($self, $filename) = @_;

    my $io = new IO::File($filename, 'r');
    $io->binmode();

    my $llsd = $self->_parse($io);
    die "Unexpected continuation of data\n" unless $io->eof();
    return $llsd;
}

sub parse($$) {
    my ($self, $data) = @_;

    my $io = new IO::String($data);
    $io->binmode();

    my $llsd = $self->_parse_value($io);
    die "Unexpected continuation of data\n" unless $io->eof();
    return $llsd;
}

sub _parse_value($$) {
    my ($self, $io) = @_;

    die "Unexpected EOF\n" if $io->eof();

    my $code = $io->getc();
    if ($code eq '!') {
        return LLSD::Undefined;
    }
    elsif ($code eq '1') {
        return LLSD::True;
    }
    elsif ($code eq '0') {
        return LLSD::False;
    }
    elsif ($code eq 'i') {
        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, 4) == 4;
        return new LLSD::Integer(unpack('l', ntoh($bin)));
    }
    elsif ($code eq 'r') {
        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, 8) == 8;
        return new LLSD::Real(unpack('d', ntoh($bin)));
    }
    elsif ($code eq 's') {
        return new LLSD::String($self->_parse_string($io));
    }
    elsif ($code eq 'u') {
        my $octets;
        die "Unexpected EOF\n" unless $io->read($octets, 16) == 16;
        my @octets = unpack('C[16]', $octets);
        my $str = sprintf('%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x', @octets);
        return new LLSD::UUID($str);
    }
    elsif ($code eq 'd') {
        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, 8) == 8;
        return new LLSD::Date(unpack('d', ntoh($bin)));
    }
    elsif ($code eq 'l') {
        return new LLSD::URI($self->_parse_string($io));
    }
    elsif ($code eq 'b') {
        my $len = $self->_parse_ulong($io);

        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, $len) == $len;
        return new LLSD::Binary($bin);
    }
    elsif ($code eq '[') {
        my $len = $self->_parse_ulong($io);

        my @array = ();
        for (my $i = 0; $i < $len; $i++) {
            push @array, $self->_parse_value($io);
        }

        die "Unexpected EOF\n" if $io->eof();
        die "Expected ]\n" unless $io->getc() eq ']';
        return \@array;
    }
    elsif ($code eq '{') {
        my $len = $self->_parse_ulong($io);

        my %map = ();
        for (my $i = 0; $i < $len; $i++) {
            die "Expected key\n" unless $io->getc() eq 'k';
            my $key = $self->_parse_string($io);
            my $value = $self->_parse_value($io);
            $map{$key} = $value;
        }

        die "Unexpected EOF\n" if $io->eof();
        die "Expected }\n" unless $io->getc() eq '}';
        return \%map;
    }
    else {
        die "Unknown tag: $code\n";
    }
}

sub _parse_ulong($$) {
    my ($self, $io) = @_;
    my $len;
    die "Unexpected EOF\n" unless $io->read($len, 4) == 4;
    return unpack('N', $len);
}

sub _parse_string($$) {
    my ($self, $io) = @_;
    my $len = $self->_parse_ulong($io);

    my $str;
    die "Unexpected EOF\n" unless $io->read($str, $len) == $len;
    utf8::decode($str) or die "Invalid UTF-8 sequence\n";

    return $str;
}


=item Convenience functions

    my $data = LLSD::parse_binary_file('myfile.bin');
    my $data = LLSD::parse_binary("i\x00\x00\x00\x7b");
    my $bin = LLSD::format_binary($llsd [, GUESS_TYPES ] );

=cut

package LLSD;

#
# Push convenience functions into LLSD namespace
#

sub parse_binary($) {
    my ($bin) = @_;
    my $serializer = new LLSD::BinarySerializer;
    return $serializer->parse($bin);
}

sub parse_binary_file($) {
    my ($binfile) = @_;
    my $serializer = new LLSD::BinarySerializer;
    return $serializer->parsefile($binfile);
}

sub format_binary($;$) {
    my ($data, $guess_types) = @_;
    my $serializer = new LLSD::BinarySerializer({guess_types => $guess_types});
    return $serializer->format($data);
}


=back

=head2 XML Serialization

=over

=item Object-oriented interface

    my $serializer = new LLSD::XMLSerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);

=cut

package LLSD::XMLSerializer;
use strict;
use warnings;
use XML::DOM;

sub new($;$) {
    my $class = shift;
    my $self = shift || {};
    bless $self, $class;
    return $self;
}

sub format($$) {
    my ($self, $data) = @_;
    my $doc = XML::DOM::Document->new;
    my $root_node = $doc->createElement("llsd");
    $doc->appendChild($root_node);

    my $node = $self->_format_value($data, $doc);
    if (defined $node) {
        $root_node->appendChild($node);
    }

    return $doc->toString();
}

sub _format_value($$$) {
    my ($self, $data, $doc) = @_;

    my $type = LLSD::type($data, $self->{'guess_types'});

    return $doc->createElement("undef") if $type eq 'undef';
    return $self->_format_array($data, $doc) if $type eq 'array';
    return $self->_format_map($data, $doc) if $type eq 'map';

    my $node = $doc->createElement($type);
    $node->insertBefore($doc->createTextNode($data));
    return $node;
}

sub _format_map($$$) {
    my ($self, $data, $doc) = @_;
    my $map_node = $doc->createElement("map");

    my @keys = keys %{$data};
    foreach my $key (@keys) {
        # key pair
        my $child = $doc->createElement("key");
        $child->insertBefore($doc->createTextNode($key));
        $map_node->insertBefore($child);

        # value pair
        $child = $self->_format_value($data->{$key}, $doc);
        if (defined $child) {
            $map_node->insertBefore($child);
        }
    }
    return $map_node;
}

sub _format_array($$$) {
    my ($self, $data, $doc) = @_;
    my $array_node = $doc->createElement("array");

    my $element;
    foreach $element (@{$data}) {
        my $child_node = $self->_format_value($element, $doc);
        if (defined $child_node) {
            $array_node->insertBefore($child_node);
        }
    }
    return $array_node;
}


sub parsefile($$) {
    my ($self, $filename) = @_;

    # try to parse the xml file
    my $parser = new XML::DOM::Parser;
    my $doc = eval {
        $parser->parsefile($filename);
    };
    if ($@) {
        die "Unable to parse XML String\n";
    }
    my $llsd = _parse_doc($doc);
    $doc->dispose;
    return $llsd;
}


sub parse($$) {
	my ($self, $xml) = @_;

    # try to parse the xml file
    my $parser = new XML::DOM::Parser;
    my $doc = eval {
        $parser->parse($xml);
    };
    if ($@) {
        die "Unable to parse XML file\n";
    }
    my $llsd = _parse_doc($doc);
    $doc->dispose;
    return $llsd;
}


# Returns undef on error.
sub _parse_doc($) {
    my ($doc) = @_;

    # gather nodes
    my $node = $doc->getDocumentElement();
    if ($node->getNodeName() ne 'llsd') {
        die "XML file with root node ", $node->getNodeName(),
            " does not have root element llsd\n";
    }

    my @nodes = $node->getElementsByTagName('*', 0);
    if (@nodes == 0 || @nodes > 1) {
        die "LLSD can only contain one element, but requested node has ",
            scalar @nodes, " elements.\n";
    }
    return _parse_node($nodes[0]);

    # Avoid memory leaks - cleanup circular references for garbage collection
    $doc->dispose();
}


# If the XML parses correctly, returns a representation
# of the current node using the LLSD type classes.
# If the XML does not parse correctly, returns undef.
sub _parse_node($); # forward declaration, for recursion
sub _parse_node($) {
    my ($node) = @_;

    # we have an xml element for representation.  Check types and format
    my $type = $node->getNodeName();
    if ($type eq 'array') {
        return _parse_array($node->getElementsByTagName('*', 0));
    }
    elsif ($type eq 'map') {
        return _parse_map($node->getElementsByTagName('*', 0));
    }
    else {
        # scalar from this point forward
        my $children_list = $node->getChildNodes();

        my $value;
        my $node_count = $children_list->getLength();
        if ($node_count == 0) {
            # <type /> means the default for that type
            # <integer /> means 0
            # <real /> means 0.0
            # <undef /> means undef
            $value = undef;
        }
        elsif ($node_count == 1) {
            $value = $children_list->[0]->getNodeValue();
        }
        else {
            die "Invalid number of nodes for scalar: $node_count\n";
        }

        if ($type eq 'undef') {
            if (!defined $value) {
                return undef;
            }
            else {
                die "Error parsing undef tag\n";
            }
        }
        elsif ($type eq 'boolean') {
            if (!defined $value) {
                return LLSD::False;
            }
            elsif ($value eq 'true' || $value eq '1') {
                return LLSD::True;
            }
            elsif ($value eq 'false' || $value eq '0') {
                return LLSD::False;
            }

            die "Invalid format for Boolean LLSD: ", $value, "\n";
        }
        elsif ($type eq 'integer') {
            if (!defined $value) {
                return LLSD::Integer->new();
            }
            elsif ($value =~ /^-?\d+$/) {
                return LLSD::Integer->new($value);
            }

            die "Invalid format for Integer LLSD: ", $value, "\n";
        }
        elsif ($type eq 'real') {
            if (!defined $value) {
                return LLSD::Real->new();
            }
            elsif ($value =~ /^-?\d+(\.\d+)?$/ ||
                   $value =~ /^-?\.\d+$/ ||
                   $value =~ /^[+-]?Infinity|NaN$/) {
                return LLSD::Real->new($value);
            }

            die "Invalid format for Real LLSD: ", $value, "\n";
        }
        elsif ($type eq 'uuid') {
            if (!defined $value) {
                return LLSD::UUID->new();
            }
            elsif ($value =~ /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/) {
                return LLSD::UUID->new($value);
            }

            die "Invalid format for UUID LLSD: ", $value, "\n";
        }
        elsif ($type eq 'date') {
            if (!defined $value) {
                return LLSD::Date->new();
            }
            elsif ($value =~ m/^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z$/) {
                return LLSD::Date->new($value);
            }

            die "Invalid format for Date LLSD: ", $value, "\n";
        }
        elsif ($type eq 'string') {
            if (!defined $value) {
                return LLSD::String->new();
            }
            else {
                return LLSD::String->new($value);
            }
        }
        elsif ($type eq 'uri') {
            if (!defined $value) {
                return LLSD::URI->new();
            }
            else {
                return LLSD::URI->new($value);
            }
        }
        elsif ($type eq 'binary') {
            if (!defined $value) {
                return LLSD::Binary->new();
            }
            else {
                return LLSD::String->new($value)->asBinary();
            }
        }

        die "Invalid Type Given for LLSD",
            (defined $type ? ' ' . $type : ''),
                "\n";
    }
}

sub _parse_array($) {
    my (@children) = @_;
    my @return_value;
    my $child;
    while ($child = shift @children) {
        my $value = _parse_node($child);
        push @return_value, $value;
    }
    return \@return_value;
}


sub _parse_map($) {
    my (@children) = @_;
    my %return_value;

    while (scalar @children) {
        my $child = shift @children;
        if ($child->getNodeName() ne 'key') {
            die "Error parsing LLSD map: expected key, saw ", $child->getNodeName(), "\n";
        }
        my $key = $child->getFirstChild()->getNodeValue();

        $child = shift @children;
        if (!defined $child) {
            die "Error parsing LLSD map: expected value\n";
        }

        $return_value{$key} = _parse_node($child);
    }
    return \%return_value;
}


=item Convenience functions

    my $data = LLSD::parse_xml_file('myfile.xml');
    my $data = LLSD::parse_xml('<llsd><integer>123</integer></llsd>');
    my $xml = LLSD::format_xml($llsd [, GUESS_TYPES ] );

=cut

package LLSD;

#
# Push convenience functions into LLSD namespace
#

sub parse_xml($) {
    my ($xml) = @_;
    my $serializer = new LLSD::XMLSerializer;
    return $serializer->parse($xml);
}

sub parse_xml_file($) {
    my ($xml) = @_;
    my $serializer = new LLSD::XMLSerializer;
    return $serializer->parsefile($xml);
}

sub format_xml($;$) {
    my ($data, $guess_types) = @_;
    my $serializer = new LLSD::XMLSerializer({guess_types => $guess_types});
    return $serializer->format($data);
}


=back

=head1 AUTHOR & COPYRIGHT

Copyright (c) 2010, Linden Research, Inc.

See package for full license details.

=cut


# ------------------------------------------------------------
1; # end of module
