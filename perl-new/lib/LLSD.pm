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

use 5.008000;
use strict;
use warnings;

require Exporter;

our $VERSION = 3.01;
our @ISA = qw(Exporter);
our @EXPORT = qw();
our @EXPORT_OK = qw(True False Undefined);

=head1 NAME

LLSD - Abstract type system implementation for Perl

=head1 SYNOPSIS

  use LLSD;
  use LLSD qw(True False Undefined);

  # Creation

  $llsd = new LLSD('abc');
  $llsd = new LLSD(123, 'integer'); # scalars default to strings
  $llsd = new LLSD({a => 'hello', b => 'world'});
  $llsd = LLSD::True;

  print new LLSD::Integer(1, "integer") + 2; # -> 3


  # Types

  print new LLSD(123, 'integer')->type; # -> 'integer'
  print LLSD::type('abc'); # -> 'string'
  print LLSD::type(1); # -> 'string'
  print LLSD::type({}); # -> 'map'
  $GUESS = 1;
  print LLSD::type(1, $GUESS); # -> 'integer'
  print LLSD::type(1.2, $GUESS); # -> 'real'


  # Serialization Functions

  my $data = LLSD::parse_xml_file('myfile.xml');
  my $data = LLSD::parse_xml('<llsd><integer>123</integer></llsd>');
  my $xml = LLSD::format_xml($llsd);

  my $data = LLSD::parse_binary_file('myfile.bin');
  my $data = LLSD::parse_binary("i\x00\x00\x00\x7b");
  my $bin = LLSD::format_binary($llsd);

  my $data = LLSD::parse_json_file('myfile.json');
  my $data = LLSD::parse_json('{"a": "q", "b": [1, 2, 3]}');
  my $bin = LLSD::format_json($llsd);

  my $data = LLSD::parse_notation_file('myfile.notation');
  my $data = LLSD::parse_notation('{s"a": s"q", s"b": [i1, i2, i3]}');
  my $bin = LLSD::format_notation($llsd);


  # Object-Oriented Serialization

  use LLSD::XMLSerializer;
  my $serializer = new LLSD::XMLSerializer({guess_types=>GUESS});

  use LLSD::BinarySerializer;
  my $serializer = new LLSD::BinarySerializer({guess_types=>GUESS});

  use LLSD::JSONSerializer;
  my $serializer = new LLSD::JSONSerializer({guess_types=>GUESS});

  use LLSD::NotationSerializer;
  my $serializer = new LLSD::NotationSerializer({guess_types=>GUESS});

  my $serialized = $serializer->format($llsd);
  my $llsd = $serializer->parse($serialized);
  my $llsd = $serializer->parsefile($filename);


=head1 DESCRIPTION

LLSD is an abstract type system for RESTful data services. It provides
a small number of commonly used simple types (Strings, Integers, Reals,
UUIDs, URIs, Dates, and Binary data) and composite types (Maps, Arrays)
common to most modern programming languages.

The type system provides well defined conversion rules between types
and default values.

Several serialization formats are defined for LLSD, including XML,
Binary and JSON.

This module provides functionality for LLSD serialization (reading and
writing) and maintaining types in Perl. For example, it is possible
to create a data structure with typed numbers and strings and ensure
they are serialized with the correct LLSD types to an XML or Binary stream.
Thanks to the L<overload> module, the typed values can be used in nearly
all cases where scalars might be used.

=cut


use Scalar::Util qw(looks_like_number);
use Date::Parse qw(str2time);


my %_types = map { $_ => 1 } qw(undefined boolean integer real string uri uuid binary date array map);


=head1 LLSD VALUES

=head2 LLSD Constructor

LLSD values are initialized using the constructor:

  $llsd = new LLSD( scalar [, type ] )
  $llsd = new LLSD( arrayref )
  $llsd = new LLSD( hashref )

For scalar values the result will be a reference blessed as C<LLSD>. 

Since Perl does not distinguish scalar types, LLSD-specific types
can be specified by name. For example:

  $llsd = new LLSD(123, 'integer');
  $llsd = new LLSD(45.67, 'real');
  $llsd = new LLSD('http://example.com', 'uri');

For array and hash references the result will be a new array or hash
reference tied as C<LLSD::Array> or C<LLSD::Map> respectively. Fetching
values from the container (by index or key) will yield LLSD values. 
Storing values in a container that are not already LLSD values will 
wrap them automatically (see below for type inference rules).

  $llsd = new LLSD({a=>'test'});
  print $llsd->{a}; # -> "test"
  print $llsd->{a}->type; # -> "string"
  $llsd->{b} = new LLSD(123, 'integer');
  print $llsd->{b}; # -> 123
  print $llsd->{b}->type; # -> "integer"

=cut

sub new($$;$) {
    my $class = shift;
    my ($value, $type) = @_;

    # Don't double-wrap LLSD values
    return $value if UNIVERSAL::isa($value, 'LLSD');
    
    $type = $type || type($value); 

    die "Invalid type: '$type'" unless exists $_types{$type};

    if ($type eq 'map') {
        return new LLSD::Map(%$value);
    }
    elsif ($type eq 'array') {
        return new LLSD::Array(@$value);
    }
    else {
        # simple
        return new LLSD::Simple($value, $type);
    }
}



=head2 Types

   $type = $llsd->type
   $type = LLSD::type( llsd_value )

   $type = LLSD::type( perl_value )
   $type = LLSD::type( perl_value, GUESS )

The C<LLSD::type> function may be used to determine the LLSD type of a value.
For LLSD values this will be one of:

=over

=item * C<undefined>

=item * C<boolean>

=item * C<integer>

=item * C<real>

=item * C<string>

=item * C<uri>

=item * C<uuid>

=item * C<binary>

=item * C<date>

=item * C<array>

=item * C<map>

=back

Perl array references and hash references map to C<array> and C<map> types
respectively, and undef maps to C<undefined>. By default, scalar values
map to C<string> as these are not distinguished by Perl.

Passing in a true value for the second parameter activates guessing which 
may yield the following types:

=over

=item * C<uuid> - values that match the 8-4-4-4-12 format defined in RFC 4122.

=item * C<date> - values that match the ISO 8601 format C<YYYY-MM-DDThh:mm:ss[.fff]Z>

=item * C<integer> - values that match C</^-?\d+$/>

=item * C<real> - non-integers that L<Scalar::Util::looks_like_number> approves

=back

You can combine the function with the constructor:

  $GUESS = 1;
  $llsd = new LLSD($value, LLSD::type($value, $GUESS));

Guessing is not recommended when processing data, e.g. reading lines from
a text file, as there are bound to be strings which happen to look like
numbers which would be altered on round trip, for example:

  $str = "1e5"; # an unassuming string?
  $GUESS = 1;
  $out = new LLSD($str, LLSD::type($str, $GUESS));
  print "$str = $out?"; # -> "1e5 = 100000?"; # unexpected!

=cut

sub type($;$) {
    my ($value, $guess) = @_;

    return "undefined" unless defined $value;

    return 'map' if UNIVERSAL::isa($value, 'LLSD::Map');
    return 'array' if UNIVERSAL::isa($value, 'LLSD::Array');

    return $value->{type} if UNIVERSAL::isa($value, 'LLSD');

    return 'map' if UNIVERSAL::isa($value, 'HASH');
    return 'array' if UNIVERSAL::isa($value, 'ARRAY');

    # For any other non-reference type:
    return "undefined" if ref($value);

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

=head2 Type Constructors

Several convenience constructors are provided:

  $bool = new LLSD::Boolean(1);
  $int = new LLSD::Integer(123);
  $real = new LLSD::Real(45.67);
  $string = new LLSD::String('hello, world');
  $uri = new LLSD::URI('http://example.com');
  $uuid = new LLSD::UUID('12345678-abcd-0123-9876-ba9876543210');
  $binary = new LLSD::Binary('\x01\x02\x03\x04');
  $date = new LLSD::Date(time());
  $array = new LLSD::Array('a', 'b', 'c');
  $map = new LLSD::Map('a' => 'b', 'c' => 'd');

These are equivalent to calling the C<LLSD> constructor and
specifying a type name, with extra sugar for the composite types.

=cut


package LLSD::Boolean;
sub new($$) { return LLSD->new($_[1], 'boolean'); }

package LLSD::Integer;
sub new($$) { return LLSD->new($_[1], 'integer'); }

package LLSD::Real;
sub new($$) { return LLSD->new($_[1], 'real'); }

package LLSD::String;
sub new($$) { return LLSD->new($_[1], 'string'); }

package LLSD::URI;
sub new($$) { return LLSD->new($_[1], 'uri'); }

package LLSD::UUID;
sub new($$) { return LLSD->new($_[1], 'uuid'); }
sub Nil { return LLSD->new('', 'uuid'); }

package LLSD::Binary;
sub new($$) { return LLSD->new($_[1], 'binary'); }

package LLSD::Date;
sub new($$) { return LLSD->new($_[1], 'date'); }

package LLSD;

=head2 Defaults

Defaults are used when creating LLSD values using type constructors
with no arguments or when conversions fail. Per the LLSD specification
the defaults are:

=over 

=item * C<boolean> - C<False>

=item * C<integer> - C<0>

=item * C<real> - C<0.0>

=item * C<string> - the empty string (C<"">)

=item * C<uri> - the empty uri (C<"">)

=item * C<uuid> - the nil UUID (C<00000000-0000-0000-0000-000000000000>)

=item * C<binary> - the empty octet sequence

=back

=cut



=head2 Constants

The following constants are defined:

=over 

=item * C<LLSD::True> - the true Boolean value

=item * C<LLSD::False> - the false Boolean value

=item * C<LLSD::Undefined> - an undefined value

=item * C<LLSD::UUID::Nil> - the nil UUID (C<00000000-0000-0000-0000-000000000000>) 

=back

=cut


# Constants
my $true_val = bless { type => 'boolean', value => 1 }, 'LLSD::Simple';
my $false_val = bless { type => 'boolean', value => 0 }, 'LLSD::Simple';
my $undef_val = bless { type => 'undefined', value => undef }, 'LLSD::Simple';
sub True() { return $true_val; }
sub False() { return $false_val; }
sub Undefined() { return $undef_val; }



package LLSD::Simple;

our @ISA = ('LLSD');

use Scalar::Util qw(looks_like_number);
use Date::Parse qw(str2time);

sub new($$$) {
    my ($class, $value, $type) = @_;

    return LLSD::_to_boolean($value) ? LLSD::True : LLSD::False 
        if $type eq 'boolean';

    $value = LLSD::_to_integer($value) 
        if $type eq 'integer';

    $value = LLSD::_to_real($value) 
        if $type eq 'real';

    $value = LLSD::_to_string($value)
        if $type eq 'string';

    $value = LLSD::_to_uri($value) 
        if $type eq 'uri';

    $value = LLSD::_to_uuid($value) 
        if $type eq 'uuid';

    $value = LLSD::_validate_binary($value)
        if $type eq 'binary';

    $value = LLSD::_to_date($value) 
        if $type eq 'date';
    
    return bless {
        type => $type,
        value => $value
    }, $class;
}

=head2 Raw Value

To access the raw value of simple types with no 
conversion, use:

  my $raw = $llsd->type;

This is most useful for Binary, where the default
conversion to String is via base64-encoding, yet
the fundamental type is a byte string. It is 
preferrable to use the appropriate conversion 
(e.g. $llsd->asBinary) instead, as this will
do the right thing when the source data may 
be a different type due to seriaization limitations.

=cut


sub value($) {
    return $_[0]->{value};
}



=head1 CONVERSIONS

LLSD defines conversions between several of the scalar 
types.

=cut 


=head2 Implicit Conversions

The L<overload> module is used to provide implicit conversions
for boolean ('bool'), number ('0+'), and string ('""') scalar contexts
for simple types (i.e. not Map or Array). The following are equivalent:

    print "true" if $llsd->asBoolean();
    print "true" if $llsd;

    print 12.34 + $llsd->asReal();
    print 12.34 + $llsd;

    print 'abc' . $llsd->asString();
    print 'abc' . $llsd;

This means that LLSD values can be used practically anywhere
that scalar values can be used. As a consequence, however,
undefined values will pass a C<defined> test since they will
be implicitly converted to a default string value.

=cut

use overload 
    fallback => 1,
    'bool' => sub { $_[0]->asBoolean() },
    '0+' => sub { $_[0]->asReal() },
    '""' => sub { $_[0]->asString() };


=head2 Explicit Conversions

The result of calling a conversion function will always
be a scalar value of the specified type. If no defined 
conversion exists, the default value for that type will
be returned.

=cut

package LLSD;

use MIME::Base64;

sub asUndefined($) { 
    return undef; 
}


=head3 asBoolean

  $bool = $llsd->asBoolean();

B<Integer>

A zero value (0) is converted to false.  All other values
are converted to true.

B<Real>

A zero value (0.0) and invalid floating point values (NaNs) are
converted to false.  All other values are converted to true.

B<String>

An empty String is converted to false.  Anything else is
converted to true.

=cut

sub asBoolean($) {
    my ($self) = @_;
    my $type = $self->type;

    return $self->value if $type eq 'boolean';
    return $self->value != 0 if $type eq 'integer';
    return ($self->value != 0 && !_is_nan($self->value)) if $type eq 'real';
    return $self->value ne '' if $type eq 'string';

    return LLSD::False;
}


=head3 asInteger

  $int = $llsd->asInteger();

B<Boolean>

The value true is converted to the Integer 1.  The value
false is converted to the Integer 0.

B<Real>

Real are rounded to the nearest representable Integer, with
ties being rounded to the nearest even number.  Invalid floating
point values (NaNs) are converted to the Integer 0.

B<String>

The string is first converted to type Real.
Then the resulting Real is converted to Integer as specified
above.

=cut

sub asInteger($) {
    my ($self) = @_;
    my $type = $self->type;
    
    return $self->value ? 1 : 0 if $type eq 'boolean';
    return $self->value if $type eq 'integer';
    return _to_integer($self->value) if $type eq 'real';
    return _to_integer($self->value) if $type eq 'string';

    return 0;
}


=head3 asReal

  $real = $llsd->asReal();

B<Boolean>

The value true is converted to the floating point value 1.0.
The value false is converted to the floating point value 0.0.

B<Integer>

Integers promoted to floating point values are converted to
the nearest representable number.

B<String>

The numeric parsing conventions for Perl are used, which
handle notations including 'inf', 'Infinity', '-inf', 
'-Infinity', and 'NaN'.

B<Date>

Dates are converted to the floating point value of the
number of seconds since the epoch.

=cut

sub asReal($) {
    my ($self) = @_;
    my $type = $self->type;

    no warnings;

    return $self->value ? 1 : 0 if $type eq 'boolean';
    return $self->value if $type eq 'integer';
    return $self->value if $type eq 'real';
    return _to_real($self->value) if $type eq 'string';
    return 0 + $self->value if $type eq 'date';

    return 0.0;
}


=head3 asString

  $str = $llsd->asString();

B<Boolean>

The value true is represented as the string "true".  The
value false is represented as the empty string ("").

B<Integer>

Integers converted to Strings are represented as signed
decimal representation.

B<Real>

The numeric formatting conventions for Perl are followed, with
special cases for Infinity, -Infinity, -0.0 and NaN.

B<UUID>

UUIDs converted to Strings are represented in the 36 character,
8-4-4-4-12 format defined in RFC 4122.

B<Date>

Dates are converted to Strings using the ISO 8601 format  
C<YYYY-MM-DDThh:mm:ss.fffZ>

B<URI>

URIs converted to Strings are simply Unicode representations of
the URI.

B<Binary>

Binary data converted to Strings are encoded using the
RFC 2045 base64 encoding.

=cut

sub asString($) {
    my ($self) = @_;
    my $type = $self->type;

    return $self->value ? 'true' : '' if $type eq 'boolean';
    return '' . $self->value if $type eq 'integer';
    return _format_real($self->value) if $type eq 'real';
    return $self->value if $type eq 'string';
    return $self->value if $type eq 'uri';
    return $self->value if $type eq 'uuid';
    return _format_binary($self->value) if $type eq 'binary';
    return _format_date($self->value) if $type eq 'date';

    return '';
}


=head3 asUUID

  $uuid = $llsd->asUUID();

B<String>

A valid 8-4-4-4-12 string representation of a UUID is
converted to the UUID it represents.  All other values are
converted to the null UUID (00000000-0000-0000-0000-
000000000000).

=cut

sub asUUID($) { 
    my ($self) = @_;
    my $type = $self->type;
   
    return $self->value if $type eq 'uuid';
    return _to_uuid($self->value) if $type eq 'string';

    return '00000000-0000-0000-0000-000000000000';
}


=head3 asURI

  $uri = $llsd->asURI();

B<String>

The characters of the String data are interpreted as a URI,
if legal.  Other Strings results in the default URI.

=cut

sub asURI($) { 
    my ($self) = @_;
    my $type = $self->type;

    return $self->value if $type eq 'uri';
    return _to_uri($self->value) if $type eq 'string';

    return ''; 
}


=head3 asBinary

  $binary = $llsd->asBinary();

B<String>

Strings which represent a valid base64-encoding are converted to an
octet sequence (byte string). Other strings result in an empty string.

=cut

sub asBinary($) { 
    my ($self) = @_;
    my $type = $self->type;

    return $self->value if $type eq 'binary';
    return _to_binary($self->value) if $type eq 'string';

    return ''; 
}


=head3 asDate

  $date = $llsd->asDate();

B<String>

Strings which match the format C<YYYY-MM-DDThh:mm:ss[.fff]Z> are converted.
Other Strings result in the default date.

=cut

sub asDate($) { 
    my ($self) = @_;
    my $type = $self->type;

    return $self->value if $type eq 'date';
    return _to_date($self->value) if $type eq 'string';

    return 0; 
}


# Conversion helpers
sub _is_nan($) { $_[0] != $_[0]; }

sub _is_finite($) { 
    $_[0] != 'inf' and $_[0] != '-inf' and !_is_nan($_[0]);
}

sub _is_negative_zero($) { 
    no warnings;
    $_[0] == 0 and sprintf('%+.f', $_[0]) eq '-0'; 
}


sub _format_real($) {
    return "NaN" if _is_nan($_[0]);
    return "Infinity" if $_[0] == 'inf';
    return "-Infinity" if $_[0] == '-inf';
    return '-0.0' if _is_negative_zero($_[0]);
    return '' . $_[0];
}


sub _encode_base64($) {
    my $enc = MIME::Base64::encode_base64($_[0]);
    $enc =~ s/\s+//g; # Eliminate embedded/trailing whitespace
    return $enc;
}

sub _decode_base64($) {
    my $b64 = $_[0];
    $b64 =~ s/\s//g;
    return undef unless $b64 =~ m/^[A-Za-z0-9+\/]+={0,2}$/ and (length($b64) % 4) == 0;
    return MIME::Base64::decode_base64($b64);
}

sub _decode_base16($) { 
    my $b16 = $_[0];
    $b16 =~ s/\s//g;
    return undef unless $b16 =~ /^[0-9A-Fa-f]*$/ and (length($b16) % 2) == 0;
    join('', map { chr(hex($_)) } unpack("(A2)*", $b16));
}



sub _format_binary($) {
    return _encode_base64($_[0]);
}

sub _format_date($) {
    # *TODO: Round to milliseconds?
    my ($sec, $min, $hour, $mday, $mon, $year) = gmtime($_[0]);
    my $ms = int(($_[0] * 1000) % 1000);
    return sprintf("%04d-%02d-%02dT%02d:%02d:%02d.%03dZ",
                   $year+1900, $mon+1, $mday, $hour, $min, $sec, $ms);
}

sub _to_boolean($) {
    return 0 unless defined $_[0];

    # Dubious, but matches previous Perl logic
    return !!$_[0] && lc($_[0]) ne 'false';
}

sub _to_integer($) {
    return 0 unless defined $_[0];
    return 0 unless looks_like_number($_[0]);

    my $MIN_INT32 = -2147483648; # -0x80000000
    my $MAX_INT32 =  2147483647; #  0x7fffffff

    my $n = shift;
    $n = 0 + $n;
    return $MIN_INT32 if $n <= $MIN_INT32;
    return $MAX_INT32 if $n >= $MAX_INT32;
    return 0 + sprintf('%d', $n); # sprintf uses round-to-even
}

sub _to_real($) {
    return 0.0 unless defined $_[0];
    return 0.0 unless looks_like_number($_[0]);
    return -0.0 if _is_negative_zero($_[0]);
    return 0.0 + $_[0];
}

sub _to_string($) {
    return '' unless defined $_[0];
    return '' . $_[0];
}

sub _to_uri($) {
    return '' unless defined $_[0];
    return $_[0] =~ m/^(|([A-Za-z][A-Za-z0-9+\-.]*):([A-Za-z0-9\-._~:\/?\#\[\]\@!\$&\'()*+,;=]|%[A-F0-9a-f]{2})+)$/
        ? $_[0]
        : '';
}

sub _to_uuid($) {
    return '00000000-0000-0000-0000-000000000000' unless defined $_[0];
    return $_[0] =~ m/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
        ? lc($_[0])
        : '00000000-0000-0000-0000-000000000000';
}

sub _validate_binary($) {
    return '' unless defined $_[0];
    map { die "Invalid byte value\n" unless ord($_) <= 0xFF } split(//, $_[0]);
    return $_[0];
}

sub _to_binary($) {
    my $b64 = _decode_base64($_[0]);
    return defined $b64 ? $b64 : '';
}

sub _to_date($) {
    return 0 unless defined $_[0]; # 1970-01-01T00:00:00Z
    return 0 + $_[0] if looks_like_number($_[0]) and _is_finite($_[0]); # Number (epoch time)
    return str2time($_[0]) if $_[0] =~ m/^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z$/;
    return 0;
}


package LLSD::Array;

use Tie::Array;
our @ISA = ('Tie::StdArray', 'LLSD');
sub STORE {
    my ($self, $index, $value) = @_;
    $self->[ $index ] = LLSD->new($value);
}
sub FETCH {
    my ($self, $index) = @_;
    return LLSD::Undefined if $index < 0 or $index >= scalar(@$self);
    return $self->[$index];
}

sub new {
    my $class = shift;
    my @self;
    tie @self, $class;
    @self = @_;
    return bless \@self, $class;
}


package LLSD::Map;

use Tie::Hash;
our @ISA = ('Tie::StdHash', 'LLSD');
sub STORE {
    my ($self, $key, $value) = @_;
    $self->{$key} = LLSD->new($value);
}
sub FETCH {
    my ($self, $key) = @_;
    return LLSD::Undefined unless exists $self->{$key};
    return $self->{$key};
}

sub new {
    my $class = shift;
    my %self;
    tie %self, $class;
    %self = @_;
    return bless \%self, $class;
}


package LLSD;

# ------------------------------------------------------------

=head1 SERIALIZATION

=head2 Binary Serialization

Although not required by the spec, this library outputs
Maps with keys sorted in lexical order for ease of testing.
This behavior should not be relied upon for interop with
other implementations, however.

=head3 Object-oriented interface

    use LLSD::BinarySerializer;
    my $serializer = new LLSD::BinarySerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);

The C<format> function can take a LLSD value or (for convenience)
a native value (undef, scalar, array reference or hash reference). If
a native value is specified type guessing may be enabled, but this is
not recommended (see above).

=head3 Convenience functions

These are exposed by the LLSD module and load the serializer lazily.

    my $data = LLSD::parse_binary_file('myfile.bin');
    my $data = LLSD::parse_binary("i\x00\x00\x00\x7b");
    my $bin = LLSD::format_binary($llsd [, GUESS_TYPES ] );

=cut

sub parse_binary($) {
    require "LLSD/BinarySerializer.pm";
    return LLSD::BinarySerializer->new->parse($_[0]);
}

sub parse_binary_file($) {
    require "LLSD/BinarySerializer.pm";
    return LLSD::BinarySerializer->new->parsefile($_[0]);
}

sub format_binary($;$) {
    require "LLSD/BinarySerializer.pm";
    my ($data, $guess_types) = @_;
    my $serializer = LLSD::BinarySerializer->new({guess_types => $guess_types});
    return $serializer->format($data);
}


=head2 XML Serialization


=head3 Object-oriented interface

    use LLSD::XMLSerializer;
    my $serializer = new LLSD::XMLSerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);

The C<format> function can take a LLSD value or (for convenience)
a native value (undef, scalar, array reference or hash reference). If
a native value is specified type guessing may be enabled, but this is
not recommended (see above).

=head3 Convenience functions

These are exposed by the LLSD module and load the serializer lazily.

    my $data = LLSD::parse_xml_file('myfile.xml');
    my $data = LLSD::parse_xml('<llsd><integer>123</integer></llsd>');
    my $xml = LLSD::format_xml($llsd [, GUESS_TYPES ] );

=cut

sub parse_xml($) {
    require "LLSD/XMLSerializer.pm";
    return LLSD::XMLSerializer->new->parse($_[0]);
}

sub parse_xml_file($) {
    require "LLSD/XMLSerializer.pm";
    return LLSD::XMLSerializer->new->parsefile($_[0]);
}

sub format_xml($;$) {
    require "LLSD/XMLSerializer.pm";
    my ($data, $guess_types) = @_;
    my $serializer = LLSD::XMLSerializer->new({guess_types => $guess_types});
    return $serializer->format($data);
}

=head2 JSON Serialization


=head3 Object-oriented interface

    use LLSD::JSONSerializer;
    my $serializer = new LLSD::JSONSerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);

The C<format> function can take a LLSD value or (for convenience)
a native value (undef, scalar, array reference or hash reference). If
a native value is specified type guessing may be enabled, but this is
not recommended (see above).

Note that serializing LLSD as JSON is "lossy" - not all LLSD types
can be represented in JSON directly. Per the specification, the mapping 
from LLSD to JSON is as follows:

=over

=item * B<Undefined> - C<null>

=item * B<True> - C<true>

=item * B<False> - C<false>

=item * B<Integer> - B<number>

=item * B<Real> - I<number>, C<"Infinity">, C<"-Infinity">, C<"-0.0"> or C<"NaN">

=item * B<String> - B<string>

=item * B<URI> - B<string>

=item * B<UUID> - B<string> - 8-4-4-4-12 format

=item * B<Binary> - B<string> - (base64 encoded)

=item * B<Date> - B<string> - C<"YYYY-MM-DDThh:mm:ss.fffZ">

=item * B<Array> - B<array>

=item * B<Map> - B<object>

=back

The mapping from JSON to LLSD is as follows:

=over

=item * C<null> - B<Undefined>

=item * C<true> - B<True>

=item * C<false> - B<False>

=item * B<number> - B<Real>

=item * B<string> - B<String>

=item * B<array> - B<Array>

=item * B<object> - B<Map>

=back

Note that with the recommended usage of pattern of LLSD, this lossy 
nature is transparent, e.g.:

  $llsd = new LLSD::Map;
  $llsd->{count} = new LLSD::Integer(123);
  $llsd->{ego} = new LLSD::Real('inf');
  $llsd->{id} = new LLSD::UUID('12345678-1234-1234-1234-1234567890ab');
  $json = LLSD::format_json($llsd);

  # ...

  $llsd = LLSD::parse_json($json);
  print $llsd->{count}->asInteger;
  print $llsd->{ego}->asReal;
  print $llsd->{id}->asUUID;

=head3 Convenience functions

These are exposed by the LLSD module and load the serializer lazily.

    my $data = LLSD::parse_json_file('myfile.bin');
    my $data = LLSD::parse_json('{"a": "q", "b": [1, 2, 3]}');
    my $bin = LLSD::format_json($llsd [, GUESS_TYPES ] );

=cut


sub parse_json($) {
    require "LLSD/JSONSerializer.pm";
    return LLSD::JSONSerializer->new->parse($_[0]);
}

sub parse_json_file($) {
    require "LLSD/JSONSerializer.pm";
    return LLSD::JSONSerializer->new->parsefile($_[0]);
}

sub format_json($;$) {
    require "LLSD/JSONSerializer.pm";
    my ($data, $guess_types) = @_;
    my $serializer = LLSD::JSONSerializer->new({guess_types => $guess_types});
    return $serializer->format($data);

}


=head2 Notation Serialization


=head3 Object-oriented interface

    my $serializer = new LLSD::NotationSerializer({guess_types=>GUESS});

    my $serialized = $serializer->format($llsd);
    my $llsd = $serializer->parse($serialized);
    my $llsd = $serializer->parsefile($filename);

The C<format> function can take a LLSD value or (for convenience)
a native value (undef, scalar, array reference or hash reference). If
a native value is specified type guessing may be enabled, but this is
not recommended (see above).

=head3 Convenience functions

These are exposed by the LLSD module and load the serializer lazily.

    my $data = LLSD::parse_notation_file('myfile.bin');
    my $data = LLSD::parse_notation('{"a": "q", "b": [1, 2, 3]}');
    my $bin = LLSD::format_notation($llsd [, GUESS_TYPES ] );

=cut

sub parse_notation($) {
    require "LLSD/NotationSerializer.pm";
    return LLSD::NotationSerializer->new->parse($_[0]);
}

sub parse_notation_file($) {
    require "LLSD/NotationSerializer.pm";
    return LLSD::NotationSerializer->new->parsefile($_[0]);
}

sub format_notation($;$) {
    require "LLSD/NotationSerializer.pm";
    my ($data, $guess_types) = @_;
    my $serializer = LLSD::NotationSerializer->new({guess_types => $guess_types});
    return $serializer->format($data);
}

=head1 AUTHOR & COPYRIGHT

Copyright (c) 2010, Linden Research, Inc.

See package for full license details.

=cut

1; # End of module
__END__
