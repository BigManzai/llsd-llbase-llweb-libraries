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

package LLSD::BinarySerializer;

use 5.008000;
use strict;
use warnings;

require Exporter;

our $VERSION = 3.01;
our @ISA = qw(Exporter);
our @EXPORT = qw();
our @EXPORT_OK = qw();

use LLSD;

use IO::File;
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

    my $bindata = '';
    my $io = new IO::String($bindata);
    _format_value($self, $data, $io);
    return $bindata;
}


sub _format_value($$$) {
    my ($self, $data, $io) = @_;
    my $type = LLSD::type($data, $self->{'guess_types'});

    # This is a no-op if it's already LLSD
    $data = new LLSD($data, $type);

    if ($type eq 'undefined') {
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
        my @octets = map { hex($_) } ($data =~ m/([0-9a-f]{2})/ig);
        print $io 'u', pack('CCCCCCCCCCCCCCCC', @octets);
    }
    elsif ($type eq 'date') {
        print $io 'd', hton(pack('d', 0 + $data));
    }
    elsif ($type eq 'uri') {
        print $io 'l';
        $self->_format_string($data, $io);
    }
    elsif ($type eq 'binary') {
        use bytes;
        my $bin = '' . $data->value;
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
        return new LLSD(unpack('l', ntoh($bin)), 'integer');
    }
    elsif ($code eq 'r') {
        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, 8) == 8;
        return new LLSD(unpack('d', ntoh($bin)), 'real');
    }
    elsif ($code eq 's') {
        return new LLSD($self->_parse_string($io), 'string');
    }
    elsif ($code eq 'u') {
        my $octets;
        die "Unexpected EOF\n" unless $io->read($octets, 16) == 16;
        my @octets = unpack('C[16]', $octets);
        my $str = sprintf('%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x', @octets);
        return new LLSD($str, 'uuid');
    }
    elsif ($code eq 'd') {
        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, 8) == 8;
        return new LLSD(unpack('d', ntoh($bin)), 'date');
    }
    elsif ($code eq 'l') {
        return new LLSD($self->_parse_string($io), 'uri');
    }
    elsif ($code eq 'b') {
        my $len = $self->_parse_ulong($io);

        my $bin;
        die "Unexpected EOF\n" unless $io->read($bin, $len) == $len;
        return new LLSD($bin, 'binary');
    }
    elsif ($code eq '[') {
        my $len = $self->_parse_ulong($io);

        my $array = new LLSD::Array();
        for (my $i = 0; $i < $len; $i++) {
            push @$array, $self->_parse_value($io);
        }

        die "Unexpected EOF\n" if $io->eof;
        die "Expected ]\n" unless $io->getc() eq ']';
        return $array;
    }
    elsif ($code eq '{') {
        my $len = $self->_parse_ulong($io);

        my $map = new LLSD::Map();
        for (my $i = 0; $i < $len; $i++) {
            die "Expected key\n" unless $io->getc() eq 'k';
            my $key = $self->_parse_string($io);
            my $value = $self->_parse_value($io);
            $map->{$key} = $value;
        }

        die "Unexpected EOF\n" if $io->eof;
        die "Expected }\n" unless $io->getc() eq '}';
        return $map;
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


1; # End of module
__END__
