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

package LLSD::JSONSerializer;

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


sub new($;$) {
    my $class = shift;
    my $self = shift || {};
    bless $self, $class;
    return $self;
}

sub format($$) {
    my ($self, $data) = @_;

    my $jsondata = '';
    my $io = new IO::String($jsondata);

    $io->binmode(':utf8');
    _format_value($self, $data, $io);
    utf8::decode($jsondata);

    return $jsondata
}


sub _format_value($$$) {
    my ($self, $data, $io) = @_;
    my $type = LLSD::type($data, $self->{'guess_types'});

    # This is a no-op if it's already LLSD
    $data = new LLSD($data, $type);

    if ($type eq 'undefined') {
        print $io 'null';
    }
    elsif ($type eq 'boolean') {
        print $io $data ? 'true' : 'false';
    }
    elsif ($type eq 'integer') {
        print $io 0 + $data;
    }
    elsif ($type eq 'real') {
        if ($data == 'inf' or
            $data == '-inf' or
            LLSD::_is_nan($data)) {
            print $io '"', LLSD::_format_real($data), '"';
        }
        elsif (LLSD::_is_negative_zero($data)) {
            print $io '-0.0';
        }
        else {
            print $io (0 + $data);
        }
    }
    elsif ($type eq 'string') {
        $self->_format_string($data->asString(), $io);
    }
    elsif ($type eq 'uuid') {
        $self->_format_string($data->asString(), $io);
    }
    elsif ($type eq 'date') {
        $self->_format_string($data->asString(), $io);
    }
    elsif ($type eq 'uri') {
        $self->_format_string($data->asString(), $io);
    }
    elsif ($type eq 'binary') {
        $self->_format_string($data->asString(), $io);
    }
    elsif ($type eq 'array') {
        print $io '[';

        my $first = 1;
        foreach my $item (@{$data}) {
            print $io ',' if !$first; 
            $first = 0;

            $self->_format_value($item, $io);
        }
        print $io ']';
    }
    elsif ($type eq 'map') {
        my @keys = sort keys %{$data}; # sorted for test stability
        print $io '{';

        my $first = 1;
        foreach my $key (@keys) {
            print $io ',' if !$first;
            $first = 0;

            $self->_format_string($key, $io);
            print $io ':';            
            $self->_format_value($data->{$key}, $io);
        }
        print $io '}';
    }
}

sub _format_string($$$) {
    my ($self, $string, $io) = @_;

    print $io '"';
    foreach my $cp (map {ord} split(//, $string)) {
        if ($cp >= 0x10000) {
            my $hi = 0xd800 | ( ( ($cp - 0x10000) >> 10 ) & 0x3ff );
            my $lo = 0xdc00 | (   ($cp - 0x10000)         & 0x3ff );
            print $io sprintf('\\u%04X\\u%04X', $hi, $lo); 
        }
        elsif ($cp == 0x22) { print $io "\\\""; } # quotation mark
        elsif ($cp == 0x5c) { print $io "\\\\"; } # reverse solidus
        elsif ($cp == 0x2f) { print $io "/"; }   # solidus
        elsif ($cp == 0x08) { print $io "\\b"; } # backspace
        elsif ($cp == 0x0c) { print $io "\\f"; } # form feed
        elsif ($cp == 0x0a) { print $io "\\n"; } # line feed
        elsif ($cp == 0x0d) { print $io "\\r"; } # carriage return
        elsif ($cp == 0x09) { print $io "\\t"; } # tab
        elsif ($cp < 0x20) {
            print $io sprintf('\\u%04X', $cp); 
        }
        else {
            # This assumes the same character encoding
            # incoming as outgoing, e.g. UTF-8
            print $io chr($cp);
        }
    }

    print $io '"';
}

sub parsefile($$) {
    my ($self, $filename) = @_;

    my $io = new IO::File($filename, 'r');
    $io->binmode(':utf8');

    my $llsd = $self->_parse($io);
    $self->_ws($io);
    die "Unexpected continuation of data\n" unless $io->eof();
    return $llsd;
}

sub parse($$) {
    my ($self, $data) = @_;

    my $utf8 = $data;
    utf8::upgrade($utf8);

    my $io = new IO::String($utf8);

    $io->binmode(':utf8');

    my $llsd = $self->_parse_value($io);
    $self->_ws($io);
    die "Unexpected continuation of data\n" unless $io->eof();
    return $llsd;
}


sub _test($$$) {
    my ($self, $io, $string) = @_;

    my $pos = $io->getpos;

    for my $c (split(//, $string)) {
        if ($c ne $io->getc) {
            $io->setpos($pos);
            return 0;
        }
    }   
    return 1;
}

sub _peek($$$) {
    my ($self, $io, $string) = @_;

    my $pos = $io->getpos;
    my $c = $io->getc;
    $io->setpos($pos);
    return $c;
}

sub _ws($$$) {
    my ($self, $io) = @_;

    while (!$io->eof) {
        my $pos = $io->getpos;
        my $c = $io->getc;
        
        if ($c ne "\x20" and
            $c ne "\x09" and
            $c ne "\x0a" and
            $c ne "\x0d") 
        {
            $io->setpos($pos);
            last;
        }
    }
    return 1;
}

sub _require($$$) {
    my ($self, $io, $required) = @_;

    die "Expected '$required', saw ".readline($io)."\n" unless $self->_test($io, $required);
}

sub _parse_string($$) {
    my ($self, $io) = @_;

    die "Expected string\n" unless $io->getc eq '"';
    my $s = '';
    my $hi = 0;
    while (!$io->eof) {
        my $cp = ord($io->getc);

        if ($cp eq ord('"')) {
            return $s;
        }
        elsif ($cp eq ord('\\')) {
            
            my $c = $io->getc;
            if    ($c eq '"' ) { $cp = 0x22; } # quotation mark
            elsif ($c eq '\\') { $cp = 0x5c; } # reverse solidus
            elsif ($c eq '/' ) { $cp = 0x2f; } # solidus
            elsif ($c eq 'b' ) { $cp = 0x08; } # backspace
            elsif ($c eq 'f' ) { $cp = 0x0c; } # form feed
            elsif ($c eq 'n' ) { $cp = 0x0a; } # line feed
            elsif ($c eq 'r' ) { $cp = 0x0d; } # carriage return
            elsif ($c eq 't' ) { $cp = 0x09; } # tab
            elsif ($c eq 'u' ) { 
                my $h = $io->getc . $io->getc . $io->getc . $io->getc;
                die "Expected hex digits" unless $h =~ /[0-9A-Fa-f]{4}/;
                $cp = hex($h);
            }
        }

        # Decode UTF-16 surrogate pairs
        if (0xd800 <= $cp and $cp <= 0xdbff) {
            $hi = $cp;
            next;
        }
        elsif (0xdc00 <= $cp and $cp <= 0xdfff) {
            $cp = 0x10000 + (($hi & 0x3ff) << 10) + ($cp & 0x3ff);
            $hi = 0;
        }
        
        $s .= chr($cp);        
    }

    die "Unexpected EOF\n";
}

sub _parse_number($$) {
    my ($self, $io) = @_;

    my $s = '';
    my $sign = 1;

    my $c = $io->getc;
    if ($c eq '-') { 
        $sign = -1;
        $c = $io->getc; 
    }
    
    if ($c eq '0') { 
        $s .= $c;
    }
    elsif ($c =~ m/[1-9]/) {
        $s .= $c; 
        while (!$io->eof && $self->_peek($io) =~ m/[0-9]/) {
            $s .= $io->getc;
        }
    }
    else {
        die "Expected digit\n";
    }
    
    if (!$io->eof && $self->_peek($io) eq '.') {
        $s .= $io->getc;
        do {
            $c = $io->getc;
            die "Expected digit" unless $c =~ m/[0-9]/;
            $s .= $c;
        } while !$io->eof && $self->_peek($io) =~ m/[0-9]/;
    }


    if (!$io->eof && $self->_peek($io) =~ /[eE]/) {
        $s .= $io->getc;
        $s .= $io->getc if !$io->eof && $self->_peek($io) =~ /[+\-]/;
        
        do {
            $c = $io->getc;
            die "Expected digit" unless $c =~ m/[0-9]/;
            $s .= $c;
        } while !$io->eof && $self->_peek($io) =~ m/[0-9]/;
    }

    $s = 0 + $s;
    return -1/'inf' if $s == 0 and $sign == -1;
    return $sign * $s;
}

sub _parse_value($$) {
    my ($self, $io) = @_;

    $self->_ws($io);
    die "Unexpected EOF\n" if $io->eof();

    if ($self->_test($io, 'null')) {
        return LLSD::Undefined;
    }
    elsif ($self->_test($io, 'true')) {
        return LLSD::True;
    }
    elsif ($self->_test($io, 'false')) {
        return LLSD::False;
    }
    elsif ($self->_test($io, '[')) {
        my $array = new LLSD::Array;
        $self->_ws($io);
        if (!$self->_test($io, ']')) {
            
            do {
                my $value = $self->_parse_value($io);
                die "Expected value" unless defined $value;
                push(@$array, $value);
                $self->_ws($io);

            } while $self->_test($io, ',') && $self->_ws($io);

            $self->_require($io, ']');
        }
        return $array;
    }
    elsif ($self->_test($io, '{')) {
        my $map = new LLSD::Map;
        $self->_ws($io);
        if (!$self->_test($io, '}')) {
            do {
                my $key = $self->_parse_string($io);
                $self->_ws($io);
                $self->_require($io, ':');
                my $value = $self->_parse_value($io);
                die "Expected value" unless defined $value;
                $map->{$key} = $value;
                $self->_ws($io);

            } while $self->_test($io, ',') && $self->_ws($io);

            $self->_require($io, '}');
        }
        return $map;
    }
    elsif ($self->_peek($io) eq '"') {
        my $string = $self->_parse_string($io);
        return new LLSD::String($string);
    }
    elsif ($self->_peek($io) =~ m/[\-0123456789]/) {
        my $number = $self->_parse_number($io);
        return new LLSD::Real($number);
    }
    else {
        die "Expected value\n";
    }
}


1; # End of module
__END__
