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

package LLSD::NotationSerializer;

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

    my $notationdata = '';
    my $io = new IO::String($notationdata);

    $io->binmode();
    _format_value($self, $data, $io);

    return $notationdata
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
        print $io $data ? 'true' : 'false';
    }
    elsif ($type eq 'integer') {
        print $io 'i', $data->asInteger;
    }
    elsif ($type eq 'real') {
        print $io 'r', $data->asReal;
    }
    elsif ($type eq 'string') {
        print $io '\'';
        $self->_format_string($data->asString, $io);
        print $io '\'';
    }
    elsif ($type eq 'uuid') {
        print $io 'u', $data->asString;
    }
    elsif ($type eq 'date') {
        print $io 'd', '"', $data->asString, '"';
    }
    elsif ($type eq 'uri') {
        print $io 'l\'';
        $self->_format_string($data->asString, $io);
        print $io '\'';
    }
    elsif ($type eq 'binary') {
        my $binstr = $data->asBinary;
        print $io 'b(', length($binstr), ')"', $binstr, '"';
    }
    elsif ($type eq 'array') {
        print $io '[';

        my $first = 1;
        foreach my $item (@{$data}) {
            print $io ',' unless $first; 
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
            print $io ',' unless $first;
            $first = 0;
            print $io '\'';
            $self->_format_string($key, $io);
            print $io "':";
            $self->_format_value($data->{$key}, $io);
        }
        print $io '}';
    }
}

sub _format_string($$$) {
    my ($self, $string, $io) = @_;

    # The canonical C++ implementation processes strings as sequences 
    # of bytes, UTF-8 encoding is assumed

    my $utf8 = $string;
    utf8::encode($utf8);
    foreach my $b (unpack("C*", $utf8)) {
        
        if    ($b ==  7) { print $io "\\a"; }
        elsif ($b ==  8) { print $io "\\b"; }
        elsif ($b ==  9) { print $io "\\t"; }
        elsif ($b == 10) { print $io "\\n"; }
        elsif ($b == 11) { print $io "\\v"; }
        elsif ($b == 12) { print $io "\\f"; }
        elsif ($b == 13) { print $io "\\r"; }
        elsif ($b == 34) { print $io "\\\""; }
        elsif ($b == 39) { print $io "\\'"; }
        elsif ($b == 92) { print $io "\\\\"; }
        elsif (32 <= $b and $b <= 127) { print $io chr($b); }
        else {
            print $io sprintf('\\x%02x', $b);
        }
    }
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
        if ($io->eof or $c ne $io->getc) {
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

    if ($self->_test($io, '"')) {
        return $self->_parse_delim_string($io, '"');
    }
    elsif ($self->_test($io, '\'')) {
        return $self->_parse_delim_string($io, '\'');
    }
    elsif ($self->_test($io, 's')) {
        $self->_require($io, '(');
        my $len = $self->_parse_number($io);
        $self->_require($io, ')');
        $self->_require($io, '"');
        my $str;
        die "Unexpected EOF" unless $io->read($str, $len) == $len;
        $self->_require($io, '"');
        utf8::decode($str);
        return $str;
    }
    die "Expected string\n";
}

sub _parse_delim_string($$$) {
    my ($self, $io, $delim) = @_;

    my $s = '';
    while (!$io->eof) {
        my $c = $io->getc;

        if ($c eq $delim) {
            utf8::decode($s);
            return $s;
        }
        
        if ($c eq '\\') {
            die "Unexpected EOF\n" if $io->eof;
            $c = $io->getc;
            if    ($c eq 'a') { $s .= "\x07"; }
            elsif ($c eq 'b') { $s .= "\x08"; }
            elsif ($c eq 't') { $s .= "\x09"; }
            elsif ($c eq 'n') { $s .= "\x0a"; }
            elsif ($c eq 'v') { $s .= "\x0b"; }
            elsif ($c eq 'f') { $s .= "\x0c"; }
            elsif ($c eq 'r') { $s .= "\x0d"; }
            elsif ($c eq 'x') {
                my $hex = $io->getc . $io->getc;
                die "Unexpected EOF\n" if $io->eof;
                $s .= chr(hex($hex));
            }
            else { $s .= $c; }
        }
        else {
            $s .= $c;
        }
    }
    die "Unexpected EOF\n";
}

sub _parse_number($$) {
    my ($self, $io) = @_;

    return 'nan' if $self->_test($io, 'nan');
    return 'inf' if $self->_test($io, 'inf');
    return '-inf' if $self->_test($io, '-inf');

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

    if ($self->_test($io, '!')) {
        return LLSD::Undefined;
    }
    elsif ($self->_test($io, '1') or
           $self->_test($io, 'true') or
           $self->_test($io, 'TRUE') or
           $self->_test($io, 't') or
           $self->_test($io, 'T')) {
        return LLSD::True;
    }
    elsif ($self->_test($io, '0') or
           $self->_test($io, 'false') or
           $self->_test($io, 'FALSE') or
           $self->_test($io, 'f') or
           $self->_test($io, 'F')) {
        return LLSD::False;
    }
    elsif ($self->_test($io, 'i')) {
        return new LLSD::Integer($self->_parse_number($io));
    }
    elsif ($self->_test($io, 'r')) {
        return new LLSD::Real($self->_parse_number($io));
    }
    elsif ($self->_test($io, 'u')) {
        my $uuid;
        die "Unexpected EOF" unless $io->read($uuid, 36) == 36;
        return new LLSD::UUID($uuid);
    }
    elsif ($self->_peek($io) eq '"' or
           $self->_peek($io) eq "'" or
           $self->_peek($io) eq 's') {
        return new LLSD::String($self->_parse_string($io));
    }
    elsif ($self->_test($io, 'l')) {
        my $delim = $io->getc;
        die "Expected quote\n" unless $delim eq '"' or $delim eq "'";
        return new LLSD::URI($self->_parse_delim_string($io, $delim));
    }
    elsif ($self->_test($io, 'd')) {
        my $delim = $io->getc;
        die "Expected quote\n" unless $delim eq '"' or $delim eq "'";
        return new LLSD::Date($self->_parse_delim_string($io, $delim));
    }
    elsif ($self->_test($io, 'b')) {
        if ($self->_test($io, '(')) {
            my $len = $self->_parse_number($io);
            $self->_require($io, ')');
            my $delim = $io->getc;
            die "Expected quote\n" unless $delim eq '"' or $delim eq "'";
            my $binstr;
            die "Unexpected EOF" unless $io->read($binstr, $len) == $len;
            $self->_require($io, $delim);
            return new LLSD::Binary($binstr);
        }
        elsif ($self->_test($io, '16')) {
            my $delim = $io->getc;
            die "Expected quote\n" unless $delim eq '"' or $delim eq "'";
            my $b16 = $self->_parse_delim_string($io, $delim);

            my $bin = LLSD::_decode_base16($b16);
            die "Invalid base16 data" unless defined $bin;
            return new LLSD::Binary($bin);

        }
        elsif ($self->_test($io, '64')) {
            my $delim = $io->getc;
            die "Expected quote\n" unless $delim eq '"' or $delim eq "'";
            my $b64 = $self->_parse_delim_string($io, $delim);

            my $bin = LLSD::_decode_base64($b64);
            die "Invalid base64 data" unless defined $bin;
            return new LLSD::Binary($bin);
        }
        
        die "Unexpected binary base\n";
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
    else {
        die "Unexpected token: ", $io->getc, "\n";
    }
}


1; # End of module
__END__
