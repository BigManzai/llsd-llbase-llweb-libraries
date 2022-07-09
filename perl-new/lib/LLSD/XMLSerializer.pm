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

package LLSD::XMLSerializer;

use 5.008000;
use strict;
use warnings;

require Exporter;

our $VERSION = 3.01;
our @ISA = qw(Exporter);
our @EXPORT = qw();
our @EXPORT_OK = qw();

use LLSD;

use XML::DOM;
use Scalar::Util qw(looks_like_number);


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

    return $doc->createElement("undef") if $type eq 'undefined';
    return $self->_format_array($data, $doc) if $type eq 'array';
    return $self->_format_map($data, $doc) if $type eq 'map';

    my $node = $doc->createElement($type);
    my $text = '' . $data;
    utf8::upgrade($text); # Convert in-memory Latin-1 to UTF-8
    $node->insertBefore($doc->createTextNode($text));
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
        die "Unable to parse XML file\n";
    }
    my $llsd = _parse_doc($doc);
    $doc->dispose;
    return $llsd;
}


sub parse($$) {
    my ($self, $xml) = @_;

    my $data = $xml;
    if (ref $xml and UNIVERSAL::isa($xml, 'IO::Handle')) {
        # no-op
    }
    else {
        # assume a string
        # if it's a byte string assume UTF-8 and decode
        utf8::decode($data) unless utf8::is_utf8($data);
    }

    # try to parse the xml
    my $parser = new XML::DOM::Parser;
    my $doc = eval {
        $parser->parse($data);
    };
    if ($@) {
        die "Unable to parse XML string\n";
    }
    my $llsd = _parse_doc($doc);
    $doc->dispose;
    return $llsd;
}


# An exeption is thrown (via die) on a parse error.
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
# If the XML does not parse correctly, an exception is thrown (via die)
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
                return LLSD::Undefined;
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
                return new LLSD(0, $type);
            }
            elsif ($value =~ /^-?\d+$/) {
                return new LLSD($value, $type);
            }

            die "Invalid format for Integer LLSD: ", $value, "\n";
        }
        elsif ($type eq 'real') {
            if (!defined $value) {
                return new LLSD::Real();
            }
            elsif (looks_like_number($value)) {
                return new LLSD::Real($value);
            }

            die "Invalid format for Real LLSD: ", $value, "\n";
        }
        elsif ($type eq 'uuid') {
            if (!defined $value) {
                return new LLSD('', $type);
            }
            elsif ($value =~ /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/) {
                return new LLSD($value, $type);
            }

            die "Invalid format for UUID LLSD: ", $value, "\n";
        }
        elsif ($type eq 'date') {
            if (!defined $value) {
                return new LLSD($value, $type);
            }
            elsif ($value =~ m/^\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z$/) {
                return new LLSD($value, $type);
            }

            die "Invalid format for Date LLSD: ", $value, "\n";
        }
        elsif ($type eq 'string') {
            return new LLSD($value, $type);
        }
        elsif ($type eq 'uri') {
            return new LLSD($value, $type);
        }
        elsif ($type eq 'binary') {
            if (!defined $value) {
                return new LLSD::Binary();
            }
            else {
                my $bin = LLSD::_decode_base64($value);
                die "Invalid base64 data" unless defined $bin;
                return new LLSD::Binary($bin);
            }
        }

        die "Invalid type", (defined $type ? ' ' . $type : ''), "\n";
    }
}

sub _parse_array($) {
    my (@children) = @_;
    my $array = new LLSD::Array;
    my $child;
    while ($child = shift @children) {
        my $value = _parse_node($child);
        push @$array, $value;
    }
    return $array;
}


sub _parse_map($) {
    my (@children) = @_;
    my $map = new LLSD::Map;

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

        $map->{$key} = _parse_node($child);
    }
    return $map;
}

1; # End of module
__END__
