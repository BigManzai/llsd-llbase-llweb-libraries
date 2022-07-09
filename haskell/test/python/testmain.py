# file testmain.py
#
# $LicenseInfo:firstyear=2009&license=mit$
#
# Copyright (c) 2009, Linden Research, Inc.
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


import llbase.llsd as llsd
import struct
import sys
from optparse import OptionParser
from testdata import LogStats, LogItem

parser = OptionParser()
parser.add_option("-g", "--gen", "--generate",
        help="generate N packets of LLSD output", metavar="N",
        action="store", type="int", dest="generate", default=0)
parser.add_option("-c", "--count",
        help="generate M records per packet", metavar="M",
        action="store", type="int", dest="count", default=1)
parser.add_option("-r", "--read",
        help="read packets of LLSD",
        action="store_true", dest="read", default=False)
parser.add_option("-e", "--every",
        help="report stats every N packets", metavar="N",
        action="store", type="int", dest="every", default=1000)
parser.add_option("-x", "--xml", "--XML",
        help="XML encoded LLD",
        action="store_const", const="xml", dest="format", default="xml")
parser.add_option("-b", "--binary",
        help="Binary encoded LLD",
        action="store_const", const="binary", dest="format")


formatters = { "xml": llsd.format_xml, "binary": llsd.format_binary }
parsers    = { "xml": llsd.parse_xml,  "binary": llsd.parse_binary  }


def report_count(options, i):
    if i % options.every == 0:
        print >> sys.stderr, i
    
def getInt32(f):
    d = f.read(4)
    (i,) = struct.unpack('>i', d)
    return i
    
def read_data(options, f):
    global parsers
    parser = parsers[options.format]
    stats = LogStats()
    count = getInt32(f)
    for i in xrange(count):
        length = getInt32(f)
        packet = f.read(length)
        ls = parser(packet)
        for l in ls:
            item = LogItem.fromLLSD(l)
            stats.note(item)
        report_count(options, i+1)
    print stats


def main():
    global parser
    (options, args) = parser.parse_args()
    if options.read:    
        read_data(options, sys.stdin)
    elif options.generate > 0:
        generate_data(options, sys.stdout)
    else:
        parser.error("no mode specified")

if __name__ == '__main__':
    main()
