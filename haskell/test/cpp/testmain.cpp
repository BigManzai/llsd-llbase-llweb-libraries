/** 
 * @file testmain.cpp
 * @brief LLSD timing tests.
 *
 * $LicenseInfo:firstyear=2009&license=mit$
 * 
 * Copyright (c) 2009, Linden Research, Inc.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 * $/LicenseInfo$
 */

#include <getopt.h>
#include <iostream>
#include <sstream>

#include "llsd.h"
#include "llsdserialize.h"

#include "testdata.h"


using namespace std;

enum Format { FormatXML, FormatBinary };

struct Mode
{
    Mode() : generate(0), count(0), read(false), every(1000),
             format(FormatXML) { }
    
    int     generate;
    int     count;
    bool    read;
    int     every;
    
    Format  format;
};


void usage(bool inError)
{
    (inError ? cerr : cout)
        << "Usage: testllsd [options]" << endl
//      << "  -t    --test, --tests        run unit tests and checks" << endl
        << "  -g N  --gen=N, --generate=N  generate N packets of LLSD output" << endl
        << "  -c M  --count=M              generate M records per packet" << endl
        << "  -r    --read                 read packets of LLSD" << endl
        << "  -e N  --every=N              report stats every N packets" << endl
        << "  -b    --binary               binary encoded LLSD" << endl
        << "  -x    --xml, --XML           XML encoded LLSD" << endl
//      << "  -d F  --data=F               read/write data file F" << endl
//      << "  -q    --quiet                supress message output" << endl
        << "  -?    --help                 print help summary" << endl
        ;
    exit(inError ? -1 : 0);
}

int processOptions(int argc, char * const argv[], Mode& mode)
{
    static struct option longopts[] =
    {
        { "gen",        required_argument,  NULL, 'g' },
        { "generate",   required_argument,  NULL, 'g' },
        { "count",      required_argument,  NULL, 'c' },
        { "read",       no_argument,        NULL, 'r' },
        { "every",      required_argument,  NULL, 'e' },
        { "xml",        no_argument,        NULL, 'x' },
        { "XML",        no_argument,        NULL, 'x' },
        { "binary",     no_argument,        NULL, 'b' },
        { NULL,         0,                  NULL,  0  }
    };
    
    int ch;
    while ((ch = getopt_long(argc, argv, "g:c:re:xb", longopts, NULL)) != -1)
    {
        switch (ch)
        {
            case 'g':   mode.generate = atoi(optarg);   break;
            case 'c':   mode.count    = atoi(optarg);   break;
            case 'r':   mode.read     = true;           break;
            case 'e':   mode.every    = atoi(optarg);   break;
            case 'x':   mode.format   = FormatXML;      break;
            case 'b':   mode.format   = FormatBinary;   break;
            case '?':
                if (optopt == '?')
                {
                    usage(false);
                }
                
                cerr << "unrecognized option '-" << optopt << "'" << endl;
                // fall through
                                
            default:
                usage(true);
        }
    }
    
    return optind;
}



void reportCount(const Mode& mode, int i)
{
    if (i % mode.every == 0)
    {
        cerr << i << endl;
    }
}

int getInt32(istream& in)
{
    int v;
    in.read((char*)&v, sizeof(v));
    return ntohl(v);
}

void readData(const Mode& mode, istream& in)
{
    LogStats stats;
    int count = getInt32(in);
    for (int i = 0; i < count; ++i) {
        int length = getInt32(in);
        
        stringstream packetBuffer;
        for (int left = length; left > 0;)
        {
            static streamsize chunkSize = 1000;
            char chunk[chunkSize];
            streamsize count = (chunkSize < left) ? chunkSize : left;
            in.read(chunk, count);
            count = in.gcount();
            packetBuffer.write(chunk, count);
            left -= count;
        }
        
        LLSD packet;
        switch (mode.format)
        {
            case FormatXML:
                LLSDSerialize::fromXMLDocument(packet, packetBuffer);
                break;
            
            case FormatBinary:
                LLSDSerialize::fromBinary(packet, packetBuffer, length);
                break;
        }
        
        for (LLSD::array_const_iterator p = packet.beginArray();
             p != packet.endArray();
             ++p)
        {
            LogBase* l = LogBase::buildFromLLSD(*p);
            l->tally(stats);
            delete l;
        }
        reportCount(mode, i+1);
    }
    stats.report();
}


int main(int argc, char * const argv[])
{
    Mode mode;
    
    int optind = processOptions(argc, argv, mode);
    argc += optind;
    argv += optind;
    
    
    if (mode.read)
    {
        readData(mode, cin);
    }
    else if (mode.generate > 0)
    {
        //generateData(mode, cout);
    }
    else
    {
        usage(true);
    }
}
