/** 
 * @file stub.cpp
 * @brief Stub implementations of data, uuid and uri objects.
 *
 * $LicenseInfo:firstyear=2010&license=mit$
 * 
 * Copyright (c) 2010, Linden Research, Inc.
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

#include "stub.h"

#include <algorithm>
#include <cmath>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <sstream>



using namespace stub;

uri::uri() { }
uri::uri(const std::string& str) : mStr(str) { }
uri::~uri() { }

std::string uri::asString() const { return mStr; }
void uri::printOn(std::ostream& o) const { o << mStr; }
bool uri::operator==(const uri& o) const { return mStr == o.mStr; }
bool uri::operator!=(const uri& o) const { return mStr != o.mStr; }



date::date() : mTimeStamp(0.0) { }
date::date(double t) : mTimeStamp(t) { }
date::~date() { }

double date::secondsSinceEpoch() const { return mTimeStamp; }
bool date::operator==(const date& o) const { return mTimeStamp == o.mTimeStamp; }
bool date::operator!=(const date& o) const { return mTimeStamp != o.mTimeStamp; }

static void match(std::istream& s, char c)
{
	if (s.get() != c)
	{
		s.setstate(std::ios_base::failbit);
	}
}

date::date(const std::string& str) : mTimeStamp(0.0)
{
	time_t epoch = 0;
	struct tm parts;
	parts = *gmtime(&epoch);
	time_t mktimeEpoch = mktime(&parts);

	std::istringstream s(str);

	s >> parts.tm_year; parts.tm_year -= 1900; match(s, '-');
	s >> parts.tm_mon;  parts.tm_mon  -= 1;    match(s, '-');
	s >> parts.tm_mday;
	
	match(s, 'T');
	
	s >> parts.tm_hour; match(s, ':');
	s >> parts.tm_min;  match(s, ':');
	s >> parts.tm_sec;
	
	double seconds = (double)(mktime(&parts) - mktimeEpoch);

	if (s.peek() == '.')
	{
		double fractional = 0.0;
		s >> fractional;
		seconds += fractional;
	}
	
	match(s,'Z');
	
	if (!s.fail())
	{
		mTimeStamp = seconds;
	}
}

std::string date::asString() const
{
	std::ostringstream s;
	printOn(s);
	return s.str();
}

void date::printOn(std::ostream& s) const
{
	double integralTimeStamp = floor(mTimeStamp);
	time_t seconds = (time_t)integralTimeStamp;
	int useconds = (int)((mTimeStamp - integralTimeStamp) * 1000000.0);

	struct tm parts;
	parts = *gmtime(&seconds);
	
	std::ostream::fmtflags oldFlags = s.flags();
	char oldFill = s.fill();
		
	s << std::dec << std::setfill('0');
	s << std::setw(4) << (parts.tm_year + 1900) << '-'
	  << std::setw(2) << (parts.tm_mon  + 1)    << '-'
	  << std::setw(2) <<  parts.tm_mday
	  << 'T'
	  << std::setw(2) << parts.tm_hour << ':'
	  << std::setw(2) << parts.tm_min  << ':'
	  << std::setw(2) << parts.tm_sec;
	
	if (useconds != 0) {
		s << '.' << std::setw(3) << (int)(useconds / 1000.f + 0.5f);
	}
	
	s << 'Z';
	
	s.fill(oldFill);
	s.setf(oldFlags);
}



uuid::uuid()                 { std::fill(mBytes, mBytes+size, 0); }
uuid::uuid(const bytes_t& b) { std::copy(b, b+size, mBytes); }
uuid::~uuid() { }

bool uuid::isNull() const { return *this == null; }

const uuid::bytes_t& uuid::bytes() const { return mBytes; }
bool uuid::operator==(const uuid& o) const { return std::equal(mBytes, mBytes+size, o.mBytes); }
bool uuid::operator!=(const uuid& o) const { return ! (*this == o); }

static int hexvalue(char c)
{
	if ('0' <= c && c <= '9') return c-'0';
	if ('A' <= c && c <= 'F') return c-'A'+10;
	if ('a' <= c && c <= 'f') return c-'a'+10;
	return 0;
}

uuid::uuid(const std::string& s)
{
	std::fill(mBytes, mBytes+size, 0);
	int v = 0;
	std::string::const_iterator j = s.begin(), e = s.end();
	for (unsigned int i = 0; i < 2*size && j != e; ++j) {
		char c = *j;
		if (c == '-') continue;
		if (!isxdigit((int)(unsigned char)c)) break;
		
		if ((i&1) == 0)
		{
			v = hexvalue(c);
		}
		else
		{
			mBytes[i/2] = (v<<4)+hexvalue(c);
		}
		
		i += 1;
	}
}

std::string uuid::asString() const
{
	std::ostringstream s;
	printOn(s);
	return s.str();
}

void uuid::printOn(std::ostream& s) const
{
	std::ostream::fmtflags oldFlags = s.flags();
	char oldFill = s.fill();
	
	s << std::hex << std::setfill('0')
	<< std::setw(2) << (unsigned int)mBytes[0]
	<< std::setw(2) << (unsigned int)mBytes[1]
	<< std::setw(2) << (unsigned int)mBytes[2]
	<< std::setw(2) << (unsigned int)mBytes[3]
	<< '-'
	<< std::setw(2) << (unsigned int)mBytes[4]
	<< std::setw(2) << (unsigned int)mBytes[5]
	<< '-'
	<< std::setw(2) << (unsigned int)mBytes[6]
	<< std::setw(2) << (unsigned int)mBytes[7]
	<< '-'
	<< std::setw(2) << (unsigned int)mBytes[8]
	<< std::setw(2) << (unsigned int)mBytes[9]
	<< '-'
	<< std::setw(2) << (unsigned int)mBytes[10]
	<< std::setw(2) << (unsigned int)mBytes[11]
	<< std::setw(2) << (unsigned int)mBytes[12]
	<< std::setw(2) << (unsigned int)mBytes[13]
	<< std::setw(2) << (unsigned int)mBytes[14]
	<< std::setw(2) << (unsigned int)mBytes[15];
	
	s.fill(oldFill);
	s.setf(oldFlags);
}

const uuid uuid::null;

