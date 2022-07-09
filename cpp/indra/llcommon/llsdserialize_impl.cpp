/** 
 * @file llsdserialize_impl.cpp
 * @brief Implementation of LLSD parsers and formatters
 *
 * $LicenseInfo:firstyear=2006&license=mit$
 * 
 * Copyright (c) 2006-2010, Linden Research, Inc.
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

#include "llsdserialize_impl.h"

#include <iostream>


namespace LLSDSerialize
{
	/**
	 * LLSDParser
	 */
	LLSDParser::LLSDParser()
	: mCheckLimits(true), mMaxBytesLeft(0)
	{
	}

	// virtual
	LLSDParser::~LLSDParser()
	{ }

	bool LLSDParser::parse(std::istream& istr, LLSD& data, S32 max_bytes,
						  std::string* err_msg)
	{
		mCheckLimits = !(LLSDSerializationTraits::SIZE_UNLIMITED == max_bytes);
		mMaxBytesLeft = max_bytes;
		mErrorMsg = err_msg;
		mErrorMsgNoted = false;
		return doParse(istr, data);
	}

	bool LLSDParser::noteFailure(const std::string& err_msg)
	{
		if (!mErrorMsgNoted && mErrorMsg)
		{
			*mErrorMsg = err_msg;
		}
		mErrorMsgNoted = true;
		return false;
	}
	
	bool LLSDParser::noteSuccess()
	{
		return !mErrorMsgNoted;
	}
	
	int LLSDParser::get(std::istream& istr) const
	{
		if(mCheckLimits) --mMaxBytesLeft;
		return istr.get();
	}

	std::istream& LLSDParser::get(
								  std::istream& istr,
								  char* s,
								  std::streamsize n,
								  char delim) const
	{
		istr.get(s, n, delim);
		if(mCheckLimits) mMaxBytesLeft -= istr.gcount();
		return istr;
	}

	std::istream& LLSDParser::get(
								  std::istream& istr,
								  std::streambuf& sb,
								  char delim) const		
	{
		istr.get(sb, delim);
		if(mCheckLimits) mMaxBytesLeft -= istr.gcount();
		return istr;
	}

	std::istream& LLSDParser::ignore(std::istream& istr) const
	{
		istr.ignore();
		if(mCheckLimits) --mMaxBytesLeft;
		return istr;
	}

	std::istream& LLSDParser::putback(std::istream& istr, char c) const
	{
		istr.putback(c);
		if(mCheckLimits) ++mMaxBytesLeft;
		return istr;
	}

	std::istream& LLSDParser::read(
								   std::istream& istr,
								   char* s,
								   std::streamsize n) const
	{
		istr.read(s, n);
		if(mCheckLimits) mMaxBytesLeft -= istr.gcount();
		return istr;
	}

	void LLSDParser::account(S32 bytes) const
	{
		if(mCheckLimits) mMaxBytesLeft -= bytes;
	}


	std::streamsize fullread(std::istream& istr,
							 char* buf,
							 std::streamsize requested)
	{
		std::streamsize got;
		std::streamsize total = 0;
		
		istr.read(buf, requested);	 /*Flawfinder: ignore*/
		got = istr.gcount();
		total += got;
		while(got && total < requested)
		{
			if(istr.fail())
			{
				// If bad is true, not much we can doo -- it implies loss
				// of stream integrity. Bail in that case, and otherwise
				// clear and attempt to continue.
				if(istr.bad()) return total;
				istr.clear();
			}
			istr.read(buf + total, requested - total);	 /*Flawfinder: ignore*/
			got = istr.gcount();
			total += got;
		}
		return total;
	}

}


void LLSDLogTraits::format(const LLSD& sd, std::ostream& str)
{
#ifdef LL_LEGACY
	LLSDNotationTraits::format(sd, str);
#else
	LLSDXMLTraits::format(sd, str);
#endif
}
