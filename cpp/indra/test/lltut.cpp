/** 
 * @file lltut.cpp
 * @brief Expanded utilities for the tut framework
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


#include "lltut.h"

#include "llsd.h"


namespace tut
{
	void ensure_equals(const std::string& msg,
		const std::vector<U8>& actual, const std::vector<U8>& expected)
	{
		std::string s(msg);
		
		ensure_equals(s + " size", actual.size(), expected.size());
		
		std::vector<U8>::const_iterator i, j;
		int k;
		for (i = actual.begin(), j = expected.begin(), k = 0;
			i != actual.end();
			++i, ++j, ++k)
		{
			ensure_equals(s + " field", *i, *j);
		}
	}

	void ensure_equals(const char* m, const LLSD& actual,
		const LLSD& expected)
    {
        ensure_equals(std::string(m), actual, expected);
    }

	void ensure_equals(const std::string& msg, const LLSD& actual,
		const LLSD& expected)
	{
		ensure_equals(msg + " type", actual.type(), expected.type());
		switch (actual.type())
		{
			case LLSD::TypeUndefined:
				return;
			
			case LLSD::TypeBoolean:
				ensure_equals(msg + " boolean", actual.asBoolean(), expected.asBoolean());
				return;
			
			case LLSD::TypeInteger:
				ensure_equals(msg + " integer", actual.asInteger(), expected.asInteger());
				return;
			
			case LLSD::TypeReal:
				ensure_equals(msg + " real", actual.asReal(), expected.asReal());
				return;
			
			case LLSD::TypeString:
				ensure_equals(msg + " string", actual.asString(), expected.asString());
				return;
			
			case LLSD::TypeUUID:
				ensure_equals(msg + " uuid", actual.asUUID(), expected.asUUID());
				return;
			
			case LLSD::TypeDate:
				ensure_equals(msg + " date", actual.asDate(), expected.asDate());
				return;
				
			case LLSD::TypeURI:
				ensure_equals(msg + " uri", actual.asURI(), expected.asURI());
				return;
		
			case LLSD::TypeBinary:
				ensure_equals(msg + " binary", actual.asBinary(), expected.asBinary());
				return;
		
			case LLSD::TypeMap:
			{
				ensure_equals(msg + " map size", actual.size(), expected.size());
				
				LLSD::map_const_iterator actual_iter = actual.beginMap();
				LLSD::map_const_iterator expected_iter = expected.beginMap();
				
				while(actual_iter != actual.endMap())
				{
					ensure_equals(msg + " map keys", 
						actual_iter->first, expected_iter->first);
					ensure_equals(msg + "[" + actual_iter->first + "]",
						actual_iter->second, expected_iter->second);
					++actual_iter;
					++expected_iter;
				}
				return;
			}
			case LLSD::TypeArray:
			{
				ensure_equals(msg + " array size", actual.size(), expected.size());
				
				for(int i = 0; i < actual.size(); ++i)
				{
					std::ostringstream s;
					s << msg << '[' << i << ']';
					
					ensure_equals(s.str(),
						actual[i], expected[i]);
				}
				return;
			}
		}
	}

	void ensure_starts_with(const std::string& msg,
		const std::string& actual, const std::string& expectedStart)
	{
		if( actual.find(expectedStart, 0) != 0 )
		{
			std::stringstream ss;
			ss << msg << ": " << "expected to find " << expectedStart
				<< " at start of actual " << actual;
			throw failure(ss.str().c_str());
		}
	}

	void ensure_ends_with(const std::string& msg,
		const std::string& actual, const std::string& expectedEnd)
	{
		if( actual.size() < expectedEnd.size()
			|| actual.rfind(expectedEnd)
				!= (actual.size() - expectedEnd.size()) )
		{
			std::stringstream ss;
			ss << msg << ": " << "expected to find " << expectedEnd
				<< " at end of actual " << actual;
			throw failure(ss.str().c_str());
		}
	}

	void ensure_contains(const std::string& msg,
		const std::string& actual, const std::string& expectedSubString)
	{
		if( actual.find(expectedSubString, 0) == std::string::npos )
		{
			std::stringstream ss;
			ss << msg << ": " << "expected to find " << expectedSubString
				<< " in actual " << actual;
			throw failure(ss.str().c_str());
		}
	}

	void ensure_does_not_contain(const std::string& msg,
		const std::string& actual, const std::string& expectedSubString)
	{
		if( actual.find(expectedSubString, 0) != std::string::npos )
		{
			std::stringstream ss;
			ss << msg << ": " << "expected not to find " << expectedSubString
				<< " in actual " << actual;
			throw failure(ss.str().c_str());
		}
	}
}

