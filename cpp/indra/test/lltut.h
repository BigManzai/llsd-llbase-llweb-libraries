/** 
 * @file lltut.h
 * @brief helper tut methods
 *
 * $LicenseInfo:firstyear=2005&license=mit$
 * 
 * Copyright (c) 2005-2010, Linden Research, Inc.
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

#ifndef LL_LLTUT_H
#define LL_LLTUT_H

#include <tut/tut.hpp>

#include "stdtypes.h"

class LLSD;


namespace tut
{	
	template <class T,class Q>
	void ensure_not_equals(const char* msg,const Q& actual,const T& expected)
	{
		if( expected == actual )
		{
			std::stringstream ss;
			ss << (msg?msg:"") << (msg?": ":"") << "both equal " << expected;
			throw tut::failure(ss.str().c_str());
		}
	}

	template <class T,class Q>
	void ensure_not_equals(const Q& actual,const T& expected)
	{
		ensure_not_equals(NULL, actual, expected);
	}
	
	
	template <class T,class Q>
	void ensure_equals(const std::string& msg,
		const Q& actual,const T& expected)
		{ ensure_equals(msg.c_str(), actual, expected); }

		
	void ensure_equals(const std::string& msg,
		const std::vector<U8>& actual, const std::vector<U8>& expected);

	void ensure_equals(const char* msg,
		const LLSD& actual, const LLSD& expected);

	void ensure_equals(const std::string& msg,
		const LLSD& actual, const LLSD& expected);
	
	void ensure_starts_with(const std::string& msg,
		const std::string& actual, const std::string& expectedStart);

	void ensure_ends_with(const std::string& msg,
		const std::string& actual, const std::string& expectedEnd);

	void ensure_contains(const std::string& msg,
		const std::string& actual, const std::string& expectedSubString);

	void ensure_does_not_contain(const std::string& msg,
		const std::string& actual, const std::string& expectedSubString);
}


#endif // LL_LLTUT_H
