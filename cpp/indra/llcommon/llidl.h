/** 
 * @file llidl.h
 * @brief LLIDL interface description langauge.
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

#ifndef LL_LLIDL_H
#define LL_LLIDL_H

#include <memory>

#include "llsd.h"


class LLIDL
{

public:
	class Value
	{
	public:
		virtual bool match(const LLSD&) const = 0;
		virtual bool valid(const LLSD&) const = 0;
		
		virtual bool has_additional(const LLSD&) const = 0;
		virtual bool has_defaulted(const LLSD&) const = 0;
		virtual bool incompatible(const LLSD&) const = 0;
		
		virtual ~Value();
	protected:
		Value();
		Value& operator=(const Value&);	// not implemented, not copyable
	};
	
	class Suite
	{
	public:
	   Value& request(const std::string&) const;
	   Value& response(const std::string&) const;
	   
	   ~Suite();
    protected:
        Suite();
        Suite& operator=(const Suite&); // not implemented, not copyable
	};
	
	static std::auto_ptr<Value> parseValue(std::istream&);
	static std::auto_ptr<Suite> parseSuite(std::istream&);
};

#endif
