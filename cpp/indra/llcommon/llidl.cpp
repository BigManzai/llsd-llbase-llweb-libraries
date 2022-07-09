/** 
 * @file llidl.cpp
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

#include "llidl.h"

#include <stdexcept>

namespace
{
    enum Fidelity
    {
        INCOMPATIBLE,  // the value was a type that couldn't convert
        UNCONVERTABLE, // convertable type, but conversion failed
        APPROXIMATE,   // converted, but information was lost
        MIXED,         // value has both additional and defaulted data
        ADDITIONAL,    // value had additional data (for maps and arrays)
        DEFAULTED,     // the default value due to undef or ""
        CONVERTED,     // the value was converted
        NATIVE,        // value was natively the correct type
        MATCHED        // the value has the correct shape 
    };
    
    class Matcher : public LLIDL::Value
    {
    public:
		virtual bool match(const LLSD& v) const 
            { return this->compare(v) >= CONVERTED; }

		virtual bool valid(const LLSD& v) const
            { return this->compare(v) >= MIXED; }
		
		virtual bool has_additional(const LLSD& v) const
            { Fidelity r = this->compare(v); return r == ADDITIONAL || r == MIXED; }
            
		virtual bool has_defaulted(const LLSD& v) const
            { Fidelity r = this->compare(v); return r == DEFAULTED || r == MIXED; }
            
		virtual bool incompatible(const LLSD& v) const
            { return this->compare(v) < MIXED; }
    
    protected:
        virtual Fidelity compare(const LLSD& v) const = 0;
    };
    
	class UndefMatcher : public Matcher
	{
		Fidelity compare(const LLSD&) const { return MATCHED; }
	};
	
	class BoolMatcher : public Matcher
	{
		Fidelity compare(const LLSD& data) const
		{
			switch (data.type())
			{
				case LLSD::TypeUndefined:
					return DEFAULTED;
				case LLSD::TypeBoolean:
					return NATIVE;
				case LLSD::TypeInteger:
				{
					int v = data;
					return (v == 0 || v == 1) ? CONVERTED : APPROXIMATE;
				}
				case LLSD::TypeReal:
				{
					F64 v = data;
					return (v == 0.0 || v == 1.0) ? CONVERTED : APPROXIMATE;
				}
				case LLSD::TypeString:
				{
					std::string v = data;
					return (v == "" || v == "true") ? CONVERTED : APPROXIMATE;
				}
				default:
					return INCOMPATIBLE;
			}
		}
	};
}


LLIDL::Value::Value()	{ }
LLIDL::Value::~Value()	{ }

std::auto_ptr<LLIDL::Value> LLIDL::parseValue(std::istream& input)
{
	std::string word;
	input >> word;
	if (word == "undef")
		return std::auto_ptr<LLIDL::Value>(new UndefMatcher());
	if (word == "bool")
		return std::auto_ptr<LLIDL::Value>(new BoolMatcher());
	throw std::logic_error("expected value");
}
