/**
 * @file llsdtraits.h
 * @brief Unit test helpers
 *
 * $LicenseInfo:firstyear=2007&license=mit$
 * 
 * Copyright (c) 2007-2010, Linden Research, Inc.
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

#ifndef LLSDTRAITS_H
#define LLSDTRAITS_H

#include "llsd.h"


template<class T>
class LLSDTraits
{
 protected:
	typedef T (LLSD::*Getter)() const;
	
	LLSD::Type type;
	Getter getter;
	
 public:
	LLSDTraits();
	
	T get(const LLSD& actual)
		{
			return (actual.*getter)();
		}
	
	bool checkType(const LLSD& actual)
		{
			return actual.type() == type;
		}
};

template<> inline
LLSDTraits<LLSD::Boolean>::LLSDTraits()
	: type(LLSD::TypeBoolean), getter(&LLSD::asBoolean)
{ }

template<> inline
LLSDTraits<LLSD::Integer>::LLSDTraits()
	: type(LLSD::TypeInteger), getter(&LLSD::asInteger)
{ }

template<> inline
LLSDTraits<LLSD::Real>::LLSDTraits()
	: type(LLSD::TypeReal), getter(&LLSD::asReal)
{ }

template<> inline
LLSDTraits<LLSD::UUID>::LLSDTraits()
	: type(LLSD::TypeUUID), getter(&LLSD::asUUID)
{ }

template<> inline
LLSDTraits<LLSD::String>::LLSDTraits()
	: type(LLSD::TypeString), getter(&LLSD::asString)
{ }

template<>
class LLSDTraits<const char*> : public LLSDTraits<LLSD::String>
{ };

template<> inline
LLSDTraits<LLSD::Date>::LLSDTraits()
	: type(LLSD::TypeDate), getter(&LLSD::asDate)
{ }

template<> inline
LLSDTraits<LLSD::URI>::LLSDTraits()
	: type(LLSD::TypeURI), getter(&LLSD::asURI)
{ }

template<> inline
LLSDTraits<LLSD::Binary>::LLSDTraits()
	: type(LLSD::TypeBinary), getter(&LLSD::asBinary)
{ }

#endif // LLSDTRAITS_H
