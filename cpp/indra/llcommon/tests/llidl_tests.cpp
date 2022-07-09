/** 
 * @file llidl_tut.cpp
 * @brief LLIDL unit tests
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

#include "lltut.h"

#include <sstream>

#include "llidl.h"
#include "llsd.h"
#include "llsdtraits.h"


namespace tut
{
	struct IDLTestData {
		const LLSD undef;
		const LLSD::Date aDate;
		const LLSD::URI aURI;
		const LLSD::UUID aUUID;
		const std::vector<U8> aBinary;
		
		IDLTestData()
			: aDate("2009-02-18T21:36:22Z"),
			aURI("http://tools.ietf.org/html/draft-hamrick-llsd-00"),
			aUUID("20053153-2e6c-48d9-899b-bab4f9119da0")
		// need to init aBinary somehow
		{
		}
		
	private:
		std::string mValueDescription;
		std::auto_ptr<LLIDL::Value> mSpec;
	
	public:
		void parseValue(const char* valueDescription)
		{
			mValueDescription = valueDescription;
			std::istringstream input(mValueDescription);
			mSpec = LLIDL::parseValue(input);
		}
		
		typedef bool (LLIDL::Value::*TestFunc)(const LLSD&) const;
		struct TestType { ; };
		
		static const TestType match;
		static const TestType valid;
		static const TestType has_additional;
		static const TestType has_defaulted;
		static const TestType incompatible;
		
		void should(TestFunc func, const char* name, const LLSD& data)
		{
			if (!((mSpec.get()->*func)(data)))
			{
				std::stringstream ss;
				ss << "LLIDL '" << mValueDescription << "' "
				    << "failed " << name
				    << "for LLSD " << data.asString();
				fail(ss.str());
			}
		}
		
		template<class T>
		void should_match(const T& data)
		  { should(&LLIDL::Value::match, "match", LLSD(data)); }
		
		template<class T>
		void should_valid(const T& data)
		  { should(&LLIDL::Value::valid, "valid", LLSD(data)); }
		
		template<class T>
		void should_has_additional(const T& data)
		  { should(&LLIDL::Value::has_additional, "has_additional", LLSD(data)); }
		
		template<class T>
		void should_has_defaulted(const T& data)
		  { should(&LLIDL::Value::has_defaulted, "has_defaulted", LLSD(data)); }
		
		template<class T>
		void should_incompatible(const T& data)
		  { should(&LLIDL::Value::incompatible, "incompatible", LLSD(data)); }
		
	};
	

	typedef test_group<IDLTestData>	IDLTestGroup;
	typedef IDLTestGroup::object	IDLTestObject;

	IDLTestGroup idlTestGroup("LLIDL");
	
	template<> template<>
	void IDLTestObject::test<1>()
	{
		parseValue("undef");
		
		should_match(undef);
		should_match(true);
		should_match(false);
		should_match(0);
		should_match(1);
		should_match(3);
		should_match(0.0);
		should_match(1.0);
		should_match(3.14);
		should_match("");
		should_match("True");
		should_match("False");
		should_match(aDate);
		should_match(aUUID);
		should_match(aURI);
		should_match(aBinary);
	}

	template<> template<>
	void IDLTestObject::test<2>()
	{
		parseValue("bool");
		
		should_has_defaulted (undef);
		should_match         (true);
		should_match         (false);
		should_match         (0);
		should_match         (1);
		should_incompatible  (3);
		should_match         (0.0);
		should_match         (1.0);
		should_incompatible  (3.14);
		should_match         ("");
		should_match         ("true");
		should_incompatible  ("false");
		should_incompatible  (aDate);
		should_incompatible  (aUUID);
		should_incompatible  (aURI);
		should_incompatible  (aBinary);
	}
};

