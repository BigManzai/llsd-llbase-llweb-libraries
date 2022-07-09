/** 
 * @file stub.h
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

#ifndef STUB_STUB_H
#define STUB_STUB_H

#include <iosfwd>
#include <string>


namespace stub {
	class uri
	{
	public:
		uri();
		uri(const std::string&);
		~uri();
		
		bool operator==(const uri&) const;
		bool operator!=(const uri&) const;
		
		std::string asString() const;
		void printOn(std::ostream&) const;
	private:
		std::string mStr;
	};
	
	
	class uuid
	{
	public:
		const static unsigned int size = 16;
		typedef unsigned char bytes_t[size];
		
		uuid();
		uuid(const bytes_t&);
		uuid(const std::string&);
		~uuid();
		
		static const uuid null;
		
		bool operator==(const uuid&) const;
		bool operator!=(const uuid&) const;
		
		bool isNull() const;
		const bytes_t& bytes() const;	// only valid as long as the receiver

		std::string asString() const;		
		void printOn(std::ostream&) const;
	private:
		bytes_t mBytes;
	};
	
	
	
	class date
	{
	public:
		date();
		date(double);
		date(const std::string&);
		~date();
		
		bool operator==(const date&) const;
		bool operator!=(const date&) const;
		
		double secondsSinceEpoch() const;

		std::string asString() const;
		void printOn(std::ostream&) const;
	private:
		double mTimeStamp;
	};

	
	inline std::ostream& operator<<(std::ostream& o, const uri& d)
		{ d.printOn(o); return o; }
	inline std::ostream& operator<<(std::ostream& o, const uuid& d)
		{ d.printOn(o); return o; }
	inline std::ostream& operator<<(std::ostream& o, const date& d)
		{ d.printOn(o); return o; }
	
};


#endif // STUB_STUB_H
