/** 
 * @file llsdserialize_impl.h
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

#ifndef LL_LLSDSERIALIZE_IMPL_H
#define LL_LLSDSERIALIZE_IMPL_H

#include "llsdserialize.h"

namespace LLSDSerialize
{
	/** 
	 * @class LLSDParser
	 * @brief Abstract base class for LLSD parsers.
	 */
	class LLSDParser
	{
	public:
		/** 
		 * @brief Anonymous enum to indicate parsing failure.
		 */
		enum
		{
			PARSE_FAILURE = -1
		};
		
		LLSDParser();
		virtual ~LLSDParser();
		
		/** 
		 * @brief Call this method to parse a stream for LLSD.
		 *
		 * This method parses the istream for a structured data. This
		 * method assumes that the istream is a complete llsd object --
		 * for example an opened and closed map with an arbitrary nesting
		 * of elements. This method will return after reading one data
		 * object, allowing continued reading from the stream by the
		 * caller.
		 * @param istr The input stream.
		 * @param data[out] The newly parse structured data.
		 * @param max_bytes The maximum number of bytes that will be in
		 * the stream. Pass in LLSDSerializationTraits::SIZE_UNLIMITED (-1)
		 * to set no byte limit.
		 * @param err_msg, if non-NULL set to an error message
		 * @return Returns true if parse succeeded.
		 */
		bool parse(std::istream& istr, LLSD& data, S32 max_bytes,
				   std::string* err_msg = NULL);
		
		/** 
		 * @brief Resets the parser so parse() can be called again for another <llsd> chunk.
		 */
		void reset()	{ doReset();	};
		
		
	protected:
		/** 
		 * @brief Pure virtual base for doing the parse.
		 *
		 * This method parses the istream for a structured data. This
		 * method assumes that the istream is a complete llsd object --
		 * for example an opened and closed map with an arbitrary nesting
		 * of elements. This method will return after reading one data
		 * object, allowing continued reading from the stream by the
		 * caller.
		 * @param istr The input stream.
		 * @param data[out] The newly parse structured data.
		 * @return Returns true if the parse succeeded.
		 */
		virtual bool doParse(std::istream& istr, LLSD& data) = 0;
		
		bool noteFailure(const std::string& msg);
		bool noteSuccess();
		
		/** 
		 * @brief Virtual default function for resetting the parser
		 */
		virtual void doReset()	{};
		
		/* @name Simple istream helper methods 
		 *
		 * These helper methods exist to help correctly use the
		 * mMaxBytesLeft without really thinking about it for most simple
		 * operations. Use of the streamtools in llstreamtools.h will
		 * require custom wrapping.
		 */
		//@{
		/** 
		 * @brief get a byte off the stream
		 *
		 * @param istr The istream to work with.
		 * @return returns the next character.
		 */
		int get(std::istream& istr) const;
		
		/** 
		 * @brief get several bytes off the stream into a buffer.
		 *
		 * @param istr The istream to work with.
		 * @param s The buffer to get into
		 * @param n Extract maximum of n-1 bytes and null temrinate.
		 * @param delim Delimiter to get until found.
		 * @return Returns istr.
		 */
		std::istream& get(
						  std::istream& istr,
						  char* s,
						  std::streamsize n,
						  char delim) const;
		
		/** 
		 * @brief get several bytes off the stream into a streambuf
		 *
		 * @param istr The istream to work with.
		 * @param sb The streambuf to read into
		 * @param delim Delimiter to get until found.
		 * @return Returns istr.
		 */
		std::istream& get(
						  std::istream& istr,
						  std::streambuf& sb,
						  char delim) const;
		
		/** 
		 * @brief ignore the next byte on the istream
		 *
		 * @param istr The istream to work with.
		 * @return Returns istr.
		 */
		std::istream& ignore(std::istream& istr) const;
		
		/** 
		 * @brief put the last character retrieved back on the stream
		 *
		 * @param istr The istream to work with.
		 * @param c The character to put back
		 * @return Returns istr.
		 */
		std::istream& putback(std::istream& istr, char c) const;
		
		/** 
		 * @brief read a block of n characters into a buffer
		 *
		 * @param istr The istream to work with.
		 * @param s The buffer to read into
		 * @param n The number of bytes to read.
		 * @return Returns istr.
		 */
		std::istream& read(std::istream& istr, char* s, std::streamsize n) const;
		//@}
		
	protected:
		/**
		 * @brief Accunt for bytes read outside of the istream helpers.
		 *
		 * Conceptually const since it only modifies mutable members.
		 * @param bytes The number of bytes read.
		 */
		void account(S32 bytes) const;
		
	protected:
		/**
		 * @brief boolean to set if byte counts should be checked during parsing.
		 */
		bool mCheckLimits;
		
		/**
		 * @brief The maximum number of bytes left to be parsed.
		 */
		mutable S32 mMaxBytesLeft;
		
		bool mErrorMsgNoted;
		std::string* mErrorMsg;
	};






	/// Like istream::read(), but try extra hard to read the requested bytes,
	/// looping if needed through multiple read() calls.
	std::streamsize fullread(std::istream& istr,
							 char* buf,
							 std::streamsize requested);

#ifdef LL_LEGACY
	// LEGACY SUPPORT: Binary format reading Notation encoded strings!  Oy!
	int deserialize_string_delim(
								 std::istream& istr,
								 std::string& value,
								 char delim);
#endif
	
}

#endif // LL_LLSDSERIALIZE_IMPL_H

