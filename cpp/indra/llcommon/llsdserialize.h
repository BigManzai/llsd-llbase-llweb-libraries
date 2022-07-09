/** 
 * @file llsdserialize.h
 * @brief Declaration of parsers and formatters for LLSD
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

#ifndef LL_LLSDSERIALIZE_H
#define LL_LLSDSERIALIZE_H

#include <iosfwd>

#include "llsd.h"


/// @name Serialization Formats
//@{

/** 
 * @brief Uniform interface to static serialization functions.
 *
 * Subclasses of this class represent various serialization formats. Those
 * formats are supported by two static functions, with types Format and Parse.
 * This base class serves to provide the common signature that those functions
 * must adhere to, and that the templatized streaming classes, LLSDOStreamer and
 * LLSDIStreamer, can rely on them.
 */
class LLSDSerializationTraits
{
public:
	static const S32 SIZE_UNLIMITED = -1;

	typedef void Format(const LLSD& sd, std::ostream& str);
		///^ Type of functions that format an LLSD onto a stream

	typedef bool Parse(LLSD& sd, std::istream& str, S32 max_bytes,
					   std::string* errorMessage);
		/**^ Type of functions that parse an LLSD value from a stream.
		 * 
		 * Note: The next character of the stream is assumed to be the start of
		 * the serailized value. If the parse is successful, the stream is left
		 * pointing to after the LLSD. See notes for specific formats.
		 *
		 * @param max_bytes If set, limits the number of bytes read from the
		 * stream, generating a parse error if that many bytes is read and the
		 * LLSD isn't complete. Pass SIZE_UNLIMITED to have not limit.
		 * @param errorMessage If non-NULL, and there is a parse error, the
		 * string there will be set to a parse error message.
		 *
		 * @return Returns true on successful parse, false on error (in which
		 * case the data will be set to an LLSD undef value, and errorMessage
		 * set, if supplied).
		 */
};


/// XML serialization of LLSD
class LLSDXMLTraits : public LLSDSerializationTraits
{
public:
	static Format format;
	static Parse parse;
		///^ Note: The end of an XML document is unclear, and this may consume
		/// trailing whitespace.
};

/// XML output of LLSD with newlines and indented tags
class LLSDPrettyXMLTraits : public LLSDSerializationTraits
{
public:
	static Format format;
};

/// Binary serialization of LLSD
class LLSDBinaryTraits : public LLSDSerializationTraits
{
public:
	static Format format;
	static Parse parse;
};

/// JSON serialization of LLSD
class LLSDJSONTraits : public LLSDSerializationTraits
{
public:
	static Format format;
	static Parse parse;
};

/// An LLSD output serialization suitable for inclusion in log messages 
class LLSDLogTraits : public LLSDSerializationTraits
{
public:
	static Format format;
};


#ifdef LL_LEGACY

/// @deprecated Notation serialization
class LLSDNotationTraits : public LLSDSerializationTraits
{
public:
	static Format format;
	static Parse parse;
};

/// @deprecated Parse XML that may be prefixed with an old, legacy header 
class LLSDLegacyXMLTraits : public LLSDSerializationTraits
{
public:
	static Parse parse;
};

/// @deprecated Parse Binary that may be prefixed with an old, legacy header 
class LLSDLegacyBinaryTraits : public LLSDSerializationTraits
{
public:
	static Parse parse;
};

#endif // LL_LEGACY

//@}


/** @name Integration With iostream
 *
 * The class templates LLSDOStreamer and LLSDIStreamer enable convienent 
 * integration with std::ostream and std::istream respectively. These classes
 * are specialized on a LLSDSerializationTraits subclass to produce read or
 * write a given format.
 *
 * Typically, LLSDOStreamer and LLSDIStreamer are not used directly. Instead,
 * the named typedefs provide a very concise, clean interface:
 * 
 * @code
 *		// writing XML to a stream:
 *		outputStream << LLSDToXML(someLLSD);
 *
 *      // reading XML from a stream:
 *      inputStream >> LLSDFromXML(someLLSD);
 *
 *		// reading XML from a stream and catching errors:
 *      LLSDFromXML parser(someLLSD);
 *      if (parser.parse(inputStream))
 *		{
 *			handleError(... parser.errorMessage() ...);
 *	    }
 * @endcode
 */
//@{

template<class Traits>
class LLSDOStreamer
{
public:
	LLSDOStreamer(const LLSD& data) : mData(data) { }

	void format(std::ostream& str) const
	{
		Traits::format(mData, str);
	}
	
	friend std::ostream& operator<<(std::ostream& str,
									const LLSDOStreamer<Traits>& streamer)
	{
		streamer.format(str);
		return str;
	}
	
private:
	const LLSD& mData;
};

template<class Traits>
class LLSDIStreamer
{
public:
	LLSDIStreamer(LLSD& data) : mData(data) { }
	
	bool parse(std::istream& str, S32 max_bytes = Traits::SIZE_UNLIMITED)
	{
		return Traits::parse(mData, str, max_bytes, &mErrorMessage);
	}
	
	bool parse(std::istream& str, S32 max_bytes = Traits::SIZE_UNLIMITED) const
	{
		return Traits::parse(mData, str, max_bytes, NULL);
	}

	std::string errorMessage()
	{
		return mErrorMessage;
	}

	friend std::istream& operator>>(std::istream& str,
									const LLSDIStreamer<Traits>& streamer)
	{
		streamer.parse(str);
		return str;
	}
	
private:
	LLSD& mData;
	std::string mErrorMessage;
};


typedef LLSDOStreamer<LLSDXMLTraits>		LLSDToXML;
typedef LLSDIStreamer<LLSDXMLTraits>		LLSDFromXML;
typedef LLSDOStreamer<LLSDPrettyXMLTraits>	LLSDToPrettyXML;

typedef LLSDOStreamer<LLSDBinaryTraits>		LLSDToBinary;
typedef LLSDIStreamer<LLSDBinaryTraits>		LLSDFromBinary;

typedef LLSDOStreamer<LLSDJSONTraits>		LLSDToJSON;
typedef LLSDIStreamer<LLSDJSONTraits>		LLSDFromJSON;

typedef LLSDOStreamer<LLSDLogTraits>		LLSDToLog;

#ifdef LL_LEGACY
typedef LLSDIStreamer<LLSDLegacyXMLTraits>	  LLSDFromLegacyXML;
typedef LLSDIStreamer<LLSDLegacyBinaryTraits> LLSDFromLegacyBinary;

typedef LLSDOStreamer<LLSDNotationTraits>	LLSDToNotation;
typedef LLSDIStreamer<LLSDNotationTraits>	LLSDFromNotation;
#endif

//@}


#endif // LL_LLSDSERIALIZE_H
