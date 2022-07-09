/**
 * @file llsdserialize_json.cpp
 * @brief Implementation of LLSD parsers and formatters
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

#include "llsdserialize_impl.h"

#include <iostream>
#include <iomanip>
#include <sstream>
#include <limits>

#define JSON_NONFINITES 1

namespace LLSDSerialize
{

    //
    // Encoding conversions between UTF-32/UTF-16 and UTF-8
    //

	typedef std::vector<U32> utf32str;
	typedef std::vector<U16> utf16str;
	const U32 UNICODE_REPLACEMENT_CHARACTER = 0xFFFD;
	const char* UNICODE_REPLACEMENT_CHARACTER_UTF8 = "\xEF\xBF\xBD";

	/**
	 * @brief decode UTF-8 string into UTF-32 sequence
	 * @param str UTF-8 string
	 * @return UTF-32 sequence (vector)
	 *
	 * Truncated sequences generate U+FFFD. Trailing bytes
	 * are not validated.
	 */
	utf32str utf8_to_utf32(const std::string& str)
	{
		utf32str utf32;
		std::string::const_iterator it = str.begin();
		while (it != str.end())
		{
			U32 cp;
			U8 ch = *it++, ch2, ch3, ch4;
			if (ch <= 0x7f)
			{
				cp = ch;
			}
			else if (0xc2 <= ch && ch <= 0xdf)
			{
				if (it == str.end()) { utf32.push_back(UNICODE_REPLACEMENT_CHARACTER); break; }
				ch2 = *it++ & 0x3f;
				cp = ((ch & 0x1f) << 6) | ch2;
			}
			else if (0xe0 <= ch && ch <= 0xef)
			{
				if (it == str.end()) { utf32.push_back(UNICODE_REPLACEMENT_CHARACTER); break; }
				ch2 = *it++ & 0x3f;
				if (it == str.end()) { utf32.push_back(UNICODE_REPLACEMENT_CHARACTER); break; }
				ch3 = *it++ & 0x3f;
				cp = ((ch & 0x0f) << 12) | ch2 << 6 | ch3;
			}
			else if (0xf0 <= ch && ch <= 0xf4)
			{
				if (it == str.end()) { utf32.push_back(UNICODE_REPLACEMENT_CHARACTER); break; }
				ch2 = *it++ & 0x3f;
				if (it == str.end()) { utf32.push_back(UNICODE_REPLACEMENT_CHARACTER); break; }
				ch3 = *it++ & 0x3f;
				if (it == str.end()) { utf32.push_back(UNICODE_REPLACEMENT_CHARACTER); break; }
				ch4 = *it++ & 0x3f;
				cp = ((ch & 0x07) << 18) | ch2 << 12 | ch3 << 6 | ch4;
			}
			utf32.push_back(cp);
		}

		return utf32;
	}

	/**
	 * @brief decode UTF-16 sequence into UTF-32 sequence
	 * @param str UTF-16 sequence (vector) with surrogate pairs
	 * @return UTF-32 sequence (vector)
	 *
	 * Invalid surrogate pair sequences generated unpredictable
	 * results.
	 */
	utf32str utf16_to_utf32(const utf16str& str)
	{
		utf32str utf32;
		U16 sp1 = 0;
		
		for (utf16str::const_iterator it = str.begin(); it != str.end(); it++)
		{
			U16 cp = *it;

			// Decode UTF-16 surrogate pairs
			// *NOTE: This will produce garbage if input is invalid
			if (0xD800 <= cp && cp <= 0xDBFF)
			{
				// First half of surrogate pair
				sp1 = cp;
				continue;
			}
			else if (0xDC00 <= cp && cp <= 0xDFFF)
			{
				// Second half of surrogate pair
				U16 sp2 = cp;
				utf32.push_back(((sp1 & 0x03FF) << 10) + (sp2 & 0x03FF) + 0x10000);
				continue;
			}
			sp1 = 0;

			utf32.push_back(cp);
		}

		return utf32;
	}

	/**
	 * @brief encode UTF-32 sequence into UTF-8 string
	 * @param str UTF-32 sequence (vector)
	 * @return UTF-8 string
	 *
	 * UTF-32 code points beyond U+10FFFF generate
	 * the Unicode replacement character U+FFFD
	 */
	std::string utf32_to_utf8(const utf32str& str)
	{
		std::ostringstream oss;
		
		for (utf32str::const_iterator it = str.begin(); it != str.end(); it++)
		{
			U32 cp = *it;
			
			if (0x0000 <= cp && cp <= 0x007f)
			{
				oss << (char)cp;
			}
			else if (0x0080 <= cp && cp <= 0x07ff)
			{
				oss << (char)(0xc0 | ((cp >>  6) & 0x1f));
				oss << (char)(0x80 | ((cp >>  0) & 0x3f));
			}
			else if (0x0800 <= cp && cp <= 0xffff)
			{
				oss << (char)(0xe0 | ((cp >> 12) & 0x0f));
				oss << (char)(0x80 | ((cp >>  6) & 0x3f));
				oss << (char)(0x80 | ((cp >>  0) & 0x3f));
			}
			else if (0x10000 <= cp && cp <= 0x10ffff)
			{
				oss << (char)(0xf0 | ((cp >> 18) & 0x07));
				oss << (char)(0x80 | ((cp >> 12) & 0x3f));
				oss << (char)(0x80 | ((cp >>  6) & 0x3f));
				oss << (char)(0x80 | ((cp >>  0) & 0x3f));
			}
			else
			{
				// Invalid Unicode code point
				oss << UNICODE_REPLACEMENT_CHARACTER_UTF8;
			}
		}

		return oss.str();
	}

	/**
	 * @brief hex value (0...15) for an ASCII digit
	 * @param c ASCII digit (case insensitive)
	 * @return the hex value (0...15) or -1 if invalid
	 */
	static int hexvalue(char c)
	{
		if ('0' <= c && c <= '9') return c-'0';
		if ('A' <= c && c <= 'F') return c-'A'+10;
		if ('a' <= c && c <= 'f') return c-'a'+10;
		return -1;
	}


	/**
	 * @class LLSDJSONParser
	 * @brief Parser which handles JSON formatted LLSD.
	 */
	class LLSDJSONParser : public LLSDParser
	{
	public:
		LLSDJSONParser() { }
		virtual ~LLSDJSONParser();

	protected:
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
		 * @return Returns success or failure
		 * data. Returns -1 on parse failure.
		 */
		virtual bool doParse(std::istream& istr, LLSD& data);


	private:
		bool parseValue(std::istream& istr, LLSD& data);
		bool parseString(std::istream& istr, LLSD& string);
		bool parseNumber(std::istream& istr, LLSD& number);
		bool parseObject(std::istream& istr, LLSD& object);
		bool parseArray(std::istream& istr, LLSD& array);

		// Helpers
		bool ws(std::istream& istr);
		int  digit(std::istream& istr);
		bool test(std::istream& istr, const char* substr);
		bool require(std::istream& istr, const char* substr);
	};



	/**
	 * @class LLSDJSONFormatter
	 * @brief Formatter which outputs the LLSD as a JSON format.
	 *
	 * The JSON format is defined in RFC 4627 and well documented
	 * at http://json.org
	 */
	class LLSDJSONFormatter
	{
	public:
		LLSDJSONFormatter();
		virtual ~LLSDJSONFormatter();

		/**
		 * @brief Call this method to format an LLSD to a stream.
		 *
		 * @param data The data to write.
		 * @param ostr The destination stream for the data.
		 * @return Returns The number of LLSD objects fomatted out
		 */
		S32 format(const LLSD& data, std::ostream& ostr) const;

	protected:
		/**
		 * @brief Helper method to serialize strings
		 *
		 * This method serializes a UTF-8 encoded string in the
		 * JSON encoding style (UTF-16 code points escaped into ASCII).
		 * @param string The string to write.
		 * @param ostr The destination stream for the data.
		 */
		void formatString(const std::string& string, std::ostream& ostr) const;
	};



	/**
	 * LLSDJSONParser
	 */
	// virtual
	LLSDJSONParser::~LLSDJSONParser()
	{
	}

	// virtual
	bool LLSDJSONParser::doParse(std::istream& istr, LLSD& data)
	{
		if (!parseValue(istr, data))
			return false;

		ws(istr);

		if (!istr.eof())
			return noteFailure("unrecognized data");

		return true;
	}


	/**
	 * @brief Consume JSON whitespace from stream
	 * @param istr input stream
	 * @return always returns true, for chaining in do...while
	 */
	bool LLSDJSONParser::ws(std::istream& istr)
	{
		// ws = *( %x20 / %x09 / %x0A / %x0D ) ; space/tab/LF/CR
		while (!istr.fail())
		{
			int peek = istr.peek();
			if (peek == 0x20 || peek == 0x09 || peek == 0x0A || peek == 0x0D)
			{
				istr.get();
			}
			else
			{
				break;
			}
		}
		return true;
	}

	/**
	 * @brief Test/consume ASCII digit
	 * @param istr input stream
	 * @return Returns an ASCII digit ('0' ... '9') or -1
	 */
	int LLSDJSONParser::digit(std::istream& istr)
	{
		int ch = istr.peek();
		if (ch < '0' || ch > '9')
		{
			return -1;
		}

		return istr.get();
	}

	/**
	 * @brief Test for a token in a stream, consume if present
	 * @param istr input stream
	 * @param token string to test for
	 * @return True if matched and consumed, false otherwise.
	 */
	bool LLSDJSONParser::test(std::istream& istr, const char* token)
	{
		std::istream::iostate state = istr.rdstate();
		std::streampos mark = istr.tellg();
		unsigned int i, len = strlen(token);

		for (i = 0; i < len; i++)
		{
			if (istr.fail() || token[i] != istr.get())
			{
				break;
			}
		}

		if (i == len)
		{
			return true;
		}
		else
		{
			istr.clear(state);
			istr.seekg(mark);
			return false;
		}
	}

	/**
	 * @brief Require a token in a stream; notes failure if absent
	 * @param istr input stream
	 * @param token string to test for
	 * @return True if matched, false otherwise
	 */
	bool LLSDJSONParser::require(std::istream& istr, const char* token)
	{
		unsigned int i, len = strlen(token);

		for (i = 0; i < len; i++)
		{
			if (istr.fail() || token[i] != istr.get())
			{
				std::ostringstream msg;
				msg << "expected " << token;
				return noteFailure(msg.str());
			}
		}
		return noteSuccess();
	}

	/**
	 * @brief Parse a single JSON value from a stream
	 * @param istr input stream
	 * @param data [out] the value (which may be undefined)
	 * @return success/failure
	 */
	bool LLSDJSONParser::parseValue(std::istream& istr, LLSD& data)
	{
		data.clear();

		// value = false / null / true / object / array / number / string

		ws(istr);

		if (test(istr, "false"))
		{
			data = LLSD(false);
			return noteSuccess();
		}

		if (test(istr, "null"))
		{
			data = LLSD();
			return noteSuccess();
		}

		if (test(istr, "true"))
		{
			data = LLSD(true);
			return noteSuccess();
		}

		if (!parseObject(istr, data))
			return noteFailure("unparsable object");
		if (data.isDefined())
			return noteSuccess();

		if (!parseArray(istr, data))
			return noteFailure("unparsable array");
		if (data.isDefined())
			return noteSuccess();

		if (!parseNumber(istr, data))
			return noteFailure("unparsable number");
		if (data.isDefined())
			return noteSuccess();

		if (!parseString(istr, data))
			return noteFailure("unparsable string");
		if (data.isDefined())
			return noteSuccess();

		return noteFailure("expected value");
	}

	/**
	 * @brief parse a JSON string
	 * @param istr input stream
	 * @param string [out] parsed string, undefined if no match.
	 *        Encoded as UTF-8
	 * @return True if either not a string or string parsed,
	 *         false if parsing fails (invalid stream)
	 */
	bool LLSDJSONParser::parseString(std::istream& istr, LLSD& string)
	{
		if (!test(istr, "\"")) return true; // but undefined

		utf16str utf16;

		for(;;)
		{
			U8 ch = istr.get();

			if (istr.fail())
			{
				return noteFailure("unterminated string");
			}
			else if (ch == 0x22) // quotation-mark = %0x22 ; '"'
			{
				break;
			}
			else if (ch == 0x5C) // escape = %x5C ; '\'
			{
				ch = istr.get();
				if (istr.fail()) return noteFailure("truncated escape sequence");

				switch (ch)
				{
				case 0x22: utf16.push_back('"'); break;
				case 0x5C: utf16.push_back('\\'); break;
				case 0x2F: utf16.push_back('/'); break;
				case 0x62: utf16.push_back('\b'); break;
				case 0x66: utf16.push_back('\f'); break;
				case 0x6E: utf16.push_back('\n'); break;
				case 0x72: utf16.push_back('\r'); break;
				case 0x74: utf16.push_back('\t'); break;
				case 0x75: // \uXXXX
				{
					U16 cp = 0;
					int hex;

					cp = 0;

					for (int i = 0; i < 4; i++)
					{
						ch = istr.get();
						if (istr.fail()) return noteFailure("truncated escape sequence");
						hex = hexvalue(ch);
						if (hex == -1) return noteFailure("invalid escape sequence");
						cp = (cp << 4) | hex;
					}

					// *TODO: handle surrogate pairs here

					utf16.push_back(cp);
					break;
				}
				default:
					return noteFailure("unexpected escape sequence");
				}
			}
			else if (ch < 0x80)
			{
				// *TODO: Reject embedded control characters?
				utf16.push_back(ch);
			}
			else
			{
				// *NOTE: UTF-8 sequences are passed through the subsequent
				// UTF-16 -> UTF-8 conversion unscathed; too fragile?
				utf16.push_back(ch);				
			}
		}

		std::string decoded = utf32_to_utf8(utf16_to_utf32(utf16));

		string = LLSD(decoded);
		return noteSuccess();
	}

	/**
	 * @brief parse a JSON number
	 * @param istr input stream
	 * @param number [out] parsed number, undefined if no match
	 * @return True if either not a number or number parsed,
	 *         false if parsing fails (invalid stream)
	 */
	bool LLSDJSONParser::parseNumber(std::istream& istr, LLSD& number)
	{
		// number = [ minus ] int [ frac ] [ exp ]
		std::ostringstream accum;
		bool isint = true;

#ifdef JSON_NONFINITES
		// "Numeric values that cannot be represented as sequences of digits
		// (such as Infinity and NaN) are not permitted" -- RFC 4627
		if (test(istr, "Infinity"))
		{
			number = LLSD(std::numeric_limits<double>::infinity());
			return noteSuccess();
		}

		if (test(istr, "-Infinity"))
		{
			number = LLSD(-std::numeric_limits<double>::infinity());
			return noteSuccess();
		}

		if (test(istr, "NaN"))
		{
			number = LLSD(std::numeric_limits<double>::quiet_NaN());
			return noteSuccess();
		}
#endif // JSON_NONFINITES

		// [ minus ]
		bool neg = test(istr, "-");
		if (neg) accum << "-";

		// int
		// int = zero / ( digit1-9 *DIGIT )
		int d = digit(istr);
		if (d == -1)
		{
			if (neg)
				return noteFailure("expected digit");
			return true; // but undefined
		}

		accum << (char)d;

		if (d != '0')
		{
			while ((d = digit(istr)) != -1)
			{
				accum << (char)d;
			}
		}

		// [ frac ]
		// frac = decimal-point 1*DIGIT
		if (test(istr, "."))
		{
			isint = false;
			accum << ".";
			d = digit(istr);
			if (d == -1)
				return noteFailure("expected digit");
			do {
				accum << (char)d;
			}
			while ((d = digit(istr)) != -1);
		}

		// [ exp ]
		// exp = e [ minus / plus ] 1*DIGIT
		if (test(istr, "e") || test(istr, "E"))
		{
			isint = false;
			accum << "e";

			if (test(istr, "-")) accum << "-";
			else if (test(istr, "+")) accum << "+";
			d = digit(istr);

			if (d == -1)
				return noteFailure("expected digit");
			do {
				accum << (char)d;
			}
			while ((d = digit(istr)) != -1);
		}

		if (isint)
			number = LLSD(atoi(accum.str().c_str()));
		else
			number = LLSD(atof(accum.str().c_str()));

		return noteSuccess();
	}

	/**
	 * @brief parse a JSON object (map)
	 * @param istr input stream
	 * @param object [out] parsed object, undefined if no match
	 * @return True if either not an object or object parsed,
	 *         false if parsing fails (invalid stream)
	 */
	bool LLSDJSONParser::parseObject(std::istream& istr, LLSD& object)
	{
		if (!test(istr, "{")) return true; // but undefined
		ws(istr);

		object = LLSD::emptyMap();

		if (!test(istr, "}"))
		{
			do {
				LLSD key;
				if (!parseString(istr, key) || key.isUndefined())
					return noteFailure("expected key");

				ws(istr);

				if (!require(istr, ":"))
					return noteFailure("expected colon");

				ws(istr);

				LLSD val;
				if (!parseValue(istr, val))
					return noteFailure("expected value");

				object.insert(key.asString(), val);

				ws(istr);

			} while(test(istr, ",") && ws(istr));

			if (!require(istr, "}"))
				return noteFailure("expected close brace");
		}

		return noteSuccess();
	}

	/**
	 * @brief parse a JSON array
	 * @param istr input stream
	 * @param array [out] parsed array, undefined if no match
	 * @return True if either not an array or array parsed,
	 *         false if parsing fails (invalid stream)
	 */
	bool LLSDJSONParser::parseArray(std::istream& istr, LLSD& array)
	{
		if (!test(istr, "[")) return true; // but undefined
		ws(istr);

		array = LLSD::emptyArray();

		if (!test(istr, "]"))
		{
			do {
				LLSD val;
				if (!parseValue(istr, val))
					return noteFailure("expected value");
				array.append(val);
				ws(istr);
			} while (test(istr, ",") && ws(istr));

			if (!require(istr, "]"))
				return noteFailure("expected close bracket");
		}

		return noteSuccess();
	}


	/**
	 * LLSDJSONFormatter
	 */
	LLSDJSONFormatter::LLSDJSONFormatter()
	{
	}

	// virtual
	LLSDJSONFormatter::~LLSDJSONFormatter()
	{ }

	// virtual
	S32 LLSDJSONFormatter::format(const LLSD& data, std::ostream& ostr) const
	{
		S32 format_count = 1;
		switch(data.type())
		{
			case LLSD::TypeMap:
			{
				ostr.put('{');
				LLSD::map_const_iterator begin = data.beginMap();
				LLSD::map_const_iterator end = data.endMap();
				for(LLSD::map_const_iterator iter = begin; iter != end; ++iter)
				{
					if (iter != begin) { ostr.put(','); }
					formatString((*iter).first, ostr);
					ostr.put(':');
					format_count += format((*iter).second, ostr);
				}
				ostr.put('}');
				break;
			}

			case LLSD::TypeArray:
			{
				ostr.put('[');
				LLSD::array_const_iterator begin = data.beginArray();
				LLSD::array_const_iterator end = data.endArray();
				for(LLSD::array_const_iterator iter = begin; iter != end; ++iter)
				{
					if (iter != begin) { ostr.put(','); }
					format_count += format(*iter, ostr);
				}
				ostr.put(']');
				break;
			}

			case LLSD::TypeUndefined:
				ostr << "null";
				break;

			case LLSD::TypeBoolean:
				if(data.asBoolean())
					ostr << "true";
				else
					ostr << "false";
				break;

			case LLSD::TypeInteger:
			{
				S32 nval = data.asInteger();
				ostr << nval;
				break;
			}

			case LLSD::TypeReal:
			{
				F64 nval = data.asReal();

#ifdef JSON_NONFINITES
				if (nval != nval)
				{
					ostr << "NaN";
				}
				else if (nval == std::numeric_limits<double>::infinity())
				{
					ostr << "Infinity";
				}
				else if (nval == -std::numeric_limits<double>::infinity())
				{
					ostr << "-Infinity";
				}
				else
#endif // JSON_NONFINITES
				{
					ostr << nval;
				}
				break;
			}

			case LLSD::TypeUUID:
				formatString(data.asString(), ostr);
				break;

			case LLSD::TypeString:
				formatString(data.asString(), ostr);
				break;

			case LLSD::TypeDate:
				formatString(data.asString(), ostr);
				break;

			case LLSD::TypeURI:
				formatString(data.asString(), ostr);
				break;

			case LLSD::TypeBinary:
				formatString(data.asString(), ostr);
				break;

		    default:
				// *NOTE: This should never happen.
				ostr << "null";
				break;
		}
		return format_count;
	}

	/**
	 * @brief Write a quoted, escaped string
	 * @param string UTF-8 string to output
	 * @param ostr output stream
	 */
	void LLSDJSONFormatter::formatString(const std::string& string,
										 std::ostream& ostr) const
	{
		ostr.put('"');

		// UTF-8 --> UTF-32
		utf32str utf32 = utf8_to_utf32(string);

		// UTF-32 --> ASCII JSON escaped UTF-16 string
		for (utf32str::const_iterator it = utf32.begin();
			 it != utf32.end(); it++)
		{
			long cp = *it;

			switch (cp)
			{
			case 0x0022: ostr << "\\\"";   break; // quotation mark
			case 0x005C: ostr << "\\\\"; break; // reverse solidus
			case 0x002F: ostr << "/";    break; // solidus
			case 0x0008: ostr << "\\b";  break; // backspace
			case 0x000C: ostr << "\\f";  break; // form feed
			case 0x000A: ostr << "\\n";  break; // line feed
			case 0x000D: ostr << "\\r";  break; // carriage return
			case 0x0009: ostr << "\\t";  break; // tab
			default:
				if (0x20 <= cp && cp <= 0x7f)
				{
					// non-escaped printable ASCII
					ostr << (char)cp;
				}
				else if (cp < 0x10000)
				{
					// \uXXXX
					std::ostream::fmtflags oldFlags = ostr.flags();
					char oldFill = ostr.fill();
					ostr << "\\u" << std::setw(4) << std::hex << std::setfill('0') << cp;
					ostr.fill(oldFill);
					ostr.setf(oldFlags);
				}
				else
				{
					// encode UTF-16 surrogate pair as \uXXXX\uXXXX
					cp -= 0x10000;
					U16 sp1 = 0xd800 | ((cp >> 10) & 0x3FF);
					U16 sp2 = 0xdc00 | (cp & 0x3FF);

					std::ostream::fmtflags oldFlags = ostr.flags();
					char oldFill = ostr.fill();
					ostr << std::hex << std::setfill('0');
					ostr << "\\u" << std::setw(4) << sp1;
					ostr << "\\u" << std::setw(4) << sp2;
					ostr.fill(oldFill);
					ostr.setf(oldFlags);
				}
				break;
			}
		}
		ostr.put('"');
	}
}


using namespace LLSDSerialize;

void LLSDJSONTraits::format(const LLSD& sd, std::ostream& str)
{
	LLSDJSONFormatter().format(sd, str);
}

bool LLSDJSONTraits::parse(LLSD& sd, std::istream& str, S32 max_bytes, std::string* errorMessage)
{
	LLSDJSONParser parser;
	return parser.parse(str, sd, max_bytes, errorMessage);
}
