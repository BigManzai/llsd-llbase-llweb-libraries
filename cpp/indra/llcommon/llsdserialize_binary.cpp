/** 
 * @file llsdserialize_binary.cpp
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

#include "llsdserialize_impl.h"

#include <iostream>
#include <sstream>

#ifndef LL_WINDOWS
#include <arpa/inet.h> // htonl & ntohl
#endif
#ifdef LL_WINDOWS
#include <Winsock2.h> // htonl & ntohl
#endif

namespace LLSDSerialize
{
	union LLEndianSwapper
	{
		F64 d;
		struct {
			U32 one;
			U32 two;
		} i;
	};
	F64 ll_htond(F64 hostdouble)
	{
		LLEndianSwapper tmp;
		tmp.d = hostdouble;
		U32 u = htonl(tmp.i.one);
		if (u == tmp.i.one)
		{
			return hostdouble;
		}
		tmp.i.one = htonl(tmp.i.two);
		tmp.i.two = u;
		return tmp.d;
	}
	F64 ll_ntohd(F64 hostdouble)
	{
		LLEndianSwapper tmp;
		tmp.d = hostdouble;
		U32 u = ntohl(tmp.i.one);
		if (u == tmp.i.one)
		{
			return hostdouble;
		}
		tmp.i.one = ntohl(tmp.i.two);
		tmp.i.two = u;
		return tmp.d;
	}


	static const char BINARY_TRUE_SERIAL = '1';
	static const char BINARY_FALSE_SERIAL = '0';


	/** 
	 * @class LLSDBinaryParser
	 * @brief Parser which handles binary formatted LLSD.
	 */
	class LLSDBinaryParser : public LLSDParser
	{
	public:
#ifndef LL_LEGACY
		LLSDBinaryParser() { }
#else
		LLSDBinaryParser(bool checkLegacy = false)
			: mCheckLegacy(checkLegacy) { }
#endif
		virtual ~LLSDBinaryParser();
		
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
		 * @return Returns the number of LLSD objects parsed into
		 * data. Returns -1 on parse failure.
		 */
		virtual bool doParse(std::istream& istr, LLSD& data);


	private:
		bool parseOne(std::istream& istr, LLSD& data, bool topLevel = false);
		
		/** 
		 * @brief Parse a map from the istream
		 *
		 * @param istr The input stream.
		 * @param map The map to add the parsed data.
		 * @return Returns The number of LLSD objects parsed into data.
		 */
		bool parseMap(std::istream& istr, LLSD& map);
		
		/** 
		 * @brief Parse an array from the istream.
		 *
		 * @param istr The input stream.
		 * @param array The array to append the parsed data.
		 * @return Returns The number of LLSD objects parsed into data.
		 */
		bool parseArray(std::istream& istr, LLSD& array);
		
		/** 
		 * @brief Parse a string from the istream and assign it to data.
		 *
		 * @param istr The input stream.
		 * @param value[out] The string to assign.
		 * @return Retuns true if a complete string was parsed.
		 */
		bool parseString(std::istream& istr, std::string& value) const;

#ifdef LL_LEGACY
		bool mCheckLegacy;
#endif
	};



	/** 
	 * @class LLSDBinaryFormatter
	 * @brief Formatter which outputs the LLSD as a binary notation format.
	 *
	 * The binary format is a compact and efficient representation of
	 * structured data useful for when transmitting over a small data pipe
	 * or when transmission frequency is very high.<br>
	 *
	 * The normal boolalpha and real format commands are ignored.<br>
	 *
	 * All integers are transmitted in network byte order. The format is:<br>
	 * Undefined: '!'<br>
	 * Boolean: character '1' for true character '0' for false<br>
	 * Integer: 'i' + 4 bytes network byte order<br>
	 * Real: 'r' + 8 bytes IEEE double<br>
	 * UUID: 'u' + 16 byte unsigned integer<br>
	 * String: 's' + 4 byte integer size + string<br>
	 * Date: 'd' + 8 byte IEEE double for seconds since epoch<br>
	 * URI: 'l' + 4 byte integer size + string uri<br>
	 * Binary: 'b' + 4 byte integer size + binary data<br>
	 * Array: '[' + 4 byte integer size  + all values + ']'<br>
	 * Map: '{' + 4 byte integer size  every(key + value) + '}'<br>
	 *  map keys are serialized as 'k' + 4 byte integer size + string
	 */
	class LLSDBinaryFormatter
	{
	public:
		LLSDBinaryFormatter();
		virtual ~LLSDBinaryFormatter();
		
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
		 * This method serializes a network byte order size and the raw
		 * string contents.
		 * @param string The string to write.
		 * @param ostr The destination stream for the data.
		 */
		void formatString(const std::string& string, std::ostream& ostr) const;
	};




	/**
	 * LLSDBinaryParser
	 */
	// virtual
	LLSDBinaryParser::~LLSDBinaryParser()
	{
	}

	// virtual
	bool LLSDBinaryParser::doParse(std::istream& istr, LLSD& data)
	{
		return parseOne(istr, data, true);
	}
	
	bool LLSDBinaryParser::parseOne(std::istream& istr, LLSD& data, bool topLevel)
	{
		/**
		 * Undefined: '!'<br>
		 * Boolean: 't' for true 'f' for false<br>
		 * Integer: 'i' + 4 bytes network byte order<br>
		 * Real: 'r' + 8 bytes IEEE double<br>
		 * UUID: 'u' + 16 byte unsigned integer<br>
		 * String: 's' + 4 byte integer size + string<br>
		 *  strings also secretly support the notation format
		 * Date: 'd' + 8 byte IEEE double for seconds since epoch<br>
		 * URI: 'l' + 4 byte integer size + string uri<br>
		 * Binary: 'b' + 4 byte integer size + binary data<br>
		 * Array: '[' + 4 byte integer size  + all values + ']'<br>
		 * Map: '{' + 4 byte integer size  every(key + value) + '}'<br>
		 *  map keys are serialized as s + 4 byte integer size + string or in
		 *  the notation format.
		 */
		
		data.clear();
		
		char c;
		c = get(istr);
		if(!istr.good())
		{
			return 0;
		}
#ifdef LL_LEGACY
		if (topLevel && mCheckLegacy && c == '<')
		{
			// This code is mimics the old header testing code exactly.
			// It does not attempt to be a rigorous, or reasonable header test.
			
			char hdr_buf[20];
			istr.get(hdr_buf, sizeof(hdr_buf), '\n');
			
			std::string header = hdr_buf;
			
			std::string::size_type start = std::string::npos;
			std::string::size_type end = std::string::npos;
			start = header.find_first_not_of("? "); // already consumed '<'
			if (start != std::string::npos)
			{
				end = header.find_first_of(" ?", start);
			}
			if ((start == std::string::npos) || (end == std::string::npos))
				return noteFailure("invalid binary tag '<'");
			
			header = header.substr(start, end - start);
			
			if (header != "LLSD/Binary")
				return noteFailure("unrecognized legacy header");
			
			ws(istr);			
			return parseOne(istr, data, false);
		}
#endif
		switch(c)
		{
			case '{':
			{
				if (!parseMap(istr, data) || data.isUndefined())
				{
					return noteFailure("unparseable map");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading map");
				}
				break;
			}
				
			case '[':
			{
				if(!parseArray(istr, data) || data.isUndefined())
				{
					return noteFailure("unparseable array");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading array");
				}
				break;
			}
				
			case '!':
				data.clear();
				break;
				
			case '0':
				data = false;
				break;
				
			case '1':
				data = true;
				break;
				
			case 'i':
			{
				U32 value_nbo = 0;
				read(istr, (char*)&value_nbo, sizeof(U32));	 /*Flawfinder: ignore*/
				data = (S32)ntohl(value_nbo);
				if(istr.fail())
				{
					return noteFailure("input failure reading integer");
				}
				break;
			}
				
			case 'r':
			{
				F64 real_nbo = 0.0;
				read(istr, (char*)&real_nbo, sizeof(F64));	 /*Flawfinder: ignore*/
				data = ll_ntohd(real_nbo);
				if(istr.fail())
				{
					return noteFailure("input failure reading real");
				}
				break;
			}
				
			case 'u':
			{
				LLSD::UUID::bytes_t buffer;
				read(istr, (char *)buffer, LLSD::UUID::size);	 /*Flawfinder: ignore*/
				data = LLSD::UUID(buffer);
				if(istr.fail())
				{
					return noteFailure("input failure reading uuid");
				}
				break;
			}

#ifdef LL_LEGACY
			case '\'':
			case '"':
			{
				if (!mCheckLegacy)
				{
					return noteFailure("found legacy string");
				}
				
				std::string value;
				int cnt = deserialize_string_delim(istr, value, c);
				if(PARSE_FAILURE == cnt)
				{
					return noteFailure("invalid legacy string");
				}
				else
				{
					data = value;
					account(cnt);
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading legacy string");
				}
				break;
			}
#endif
				
			case 's':
			{
				std::string value;
				if(parseString(istr, value))
				{
					data = value;
				}
				else
				{
					return noteFailure("invalid string");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading string");
				}
				break;
			}
				
			case 'l':
			{
				std::string value;
				if(parseString(istr, value))
				{
					data = LLSD::URI(value);
				}
				else
				{
					return noteFailure("invalid uri");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading uri");
				}
				break;
			}
				
			case 'd':
			{
				F64 real = 0.0;
				read(istr, (char*)&real, sizeof(F64));	 /*Flawfinder: ignore*/
				data = LLSD::Date(real);
				if(istr.fail())
				{
					return noteFailure("input failure reading date");
				}
				break;
			}
				
			case 'b':
			{
				// We probably have a valid raw binary stream. determine
				// the size, and read it.
				U32 size_nbo = 0;
				read(istr, (char*)&size_nbo, sizeof(U32));	/*Flawfinder: ignore*/
				S32 size = (S32)ntohl(size_nbo);
				if(mCheckLimits && (size > mMaxBytesLeft))
				{
					return noteFailure("ran out of bytes reading binary");
				}
				else
				{
					std::vector<U8> value;
					if(size > 0)
					{
						value.resize(size);
						account(fullread(istr, (char*)&value[0], size));
					}
					data = value;
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading binary");
				}
				break;
			}
				
			default:
			{
				std::ostringstream msg;
				msg << "unrecognized tag '" << c << "'";
				return noteFailure(msg.str());
			}
		}
		return noteSuccess();
	}

	bool LLSDBinaryParser::parseMap(std::istream& istr, LLSD& map)
	{
		map = LLSD::emptyMap();
		U32 value_nbo = 0;
		read(istr, (char*)&value_nbo, sizeof(U32));		 /*Flawfinder: ignore*/
		S32 size = (S32)ntohl(value_nbo);
		S32 count = 0;
		char c = get(istr);
		while(c != '}' && (count < size) && istr.good())
		{
			std::string name;
			switch(c)
			{
				case 'k':
					if(!parseString(istr, name))
					{
						return noteFailure("invalid map key");
					}
					break;
#ifdef LL_LEGACY
				case '\'':
				case '"':
				{
					if (!mCheckLegacy)
					{
						return noteFailure("found legacy map key");
					}
					
					int cnt = deserialize_string_delim(istr, name, c);
					if(PARSE_FAILURE == cnt)
					{
						return noteFailure("invalid legacy map key");
					}
					account(cnt);
					break;
				}
#endif
			}
			LLSD child;
			if(parseOne(istr, child))
			{
				map.insert(name, child);
			}
			else
			{
				return noteFailure("invalid map value");
			}
			++count;
			c = get(istr);
		}
		if((c != '}') || (count < size))
		{
			// Make sure it is correctly terminated and we parsed as many
			// as were said to be there.
			return noteFailure("incorrectly sized map");
		}
		return noteSuccess();
	}

	bool LLSDBinaryParser::parseArray(std::istream& istr, LLSD& array)
	{
		array = LLSD::emptyArray();
		U32 value_nbo = 0;
		read(istr, (char*)&value_nbo, sizeof(U32));		 /*Flawfinder: ignore*/
		S32 size = (S32)ntohl(value_nbo);
		
		// *FIX: This would be a good place to reserve some space in the
		// array...
		
		S32 count = 0;
		char c = istr.peek();
		while((c != ']') && (count < size) && istr.good())
		{
			LLSD child;
			if(!parseOne(istr, child))
			{
				return noteFailure("invalid array value");
			}
			else
			{
				array.append(child);
			}
			++count;
			c = istr.peek();
		}
		c = get(istr);
		if((c != ']') || (count < size))
		{
			// Make sure it is correctly terminated and we parsed as many
			// as were said to be there.
			return noteFailure("incorrectly sized array");
		}
		return noteSuccess();
	}

	bool LLSDBinaryParser::parseString(
									   std::istream& istr,
									   std::string& value) const
	{
		// *FIX: This is memory inefficient.
		U32 value_nbo = 0;
		read(istr, (char*)&value_nbo, sizeof(U32));		 /*Flawfinder: ignore*/
		S32 size = (S32)ntohl(value_nbo);
		if(mCheckLimits && (size > mMaxBytesLeft)) return false;
		std::vector<char> buf;
		if(size)
		{
			buf.resize(size);
			account(fullread(istr, &buf[0], size));
			value.assign(buf.begin(), buf.end());
		}
		return true;
	}



	/**
	 * LLSDBinaryFormatter
	 */
	LLSDBinaryFormatter::LLSDBinaryFormatter()
	{
	}

	// virtual
	LLSDBinaryFormatter::~LLSDBinaryFormatter()
	{ }

	// virtual
	S32 LLSDBinaryFormatter::format(const LLSD& data, std::ostream& ostr) const
	{
		S32 format_count = 1;
		switch(data.type())
		{
			case LLSD::TypeMap:
			{
				ostr.put('{');
				U32 size_nbo = htonl(data.size());
				ostr.write((const char*)(&size_nbo), sizeof(U32));
				LLSD::map_const_iterator iter = data.beginMap();
				LLSD::map_const_iterator end = data.endMap();
				for(; iter != end; ++iter)
				{
					ostr.put('k');
					formatString((*iter).first, ostr);
					format_count += format((*iter).second, ostr);
				}
				ostr.put('}');
				break;
			}
				
			case LLSD::TypeArray:
			{
				ostr.put('[');
				U32 size_nbo = htonl(data.size());
				ostr.write((const char*)(&size_nbo), sizeof(U32));
				LLSD::array_const_iterator iter = data.beginArray();
				LLSD::array_const_iterator end = data.endArray();
				for(; iter != end; ++iter)
				{
					format_count += format(*iter, ostr);
				}
				ostr.put(']');
				break;
			}
				
			case LLSD::TypeUndefined:
				ostr.put('!');
				break;
				
			case LLSD::TypeBoolean:
				if(data.asBoolean()) ostr.put(BINARY_TRUE_SERIAL);
				else ostr.put(BINARY_FALSE_SERIAL);
				break;
				
			case LLSD::TypeInteger:
			{
				ostr.put('i');
				U32 value_nbo = htonl(data.asInteger());
				ostr.write((const char*)(&value_nbo), sizeof(U32));
				break;
			}
				
			case LLSD::TypeReal:
			{
				ostr.put('r');
				F64 value_nbo = ll_htond(data.asReal());
				ostr.write((const char*)(&value_nbo), sizeof(F64));
				break;
			}
				
			case LLSD::TypeUUID:
				ostr.put('u');
				ostr.write((char *)data.asUUID().bytes(), LLSD::UUID::size);
				break;
				
			case LLSD::TypeString:
				ostr.put('s');
				formatString(data.asString(), ostr);
				break;
				
			case LLSD::TypeDate:
			{
				ostr.put('d');
				F64 value = data.asReal();
				ostr.write((const char*)(&value), sizeof(F64));
				break;
			}
				
			case LLSD::TypeURI:
				ostr.put('l');
				formatString(data.asString(), ostr);
				break;
				
			case LLSD::TypeBinary:
			{
				// *FIX: memory inefficient.
				ostr.put('b');
				std::vector<U8> buffer = data.asBinary();
				U32 size_nbo = htonl(buffer.size());
				ostr.write((const char*)(&size_nbo), sizeof(U32));
				if(buffer.size()) ostr.write((const char*)&buffer[0], buffer.size());
				break;
			}
				
			default:
				// *NOTE: This should never happen.
				ostr.put('!');
				break;
		}
		return format_count;
	}

	void LLSDBinaryFormatter::formatString(
										   const std::string& string,
										   std::ostream& ostr) const
	{
		U32 size_nbo = htonl(string.size());
		ostr.write((const char*)(&size_nbo), sizeof(U32));
		ostr.write(string.c_str(), string.size());
	}

}


using namespace LLSDSerialize;

void LLSDBinaryTraits::format(const LLSD& sd, std::ostream& str)
{
	LLSDBinaryFormatter().format(sd, str);
}

bool LLSDBinaryTraits::parse(LLSD& sd, std::istream& str, S32 max_bytes, std::string* errorMessage)
{
	LLSDBinaryParser parser;
	return parser.parse(str, sd, max_bytes, errorMessage);
}


#ifdef LL_LEGACY

bool LLSDLegacyBinaryTraits::parse(LLSD& sd, std::istream& str, S32 max_bytes, std::string* errorMessage)
{
	LLSDBinaryParser parser(true);
	return parser.parse(str, sd, max_bytes, errorMessage);
}

#endif
