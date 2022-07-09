/** 
 * @file llsdserialize_notation.cpp
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
#include <stdlib.h> // for strtol

#include "apr_base64.h"


#ifdef LL_LEGACY

static const std::string NOTATION_TRUE_SERIAL("true");
static const std::string NOTATION_FALSE_SERIAL("false");

namespace LLSDSerialize
{
	/**
	 * Local functions.
	 */
	/**
	 * @brief Figure out what kind of string it is (raw or delimited) and handoff.
	 *
	 * @param istr The stream to read from.
	 * @param value [out] The string which was found.
	 * @param max_bytes The maximum possible length of the string. Passing in
	 * a negative value will skip this check.
	 * @return Returns number of bytes read off of the stream. Returns
	 * PARSE_FAILURE (-1) on failure.
	 */
	int deserialize_string(std::istream& istr, std::string& value, S32 max_bytes);

	/**
	 * @brief Parse a delimited string. 
	 *
	 * @param istr The stream to read from, with the delimiter already popped.
	 * @param value [out] The string which was found.
	 * @param d The delimiter to use.
	 * @return Returns number of bytes read off of the stream. Returns
	 * PARSE_FAILURE (-1) on failure.
	 */
	int deserialize_string_delim(std::istream& istr, std::string& value, char d);

	/**
	 * @brief Read a raw string off the stream.
	 *
	 * @param istr The stream to read from, with the (len) parameter
	 * leading the stream.
	 * @param value [out] The string which was found.
	 * @param d The delimiter to use.
	 * @param max_bytes The maximum possible length of the string. Passing in
	 * a negative value will skip this check.
	 * @return Returns number of bytes read off of the stream. Returns
	 * PARSE_FAILURE (-1) on failure.
	 */
	int deserialize_string_raw(
							   std::istream& istr,
							   std::string& value,
							   S32 max_bytes);

	/**
	 * @brief helper method for dealing with the different notation boolean format.
	 *
	 * @param istr The stream to read from with the leading character stripped.
	 * @param data [out] the result of the parse.
	 * @param compare The string to compare the boolean against
	 * @param vale The value to assign to data if the parse succeeds.
	 * @return Returns number of bytes read off of the stream. Returns
	 * PARSE_FAILURE (-1) on failure.
	 */
	int deserialize_boolean(
							std::istream& istr,
							LLSD& data,
							const std::string& compare,
							bool value);

	/**
	 * @brief Do notation escaping of a string to an ostream.
	 *
	 * @param value The string to escape and serialize
	 * @param str The stream to serialize to.
	 */
	void serialize_string(const std::string& value, std::ostream& str);



	static int hexvalue(char c)
	{
		if ('0' <= c && c <= '9') return c-'0';
		if ('A' <= c && c <= 'F') return c-'A'+10;
		if ('a' <= c && c <= 'f') return c-'a'+10;
		return 0;
	}


	/**
	 * local functions
	 */
	int deserialize_string(std::istream& istr, std::string& value, S32 max_bytes)
	{
		int c = istr.get();
		if(istr.fail())
		{
			// No data in stream, bail out but mention the character we
			// grabbed.
			return LLSDParser::PARSE_FAILURE;
		}
		
		int rv = LLSDParser::PARSE_FAILURE;
		switch(c)
		{
			case '\'':
			case '"':
				rv = deserialize_string_delim(istr, value, c);
				break;
			case 's':
				// technically, less than max_bytes, but this is just meant to
				// catch egregious protocol errors. parse errors will be
				// caught in the case of incorrect counts.
				rv = deserialize_string_raw(istr, value, max_bytes);
				break;
			default:
				break;
		}
		if(LLSDParser::PARSE_FAILURE == rv) return rv;
		return rv + 1; // account for the character grabbed at the top.
	}

	int deserialize_string_delim(
								 std::istream& istr,
								 std::string& value,
								 char delim)
	{
		std::ostringstream write_buffer;
		bool found_escape = false;
		bool found_hex = false;
		bool found_digit = false;
		U8 byte = 0;
		int count = 0;
		
		while (true)
		{
			int next_byte = istr.get();
			++count;
			
			if(istr.fail())
			{
				// If our stream is empty, break out
				value = write_buffer.str();
				return LLSDParser::PARSE_FAILURE;
			}
			
			char next_char = (char)next_byte; // Now that we know it's not EOF
			
			if(found_escape)
			{
				// next character(s) is a special sequence.
				if(found_hex)
				{
					if(found_digit)
					{
						found_digit = false;
						found_hex = false;
						found_escape = false;
						byte = byte << 4;
						byte |= hexvalue(next_char);
						write_buffer << byte;
						byte = 0;
					}
					else
					{
						// next character is the first nybble of
						//
						found_digit = true;
						byte = hexvalue(next_char);
					}
				}
				else if(next_char == 'x')
				{
					found_hex = true;
				}
				else
				{
					switch(next_char)
					{
						case 'a':
							write_buffer << '\a';
							break;
						case 'b':
							write_buffer << '\b';
							break;
						case 'f':
							write_buffer << '\f';
							break;
						case 'n':
							write_buffer << '\n';
							break;
						case 'r':
							write_buffer << '\r';
							break;
						case 't':
							write_buffer << '\t';
							break;
						case 'v':
							write_buffer << '\v';
							break;
						default:
							write_buffer << next_char;
							break;
					}
					found_escape = false;
				}
			}
			else if(next_char == '\\')
			{
				found_escape = true;
			}
			else if(next_char == delim)
			{
				break;
			}
			else
			{
				write_buffer << next_char;
			}
		}
		
		value = write_buffer.str();
		return count;
	}

	int deserialize_string_raw(
							   std::istream& istr,
							   std::string& value,
							   S32 max_bytes)
	{
		int count = 0;
		const S32 BUF_LEN = 20;
		char buf[BUF_LEN];		/* Flawfinder: ignore */
		istr.get(buf, BUF_LEN - 1, ')');
		count += istr.gcount();
		int c = istr.get();
		c = istr.get();
		count += 2;
		if(((c == '"') || (c == '\'')) && (buf[0] == '('))
		{
			// We probably have a valid raw string. determine
			// the size, and read it.
			// *FIX: This is memory inefficient.
			S32 len = strtol(buf + 1, NULL, 0);
			if((max_bytes>0)&&(len>max_bytes)) return LLSDParser::PARSE_FAILURE;
			std::vector<char> buf;
			if(len)
			{
				buf.resize(len);
				count += fullread(istr, (char *)&buf[0], len);
				value.assign(buf.begin(), buf.end());
			}
			c = istr.get();
			++count;
			if(!((c == '"') || (c == '\'')))
			{
				return LLSDParser::PARSE_FAILURE;
			}
		}
		else
		{
			return LLSDParser::PARSE_FAILURE;
		}
		return count;
	}

	static const char* NOTATION_STRING_CHARACTERS[256] =
	{
		"\\x00",	// 0
		"\\x01",	// 1
		"\\x02",	// 2
		"\\x03",	// 3
		"\\x04",	// 4
		"\\x05",	// 5
		"\\x06",	// 6
		"\\a",		// 7
		"\\b",		// 8
		"\\t",		// 9
		"\\n",		// 10
		"\\v",		// 11
		"\\f",		// 12
		"\\r",		// 13
		"\\x0e",	// 14
		"\\x0f",	// 15
		"\\x10",	// 16
		"\\x11",	// 17
		"\\x12",	// 18
		"\\x13",	// 19
		"\\x14",	// 20
		"\\x15",	// 21
		"\\x16",	// 22
		"\\x17",	// 23
		"\\x18",	// 24
		"\\x19",	// 25
		"\\x1a",	// 26
		"\\x1b",	// 27
		"\\x1c",	// 28
		"\\x1d",	// 29
		"\\x1e",	// 30
		"\\x1f",	// 31
		" ",		// 32
		"!",		// 33
		"\"",		// 34
		"#",		// 35
		"$",		// 36
		"%",		// 37
		"&",		// 38
		"\\'",		// 39
		"(",		// 40
		")",		// 41
		"*",		// 42
		"+",		// 43
		",",		// 44
		"-",		// 45
		".",		// 46
		"/",		// 47
		"0",		// 48
		"1",		// 49
		"2",		// 50
		"3",		// 51
		"4",		// 52
		"5",		// 53
		"6",		// 54
		"7",		// 55
		"8",		// 56
		"9",		// 57
		":",		// 58
		";",		// 59
		"<",		// 60
		"=",		// 61
		">",		// 62
		"?",		// 63
		"@",		// 64
		"A",		// 65
		"B",		// 66
		"C",		// 67
		"D",		// 68
		"E",		// 69
		"F",		// 70
		"G",		// 71
		"H",		// 72
		"I",		// 73
		"J",		// 74
		"K",		// 75
		"L",		// 76
		"M",		// 77
		"N",		// 78
		"O",		// 79
		"P",		// 80
		"Q",		// 81
		"R",		// 82
		"S",		// 83
		"T",		// 84
		"U",		// 85
		"V",		// 86
		"W",		// 87
		"X",		// 88
		"Y",		// 89
		"Z",		// 90
		"[",		// 91
		"\\\\",		// 92
		"]",		// 93
		"^",		// 94
		"_",		// 95
		"`",		// 96
		"a",		// 97
		"b",		// 98
		"c",		// 99
		"d",		// 100
		"e",		// 101
		"f",		// 102
		"g",		// 103
		"h",		// 104
		"i",		// 105
		"j",		// 106
		"k",		// 107
		"l",		// 108
		"m",		// 109
		"n",		// 110
		"o",		// 111
		"p",		// 112
		"q",		// 113
		"r",		// 114
		"s",		// 115
		"t",		// 116
		"u",		// 117
		"v",		// 118
		"w",		// 119
		"x",		// 120
		"y",		// 121
		"z",		// 122
		"{",		// 123
		"|",		// 124
		"}",		// 125
		"~",		// 126
		"\\x7f",	// 127
		"\\x80",	// 128
		"\\x81",	// 129
		"\\x82",	// 130
		"\\x83",	// 131
		"\\x84",	// 132
		"\\x85",	// 133
		"\\x86",	// 134
		"\\x87",	// 135
		"\\x88",	// 136
		"\\x89",	// 137
		"\\x8a",	// 138
		"\\x8b",	// 139
		"\\x8c",	// 140
		"\\x8d",	// 141
		"\\x8e",	// 142
		"\\x8f",	// 143
		"\\x90",	// 144
		"\\x91",	// 145
		"\\x92",	// 146
		"\\x93",	// 147
		"\\x94",	// 148
		"\\x95",	// 149
		"\\x96",	// 150
		"\\x97",	// 151
		"\\x98",	// 152
		"\\x99",	// 153
		"\\x9a",	// 154
		"\\x9b",	// 155
		"\\x9c",	// 156
		"\\x9d",	// 157
		"\\x9e",	// 158
		"\\x9f",	// 159
		"\\xa0",	// 160
		"\\xa1",	// 161
		"\\xa2",	// 162
		"\\xa3",	// 163
		"\\xa4",	// 164
		"\\xa5",	// 165
		"\\xa6",	// 166
		"\\xa7",	// 167
		"\\xa8",	// 168
		"\\xa9",	// 169
		"\\xaa",	// 170
		"\\xab",	// 171
		"\\xac",	// 172
		"\\xad",	// 173
		"\\xae",	// 174
		"\\xaf",	// 175
		"\\xb0",	// 176
		"\\xb1",	// 177
		"\\xb2",	// 178
		"\\xb3",	// 179
		"\\xb4",	// 180
		"\\xb5",	// 181
		"\\xb6",	// 182
		"\\xb7",	// 183
		"\\xb8",	// 184
		"\\xb9",	// 185
		"\\xba",	// 186
		"\\xbb",	// 187
		"\\xbc",	// 188
		"\\xbd",	// 189
		"\\xbe",	// 190
		"\\xbf",	// 191
		"\\xc0",	// 192
		"\\xc1",	// 193
		"\\xc2",	// 194
		"\\xc3",	// 195
		"\\xc4",	// 196
		"\\xc5",	// 197
		"\\xc6",	// 198
		"\\xc7",	// 199
		"\\xc8",	// 200
		"\\xc9",	// 201
		"\\xca",	// 202
		"\\xcb",	// 203
		"\\xcc",	// 204
		"\\xcd",	// 205
		"\\xce",	// 206
		"\\xcf",	// 207
		"\\xd0",	// 208
		"\\xd1",	// 209
		"\\xd2",	// 210
		"\\xd3",	// 211
		"\\xd4",	// 212
		"\\xd5",	// 213
		"\\xd6",	// 214
		"\\xd7",	// 215
		"\\xd8",	// 216
		"\\xd9",	// 217
		"\\xda",	// 218
		"\\xdb",	// 219
		"\\xdc",	// 220
		"\\xdd",	// 221
		"\\xde",	// 222
		"\\xdf",	// 223
		"\\xe0",	// 224
		"\\xe1",	// 225
		"\\xe2",	// 226
		"\\xe3",	// 227
		"\\xe4",	// 228
		"\\xe5",	// 229
		"\\xe6",	// 230
		"\\xe7",	// 231
		"\\xe8",	// 232
		"\\xe9",	// 233
		"\\xea",	// 234
		"\\xeb",	// 235
		"\\xec",	// 236
		"\\xed",	// 237
		"\\xee",	// 238
		"\\xef",	// 239
		"\\xf0",	// 240
		"\\xf1",	// 241
		"\\xf2",	// 242
		"\\xf3",	// 243
		"\\xf4",	// 244
		"\\xf5",	// 245
		"\\xf6",	// 246
		"\\xf7",	// 247
		"\\xf8",	// 248
		"\\xf9",	// 249
		"\\xfa",	// 250
		"\\xfb",	// 251
		"\\xfc",	// 252
		"\\xfd",	// 253
		"\\xfe",	// 254
		"\\xff"		// 255
	};

	void serialize_string(const std::string& value, std::ostream& str)
	{
		std::string::const_iterator it = value.begin();
		std::string::const_iterator end = value.end();
		U8 c;
		for(; it != end; ++it)
		{
			c = (U8)(*it);
			str << NOTATION_STRING_CHARACTERS[c];
		}
	}

	int deserialize_boolean(
							std::istream& istr,
							LLSD& data,
							const std::string& compare,
							bool value)
	{
		//
		// this method is a little goofy, because it gets the stream at
		// the point where the t or f has already been
		// consumed. Basically, parse for a patch to the string passed in
		// starting at index 1. If it's a match:
		//  * assign data to value
		//  * return the number of bytes read
		// otherwise:
		//  * set data to LLSD::null
		//  * return LLSDParser::PARSE_FAILURE (-1)
		//
		int bytes_read = 0;
		std::string::size_type ii = 0;
		char c = istr.peek();
		while((++ii < compare.size())
			  && (tolower(c) == (int)compare[ii])
			  && istr.good())
		{
			istr.ignore();
			++bytes_read;
			c = istr.peek();
		}
		if(compare.size() != ii)
		{
			data.clear();
			return LLSDParser::PARSE_FAILURE;
		}
		data = value;
		return bytes_read;
	}



	/** 
	 * @class LLSDNotationParser
	 * @brief Parser which handles the original notation format for LLSD.
	 */
	class LLSDNotationParser : public LLSDParser
	{
	public:
		LLSDNotationParser();
		virtual ~LLSDNotationParser();
		
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
		 * @param data[out] The newly parse structured data. Undefined on failure.
		 * @return Returns the number of LLSD objects parsed into
		 * data. Returns PARSE_FAILURE (-1) on parse failure.
		 */
		virtual bool doParse(std::istream& istr, LLSD& data);
		
	private:
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
		 * @param data[out] The data to assign.
		 * @return Retuns true if a complete string was parsed.
		 */
		bool parseString(std::istream& istr, LLSD& data) const;
		
		/** 
		 * @brief Parse binary data from the stream.
		 *
		 * @param istr The input stream.
		 * @param data[out] The data to assign.
		 * @return Retuns true if a complete blob was parsed.
		 */
		bool parseBinary(std::istream& istr, LLSD& data) const;
	};


	/** 
	 * @class LLSDNotationFormatter
	 * @brief Formatter which outputs the original notation format for LLSD.
	 */
	class LLSDNotationFormatter
	{
	public:
		LLSDNotationFormatter();
		virtual ~LLSDNotationFormatter();
		
		/** 
		 * @brief Helper static method to return a notation escaped string
		 *
		 * This method will return the notation escaped string, but not
		 * the surrounding serialization identifiers such as a double or
		 * single quote. It will be up to the caller to embed those as
		 * appropriate.
		 * @param in The raw, unescaped string.
		 * @return Returns an escaped string appropriate for serialization.
		 */
		static std::string escapeString(const std::string& in);
		
		/** 
		 * @brief Call this method to format an LLSD to a stream.
		 *
		 * @param data The data to write.
		 * @param ostr The destination stream for the data.
		 * @return Returns The number of LLSD objects fomatted out
		 */
		S32 format(const LLSD& data, std::ostream& ostr) const;
	};



	/**
	 * LLSDNotationParser
	 */
	LLSDNotationParser::LLSDNotationParser()
	{
	}	

	// virtual
	LLSDNotationParser::~LLSDNotationParser()
	{ }

	// virtual
	bool LLSDNotationParser::doParse(std::istream& istr, LLSD& data)
	{
		// map: { string:object, string:object }
		// array: [ object, object, object ]
		// undef: !
		// boolean: true | false | 1 | 0 | T | F | t | f | TRUE | FALSE
		// integer: i####
		// real: r####
		// uuid: u####
		// string: "g'day" | 'have a "nice" day' | s(size)"raw data"
		// uri: l"escaped"
		// date: d"YYYY-MM-DDTHH:MM:SS.FFZ"
		// binary: b##"ff3120ab1" | b(size)"raw data"

		data.clear();

		char c;
		c = istr.peek();
		while(isspace(c))
		{
			// pop the whitespace.
			c = get(istr);
			c = istr.peek();
			continue;
		}
		if(!istr.good())
		{
			return noteFailure("input failure");
		}
		switch(c)
		{
			case '{':
			{
				if(!parseMap(istr, data) || data.isUndefined())
				{
					return noteFailure("invalid map");
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
					return noteFailure("invalid array");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading array");
				}
				break;
			}
				
			case '!':
				c = get(istr);
				data.clear();
				break;
				
			case '0':
				c = get(istr);
				data = false;
				break;
				
			case 'F':
			case 'f':
				ignore(istr);
				c = istr.peek();
				if(isalpha(c))
				{
					int cnt = deserialize_boolean(
												  istr,
												  data,
												  NOTATION_FALSE_SERIAL,
												  false);
					if(PARSE_FAILURE == cnt) return noteFailure("invalid boolean false");
					else account(cnt);
				}
				else
				{
					data = false;
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading boolean false");
				}
				break;
				
			case '1':
				c = get(istr);
				data = true;
				break;
				
			case 'T':
			case 't':
				ignore(istr);
				c = istr.peek();
				if(isalpha(c))
				{
					int cnt = deserialize_boolean(istr,data,NOTATION_TRUE_SERIAL,true);
					if(PARSE_FAILURE == cnt) return noteFailure("invalid boolean true");
					else account(cnt);
				}
				else
				{
					data = true;
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading boolean true");
				}
				break;
				
			case 'i':
			{
				c = get(istr);
				S32 integer = 0;
				istr >> integer;
				data = integer;
				if(istr.fail())
				{
					return noteFailure("input failure reading integer");
				}
				break;
			}
				
			case 'r':
			{
				c = get(istr);
				F64 real = 0.0;
				istr >> real;
				data = real;
				if(istr.fail())
				{
					return noteFailure("input failure reading real");
				}
				break;
			}
				
			case 'u':
			{
				c = get(istr);
				char buffer[36];
				istr.read(buffer, sizeof(buffer));
				data = LLSD::UUID(std::string(buffer, sizeof(buffer)));
				if(istr.fail())
				{
					return noteFailure("input failure reading uuid");
				}
				break;
			}
				
			case '\"':
			case '\'':
			case 's':
				if(!parseString(istr, data))
				{
					return noteFailure("invalid string");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading string");
				}
				break;
				
			case 'l':
			{
				c = get(istr); // pop the 'l'
				c = get(istr); // pop the delimiter
				std::string str;
				int cnt = deserialize_string_delim(istr, str, c);
				if(PARSE_FAILURE == cnt)
				{
					return noteFailure("invalid uri");
				}
				else
				{
					data = LLSD::URI(str);
					account(cnt);
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading uri");
				}
				break;
			}
				
			case 'd':
			{
				c = get(istr); // pop the 'd'
				c = get(istr); // pop the delimiter
				std::string str;
				int cnt = deserialize_string_delim(istr, str, c);
				if(PARSE_FAILURE == cnt)
				{
					return noteFailure("invalid date");
				}
				else
				{
					data = LLSD::Date(str);
					account(cnt);
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading date");
				}
				break;
			}
				
			case 'b':
				if(!parseBinary(istr, data))
				{
					return noteFailure("invalid binary");
				}
				if(istr.fail())
				{
					return noteFailure("input failure reading binary");
				}
				break;
				
			default:
			{
				std::ostringstream msg;
				msg << "unrecognized character '" << c << "'";
				return noteFailure(msg.str());
			}
		}
		return noteSuccess();
	}

	bool LLSDNotationParser::parseMap(std::istream& istr, LLSD& map)
	{
		// map: { string:object, string:object }
		map = LLSD::emptyMap();
		char c = get(istr);
		if(c == '{')
		{
			// eat commas, white
			bool found_name = false;
			std::string name;
			c = get(istr);
			while(c != '}' && istr.good())
			{
				if(!found_name)
				{
					if((c == '\"') || (c == '\'') || (c == 's'))
					{
						putback(istr, c);
						found_name = true;
						int count = deserialize_string(istr, name, mMaxBytesLeft);
						if(PARSE_FAILURE == count) return noteFailure("invalid map key");
						account(count);
					}
					c = get(istr);
				}
				else
				{
					if(isspace(c) || (c == ':'))
					{
						c = get(istr);
						continue;
					}
					putback(istr, c);
					LLSD child;
					if(doParse(istr, child))
					{
						// There must be a value for every key, thus
						// child_count must be greater than 0.
						map.insert(name, child);
					}
					else
					{
						return noteFailure("invalid map value");
					}
					found_name = false;
					c = get(istr);
				}
			}
			if(c != '}')
			{
				map.clear();
				return noteFailure("incorrectly sized map, missing terminator");
			}
		}
		return noteSuccess();
	}

	bool LLSDNotationParser::parseArray(std::istream& istr, LLSD& array)
	{
		// array: [ object, object, object ]
		array = LLSD::emptyArray();
		char c = get(istr);
		if(c == '[')
		{
			// eat commas, white
			c = get(istr);
			while((c != ']') && istr.good())
			{
				LLSD child;
				if(isspace(c) || (c == ','))
				{
					c = get(istr);
					continue;
				}
				putback(istr, c);
				if(!doParse(istr, child))
				{
					return noteFailure("invalid array value");
				}
				else
				{
					array.append(child);
				}
				c = get(istr);
			}
			if(c != ']')
			{
				return noteFailure("incorrectly sized array, missing terminator");
			}
		}
		return noteSuccess();
	}

	bool LLSDNotationParser::parseString(std::istream& istr, LLSD& data) const
	{
		std::string value;
		int count = deserialize_string(istr, value, mMaxBytesLeft);
		if(PARSE_FAILURE == count) return false;
		account(count);
		data = value;
		return true;
	}

	bool LLSDNotationParser::parseBinary(std::istream& istr, LLSD& data) const
	{
		// binary: b##"ff3120ab1"
		// or: b(len)"..."
		
		// I want to manually control those values here to make sure the
		// parser doesn't break when someone changes a constant somewhere
		// else.
		const U32 BINARY_BUFFER_SIZE = 256;
		const U32 STREAM_GET_COUNT = 255;
		
		// need to read the base out.
		char buf[BINARY_BUFFER_SIZE];		/* Flawfinder: ignore */
		get(istr, buf, STREAM_GET_COUNT, '"');
		char c = get(istr);
		if(c != '"') return false;
		if(0 == strncmp("b(", buf, 2))
		{
			// We probably have a valid raw binary stream. determine
			// the size, and read it.
			S32 len = strtol(buf + 2, NULL, 0);
			if(mCheckLimits && (len > mMaxBytesLeft)) return false;
			std::vector<U8> value;
			if(len)
			{
				value.resize(len);
				account(fullread(istr, (char *)&value[0], len));
			}
			c = get(istr); // strip off the trailing double-quote
			data = value;
		}
		else if(0 == strncmp("b64", buf, 3))
		{
			// *FIX: A bit inefficient, but works for now. To make the
			// format better, I would need to add a hint into the
			// serialization format that indicated how long it was.
			std::stringstream coded_stream;
			get(istr, *(coded_stream.rdbuf()), '\"');
			c = get(istr);
			std::string encoded(coded_stream.str());
			S32 len = apr_base64_decode_len(encoded.c_str());
			std::vector<U8> value;
			if(len)
			{
				value.resize(len);
				len = apr_base64_decode_binary(&value[0], encoded.c_str());
				value.resize(len);
			}
			data = value;
		}
		else if(0 == strncmp("b16", buf, 3))
		{
			// yay, base 16. We pop the next character which is either a
			// double quote or base 16 data. If it's a double quote, we're
			// done parsing. If it's not, put the data back, and read the
			// stream until the next double quote.
			char* read;	 /*Flawfinder: ignore*/
			U8 byte;
			U8 byte_buffer[BINARY_BUFFER_SIZE];
			U8* write;
			std::vector<U8> value;
			c = get(istr);
			while(c != '"')
			{
				putback(istr, c);
				read = buf;
				write = byte_buffer;
				get(istr, buf, STREAM_GET_COUNT, '"');
				c = get(istr);
				while(*read != '\0')	 /*Flawfinder: ignore*/
				{
					byte = hexvalue(*read++);
					byte = byte << 4;
					byte |= hexvalue(*read++);
					*write++ = byte;
				}
				// copy the data out of the byte buffer
				value.insert(value.end(), byte_buffer, write);
			}
			data = value;
		}
		else
		{
			return false;
		}
		return true;
	}



	/**
	 * LLSDNotationFormatter
	 */
	LLSDNotationFormatter::LLSDNotationFormatter()
	{
	}

	// virtual
	LLSDNotationFormatter::~LLSDNotationFormatter()
	{ }

	// static
	std::string LLSDNotationFormatter::escapeString(const std::string& in)
	{
		std::ostringstream ostr;
		serialize_string(in, ostr);
		return ostr.str();
	}

	// virtual
	S32 LLSDNotationFormatter::format(const LLSD& data, std::ostream& ostr) const
	{
		S32 format_count = 1;
		switch(data.type())
		{
			case LLSD::TypeMap:
			{
				ostr << "{";
				bool need_comma = false;
				LLSD::map_const_iterator iter = data.beginMap();
				LLSD::map_const_iterator end = data.endMap();
				for(; iter != end; ++iter)
				{
					if(need_comma) ostr << ",";
					need_comma = true;
					ostr << '\'';
					serialize_string((*iter).first, ostr);
					ostr << "':";
					format_count += format((*iter).second, ostr);
				}
				ostr << "}";
				break;
			}
				
			case LLSD::TypeArray:
			{
				ostr << "[";
				bool need_comma = false;
				LLSD::array_const_iterator iter = data.beginArray();
				LLSD::array_const_iterator end = data.endArray();
				for(; iter != end; ++iter)
				{
					if(need_comma) ostr << ",";
					need_comma = true;
					format_count += format(*iter, ostr);
				}
				ostr << "]";
				break;
			}
				
			case LLSD::TypeUndefined:
				ostr << "!";
				break;
				
			case LLSD::TypeBoolean:
				ostr << (data.asBoolean()
						 ? NOTATION_TRUE_SERIAL : NOTATION_FALSE_SERIAL);
				break;
				
			case LLSD::TypeInteger:
				ostr << "i" << data.asInteger();
				break;
				
			case LLSD::TypeReal:
				ostr << "r" << data.asReal();
				break;
				
			case LLSD::TypeUUID:
				ostr << "u" << data.asUUID();
				break;
				
			case LLSD::TypeString:
				ostr << '\'';
				serialize_string(data.asString(), ostr);
				ostr << '\'';
				break;
				
			case LLSD::TypeDate:
				ostr << "d\"" << data.asDate() << "\"";
				break;
				
			case LLSD::TypeURI:
				ostr << "l\"";
				serialize_string(data.asString(), ostr);
				ostr << "\"";
				break;
				
			case LLSD::TypeBinary:
			{
				// *FIX: memory inefficient.
				std::vector<U8> buffer = data.asBinary();
				ostr << "b(" << buffer.size() << ")\"";
				if(buffer.size()) ostr.write((const char*)&buffer[0], buffer.size());
				ostr << "\"";
				break;
			}
				
			default:
				// *NOTE: This should never happen.
				ostr << "!";
				break;
		}
		return format_count;
	}

}


using namespace LLSDSerialize;

void LLSDNotationTraits::format(const LLSD& sd, std::ostream& str)
{
	LLSDNotationFormatter().format(sd, str);
}

bool LLSDNotationTraits::parse(LLSD& sd, std::istream& str, S32 max_bytes, std::string* errorMessage)
{
	LLSDNotationParser parser;
	return parser.parse(str, sd, max_bytes, errorMessage);
}


#endif // LL_LEGACY
