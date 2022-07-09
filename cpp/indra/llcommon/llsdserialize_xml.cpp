/** 
 * @file llsdserialize_xml.cpp
 * @brief XML parsers and formatters for LLSD
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

#include <deque>
#include <iostream>
#include <sstream>
#include <stdlib.h>	// for strtol and strtod

extern "C"
{
#include <expat.h>
}


namespace 
{
	class XMLEscapeString
	{
	public:
		XMLEscapeString(const std::string& s) : mString(s) { }
		void escapeOnto(std::ostream& out) const;
	private:
		const std::string& mString;
	};
		
	void XMLEscapeString::escapeOnto(std::ostream& out) const
	{
		std::string::size_type i = 0;
		while (true)
		{
			std::string::size_type j = mString.find_first_of("<>&\'\"", i);
			if (j == std::string::npos)
			{
				out << mString.substr(i);
				return;
			}
			out << mString.substr(i, j - i);
			switch (mString[j])
			{
				case '<':
					out << "&lt;";
					break;
				case '>':
					out << "&gt;";
					break;
				case '&':
					out << "&amp;";
					break;
				case '\'':
					out << "&apos;";
					break;
				case '"':
					out << "&quot;";
					break;
				default:
					// should never happen
					out << mString[j];
					break;
			}
			i = j + 1;
		}
	}
	
	std::ostream& operator<<(std::ostream& out, const XMLEscapeString& esc)
	{
		esc.escapeOnto(out);
		return out;
	}
	
	void XMLFormatOne(const LLSD& data, std::ostream& ostr, bool pretty, U32 level)
	{
		std::string pre;
		std::string post;

		if (pretty)
		{
			for (U32 i = 0; i < level; i++)
			{
				pre += "    ";
			}
			post = "\n";
		}

		switch(data.type())
		{
		case LLSD::TypeMap:
			if(0 == data.size())
			{
				ostr << pre << "<map />" << post;
			}
			else
			{
				ostr << pre << "<map>" << post;
				LLSD::map_const_iterator iter = data.beginMap();
				LLSD::map_const_iterator end = data.endMap();
				for(; iter != end; ++iter)
				{
					ostr << pre << "<key>" << XMLEscapeString((*iter).first) << "</key>" << post;
					XMLFormatOne((*iter).second, ostr, pretty, level + 1);
				}
				ostr << pre <<  "</map>" << post;
			}
			break;

		case LLSD::TypeArray:
			if(0 == data.size())
			{
				ostr << pre << "<array />" << post;
			}
			else
			{
				ostr << pre << "<array>" << post;
				LLSD::array_const_iterator iter = data.beginArray();
				LLSD::array_const_iterator end = data.endArray();
				for(; iter != end; ++iter)
				{
					XMLFormatOne(*iter, ostr, pretty, level + 1);
				}
				ostr << pre << "</array>" << post;
			}
			break;

		case LLSD::TypeUndefined:
			ostr << pre << "<undef />" << post;
			break;

		case LLSD::TypeBoolean:
			ostr << pre << "<boolean>"
				 << (data.asBoolean() ? "true" : "false")
				 << "</boolean>" << post;
			break;

		case LLSD::TypeInteger:
			ostr << pre << "<integer>" << data.asInteger() << "</integer>" << post;
			break;

		case LLSD::TypeReal:
			ostr << pre << "<real>" << data.asReal() << "</real>" << post;
			break;

		case LLSD::TypeUUID:
			if(data.asUUID().isNull()) ostr << pre << "<uuid />" << post;
			else ostr << pre << "<uuid>" << data.asUUID() << "</uuid>" << post;
			break;

		case LLSD::TypeString:
			if(data.asString().empty()) ostr << pre << "<string />" << post;
			else ostr << pre << "<string>" << XMLEscapeString(data.asString()) <<"</string>" << post;
			break;

		case LLSD::TypeDate:
			ostr << pre << "<date>" << data.asDate() << "</date>" << post;
			break;

		case LLSD::TypeURI:
			ostr << pre << "<uri>" << XMLEscapeString(data.asString()) << "</uri>" << post;
			break;

		case LLSD::TypeBinary:
		{
			LLSD::Binary buffer = data.asBinary();
			if(buffer.empty())
			{
				ostr << pre << "<binary />" << post;
			}
			else
			{
				ostr << pre << "<binary encoding=\"base64\">";
				// *FIX: memory inefficient.
				ostr << data.asString();
				ostr << "</binary>" << post;
			}
			break;
		}
		default:
			// *NOTE: This should never happen.
			ostr << pre << "<undef />" << post;
			break;
		}
	}

	void XMLFormat(const LLSD& data, std::ostream& ostr, bool pretty)
	{
		std::streamsize old_precision = ostr.precision(25);
		
		std::string post;
		if (pretty)
		{
			post = "\n";
		}
		ostr << "<llsd>" << post;
		XMLFormatOne(data, ostr, pretty, 1);
		ostr << "</llsd>\n";
		
		ostr.precision(old_precision);
	}
}

void LLSDXMLTraits::format(const LLSD& sd, std::ostream& str)
{
	XMLFormat(sd, str, false);
}

void LLSDPrettyXMLTraits::format(const LLSD& sd, std::ostream& str)
{
	XMLFormat(sd, str, true);
}





namespace
{
	class XMLParser
	{
	public:
		XMLParser();
		~XMLParser();
		
		bool parse(LLSD& data, std::istream& input);
		void reset();

		std::string errorMessage() const;
		
#ifdef LL_LEGACY
		void setLegacyMode() { mLegacyMode = true; };
		void clear_input(std::istream& input) const;
		bool isLegacyHeader(const std::string& line) const;
#else
		void clear_input(std::istream&) const { }
#endif
		
	private:
		void startElementHandler(const XML_Char* name, const XML_Char** attributes);
		void endElementHandler(const XML_Char* name);
		void characterDataHandler(const XML_Char* data, int length);
		
		static void sStartElementHandler(
			void* userData, const XML_Char* name, const XML_Char** attributes);
		static void sEndElementHandler(
			void* userData, const XML_Char* name);
		static void sCharacterDataHandler(
			void* userData, const XML_Char* data, int length);

		void startSkipping(const char* msg, const char* arg = NULL);
		
		void noteError(const char* msg, const char* arg = NULL);
		
		enum Element {
			ELEMENT_LLSD,
			ELEMENT_UNDEF,
			ELEMENT_BOOL,
			ELEMENT_INTEGER,
			ELEMENT_REAL,
			ELEMENT_STRING,
			ELEMENT_UUID,
			ELEMENT_DATE,
			ELEMENT_URI,
			ELEMENT_BINARY,
			ELEMENT_MAP,
			ELEMENT_ARRAY,
			ELEMENT_KEY,
			ELEMENT_UNKNOWN
		};
		static Element readElement(const XML_Char* name);
		
		static const XML_Char* findAttribute(const XML_Char* name, const XML_Char** pairs);
		

		XML_Parser	mParser;

		LLSD mResult;
		
		bool mInLLSDElement;			// true if we're on LLSD
		bool mLLSDParsed;				// true if we found the </llsd
		
		typedef std::deque<LLSD*> LLSDRefStack;
		LLSDRefStack mStack;
		
		int mDepth;
		bool mSkipping;
		int mSkipThrough;
		
		std::string mCurrentKey;		// Current XML <tag>
		std::string mCurrentContent;	// String data between <tag> and </tag>
	
		std::string mErrorMessage;
		bool mErrorNoted;
		
#ifdef LL_LEGACY
		bool mLegacyMode;
#endif
	};


	XMLParser::XMLParser()
	{
		mParser = XML_ParserCreate(NULL);
		reset();
	}

	XMLParser::~XMLParser()
	{
		XML_ParserFree(mParser);
	}

#ifdef LL_LEGACY
	inline bool is_eol(char c)
	{
		return (c == '\n' || c == '\r');
	}

	void XMLParser::clear_input(std::istream& input) const
	{
		// TODO: remove? not clear anyone relied on this strange behavior
		if (!mLegacyMode)
			return;

		char c = input.peek();
		while (input.good() && is_eol(c))
		{
			input.get(c);
			c = input.peek();
		}
	}
	
	bool XMLParser::isLegacyHeader(const std::string& header) const
	{
		// This code is mimics the old header testing code exactly.
		// It does not attempt to be a rigorous, or reasonable header test.
		
		std::string::size_type start = std::string::npos;
		std::string::size_type end = std::string::npos;
		start = header.find_first_not_of("<? ");
		if (start != std::string::npos)
		{
			end = header.find_first_of(" ?", start);
		}
		if ((start == std::string::npos) || (end == std::string::npos))
			return false;
		
		std::string pi = header.substr(start, end - start);
		
		return pi == "LLSD/XML";
	}
#endif // LL_LEGACY

	int getfullline(std::istream& input, char* buf, int bufsize)
	/**^ Like istream::getline(), but returns the newline character, if any,
	 *  and doesn't \0 terminate the buffer. The returned size is the number
	 *  characters placed in the buffer, including any trailing newline.
	 */  
	{
		input.getline(buf, bufsize, '\n');
		std::streamsize count = input.gcount();
		
		if (count > 0)
		{
			if (input.good())
			{
				// If getline() was 'good', then a full line was read from
				// the stream, but the final newline wasn't put in the buffer
				// instead it was null terminated. This overwrites that null
				// with the dropped newline.
				buf[count-1] = '\n';
			}
			else
			{
				// If getline() read a partial line, it sets 'fail', which
				// must be cleared before any further reading can take place.
				input.clear(); 				
			}
		}
		
		return count;
	}

	bool XMLParser::parse(LLSD& data, std::istream& input)
	{
		static const int BUFFER_SIZE = 1024;

		XML_Status status;

		clear_input(input);
		
#ifdef LL_LEGACY
		bool first_line = true;
#endif
		
		while(input.good() && !input.eof())
		{
			char* buffer = (char*)XML_GetBuffer(mParser, BUFFER_SIZE);
			if (!buffer)
			{
				status = XML_STATUS_ERROR;
				break;
			}
			
			int count = getfullline(input, buffer, BUFFER_SIZE);
#ifdef LL_LEGACY
			if (first_line && mLegacyMode
			&& isLegacyHeader(std::string(buffer, count)))
			{
				count = getfullline(input, buffer, BUFFER_SIZE);
			}
			first_line = false;
#endif

			status = XML_ParseBuffer(mParser, count, false);
			if (status == XML_STATUS_ERROR)
			{
				break;
			}
		}

		if (status != XML_STATUS_ERROR && !mLLSDParsed)
		{
			status = XML_Parse(mParser, NULL, 0, true);
		}
		
		if (status == XML_STATUS_ERROR)
		{
			XML_Error code = XML_GetErrorCode(mParser);
			if (code != XML_ERROR_ABORTED && code != XML_ERROR_FINISHED)
			{
				noteError("XML parse error", XML_ErrorString(code));
			}
		}
		if (!mLLSDParsed)
		{
			noteError("no llsd element found");
		}

		clear_input(input);

		data = mErrorNoted ? LLSD() : mResult;
		return !mErrorNoted;
	}


	void XMLParser::reset()
	{
		mResult.clear();

		mInLLSDElement = false;
		mDepth = 0;

		mLLSDParsed = false;

		mStack.clear();
		
		mSkipping = false;
		
		mCurrentKey.clear();
		
		mErrorMessage = "";
		mErrorNoted = false;
		
#ifdef LL_LEGACY
		mLegacyMode = false;
#endif

		XML_ParserReset(mParser, "utf-8");
		XML_SetUserData(mParser, this);
		XML_SetElementHandler(mParser, sStartElementHandler, sEndElementHandler);
		XML_SetCharacterDataHandler(mParser, sCharacterDataHandler);
	}


	void XMLParser::startSkipping(const char* msg, const char* arg)
	{
		mSkipping = true;
		mSkipThrough = mDepth;
#ifdef LL_LEGACY
		if (mLegacyMode) return;
#endif
		noteError(msg, arg);
	}

	const XML_Char*
	XMLParser::findAttribute(const XML_Char* name, const XML_Char** pairs)
	{
		while (NULL != pairs && NULL != *pairs)
		{
			if(0 == strcmp(name, *pairs))
			{
				return *(pairs + 1);
			}
			pairs += 2;
		}
		return NULL;
	}

	void XMLParser::startElementHandler(const XML_Char* name, const XML_Char** attributes)
	{
		++mDepth;
		if (mSkipping)
		{
			return;
		}

		Element element = readElement(name);
		
		mCurrentContent.clear();

		
		if (!mInLLSDElement && element != ELEMENT_LLSD)
		{
			return startSkipping("element outside of llsd element", name);
		}
		
		switch (element)
		{
			case ELEMENT_LLSD:
				if (mInLLSDElement)
				{
					return startSkipping("nested llsd element found");
				}
				mInLLSDElement = true;
				return;
		
			case ELEMENT_KEY:
				if (mStack.empty()  ||  !(mStack.back()->isMap()))
				{
					return startSkipping("unexpected key element");
				}
				return;

			case ELEMENT_BINARY:
			{
				const XML_Char* encoding = findAttribute("encoding", attributes);
				if(encoding && strcmp("base64", encoding) != 0)
				{
					return startSkipping("unrecognized binary encoding", encoding);
				}
				break;
			}
			
			default:
				// all rest are values, fall through
				;
		}
		
		
		if (mStack.empty())
		{
			mStack.push_back(&mResult);
		}
		else if (mStack.back()->isMap())
		{
			if (mCurrentKey.empty())
			{
				return startSkipping("missing map key");
			}
			
			LLSD& map = *mStack.back();
			LLSD& newElement = map[mCurrentKey];
			mStack.push_back(&newElement);		

			mCurrentKey.clear();
		}
		else if (mStack.back()->isArray())
		{
			LLSD& array = *mStack.back();
			array.append(LLSD());
			LLSD& newElement = array[array.size()-1];
			mStack.push_back(&newElement);
		}
		else {
			return startSkipping("element nested in simple value", name);
		}

		switch (element)
		{
			case ELEMENT_MAP:
				*mStack.back() = LLSD::emptyMap();
				break;
			
			case ELEMENT_ARRAY:
				*mStack.back() = LLSD::emptyArray();
				break;
			
			case ELEMENT_UNKNOWN:
#ifdef LL_LEGACY
				if (mLegacyMode) break;
#endif
				startSkipping("invalid element", name);
				break;

			default:
				// all the other values will be set in the end element handler
				;
		}
	}

	void XMLParser::endElementHandler(const XML_Char* name)
	{
		--mDepth;
		if (mSkipping)
		{
			if (mDepth < mSkipThrough)
			{
				mSkipping = false;
			}
			return;
		}
		
		Element element = readElement(name);
		
		switch (element)
		{
			case ELEMENT_LLSD:
				if (mInLLSDElement)
				{
					mInLLSDElement = false;
					mLLSDParsed = true;
					XML_StopParser(mParser, false);
				}
				return;
		
			case ELEMENT_KEY:
				mCurrentKey = mCurrentContent;
				return;
				
			default:
				// all rest are values, fall through
				;
		}
		
		if (!mInLLSDElement) { return; }

		LLSD& value = *mStack.back();
		mStack.pop_back();
		
		switch (element)
		{
			case ELEMENT_UNDEF:
				value.clear();
				break;
			
			case ELEMENT_BOOL:
				value = (mCurrentContent == "true"
#ifdef LL_LEGACY
						 || (mLegacyMode && mCurrentContent == "1")
#endif
						 );
				break;
			
			case ELEMENT_INTEGER:
				{
					char* end;
					S32 i = strtol(mCurrentContent.c_str(), &end, 10);
					if ( *end != '\0' )
					{
						i = 0;
					}
					value = i;
				}
				break;
			
			case ELEMENT_REAL:
				{
					char* end;
					F64 r = strtod(mCurrentContent.c_str(), &end);
					if ( *end != '\0' )
					{
						r = 0.0;
					}
					value = r;
				}
				break;
			
			case ELEMENT_STRING:
				value = mCurrentContent;
				break;
			
			case ELEMENT_UUID:
				value = LLSD::UUID(mCurrentContent);
				break;
			
			case ELEMENT_DATE:
				value = LLSD::Date(mCurrentContent);
				break;
			
			case ELEMENT_URI:
				value = LLSD::URI(mCurrentContent);
				break;
			
			case ELEMENT_BINARY:
			{
				value = LLSD(mCurrentContent).asBinary();
				break;
			}
			
			case ELEMENT_UNKNOWN:
				// in legacy mode, this is how unkown elements are handled
				value.clear();
				break;
				
			default:
				// other values, map and array, have already been set
				break;
		}

		mCurrentContent.clear();
	}

	void XMLParser::characterDataHandler(const XML_Char* data, int length)
	{
		mCurrentContent.append(data, length);
	}


	void XMLParser::sStartElementHandler(
		void* userData, const XML_Char* name, const XML_Char** attributes)
	{
		((XMLParser*)userData)->startElementHandler(name, attributes);
	}

	void XMLParser::sEndElementHandler(
		void* userData, const XML_Char* name)
	{
		((XMLParser*)userData)->endElementHandler(name);
	}

	void XMLParser::sCharacterDataHandler(
		void* userData, const XML_Char* data, int length)
	{
		((XMLParser*)userData)->characterDataHandler(data, length);
	}

	void XMLParser::noteError(const char* msg, const char* arg)
	{
		if (mErrorNoted) return;
		mErrorNoted = true;

		if (!msg)
		{
			mErrorMessage = "parse error";
		}
		else if (!arg)
		{
			mErrorMessage = msg;
		}
		else
		{
			std::ostringstream s;
			s << msg << ": \"" << arg << "\"";
			mErrorMessage = s.str();
		}
	}
	
	std::string XMLParser::errorMessage() const
	{
		return mErrorMessage;
	}
	
	/*
		This code is time critical

		This is a sample of tag occurances of text in simstate file with ~8000 objects.
		A tag pair (<key>something</key>) counts is counted as two:

			key     - 2680178
			real    - 1818362
			integer -  906078
			array   -  295682
			map     -  191818
			uuid    -  177903
			binary  -  175748
			string  -   53482
			undef   -   40353
			boolean -   33874
			llsd    -   16332
			uri     -      38
			date    -       1
	*/
	XMLParser::Element XMLParser::readElement(const XML_Char* name)
	{
		XML_Char c = *name;
		switch (c)
		{
			case 'k':
				if (strcmp(name, "key") == 0) { return ELEMENT_KEY; }
				break;
			case 'r':
				if (strcmp(name, "real") == 0) { return ELEMENT_REAL; }
				break;
			case 'i':
				if (strcmp(name, "integer") == 0) { return ELEMENT_INTEGER; }
				break;
			case 'a':
				if (strcmp(name, "array") == 0) { return ELEMENT_ARRAY; }
				break;
			case 'm':
				if (strcmp(name, "map") == 0) { return ELEMENT_MAP; }
				break;
			case 'u':
				if (strcmp(name, "uuid") == 0) { return ELEMENT_UUID; }
				if (strcmp(name, "undef") == 0) { return ELEMENT_UNDEF; }
				if (strcmp(name, "uri") == 0) { return ELEMENT_URI; }
				break;
			case 'b':
				if (strcmp(name, "binary") == 0) { return ELEMENT_BINARY; }
				if (strcmp(name, "boolean") == 0) { return ELEMENT_BOOL; }
				break;
			case 's':
				if (strcmp(name, "string") == 0) { return ELEMENT_STRING; }
				break;
			case 'l':
				if (strcmp(name, "llsd") == 0) { return ELEMENT_LLSD; }
				break;
			case 'd':
				if (strcmp(name, "date") == 0) { return ELEMENT_DATE; }
				break;
		}
		return ELEMENT_UNKNOWN;
	}
}



bool LLSDXMLTraits::parse(LLSD& sd, std::istream& str, S32 max_bytes, std::string* errorMessage)
{
	// FIXME: need a way to use max_bytes
	XMLParser parser;
	if (parser.parse(sd, str)) return true;
	if (errorMessage) *errorMessage = parser.errorMessage();
	return false;
}

#ifdef LL_LEGACY

bool LLSDLegacyXMLTraits::parse(LLSD& sd, std::istream& str, S32 max_bytes, std::string* errorMessage)
{
	XMLParser parser;
	parser.setLegacyMode();
	if (parser.parse(sd, str)) return true;
	if (errorMessage) *errorMessage = parser.errorMessage();
	return false;
}

#endif
