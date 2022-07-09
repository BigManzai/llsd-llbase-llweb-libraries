/**
 * @file llsd.cpp
 * @brief LLSD flexible data system
 *
 * $LicenseInfo:firstyear=2005&license=mit$
 *
 * Copyright (c) 2005-2010, Linden Research, Inc.
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

#include "llsd.h"

#include <cmath>
#include <sstream>

#include "llsdserialize.h"

#include "apr_base64.h"


#ifdef LL_WINDOWS
#include <float.h>
namespace std
{
	int isnan(double f) { return _isnan(f); }
}
#endif

namespace
{
	class ImplMap;
	class ImplArray;
}


class LLSD::Impl
	/**< This class is the abstract base class of the implementation of LLSD
		 It provides the reference counting implementation, and the default
		 implementation of most methods for most data types.  It also serves
		 as a working implementation of the Undefined type.

	*/
{
private:
	U32 mUseCount;

protected:
	Impl();

	enum StaticAllocationMarker { STATIC };
	Impl(StaticAllocationMarker);
		///< This constructor is used for static objects and causes the
		//   suppresses adjusting the debugging counters when they are
		//	 finally initialized.

	virtual ~Impl();

	bool shared() const							{ return mUseCount > 1; }

public:
	static void reset(Impl*& var, Impl* impl);
		///< safely set var to refer to the new impl (possibly shared)

	static       Impl& safe(      Impl*);
	static const Impl& safe(const Impl*);
		///< since a NULL Impl* is used for undefined, this ensures there is
		//	 always an object you call virtual member functions on

	virtual ImplMap& makeMap(Impl*& var);
	virtual ImplArray& makeArray(Impl*& var);
		///< sure var is a modifiable, non-shared map or array

	virtual LLSD::Type type() const				{ return LLSD::TypeUndefined; }

	static  void assignUndefined(LLSD::Impl*& var);
	static  void assign(LLSD::Impl*& var, const LLSD::Impl* other);

	virtual void assign(Impl*& var, LLSD::Boolean);
	virtual void assign(Impl*& var, LLSD::Integer);
	virtual void assign(Impl*& var, LLSD::Real);
	virtual void assign(Impl*& var, const LLSD::String&);
	virtual void assign(Impl*& var, const LLSD::UUID&);
	virtual void assign(Impl*& var, const LLSD::Date&);
	virtual void assign(Impl*& var, const LLSD::URI&);
	virtual void assign(Impl*& var, const LLSD::Binary&);
		///< If the receiver is the right type and unshared, these are simple
		//   data assignments, othewise the default implementation handless
		//   constructing the proper Impl subclass

	virtual Boolean	asBoolean() const			{ return false; }
	virtual Integer	asInteger() const			{ return 0; }
	virtual Real	asReal() const				{ return 0.0; }
	virtual String	asString() const			{ return std::string(); }
	virtual UUID	asUUID() const				{ return UUID(); }
	virtual Date	asDate() const				{ return Date(); }
	virtual URI		asURI() const				{ return URI(); }
	virtual Binary	asBinary() const			{ return std::vector<U8>(); }

	virtual bool has(const String&) const		{ return false; }
	virtual LLSD get(const String&) const		{ return LLSD(); }
	virtual void erase(const String&)			{ }
	virtual const LLSD& ref(const String&) const{ return undef(); }

	virtual int size() const					{ return 0; }
	virtual LLSD get(Integer) const				{ return LLSD(); }
	virtual void erase(Integer)					{ }
	virtual const LLSD& ref(Integer) const		{ return undef(); }

	virtual LLSD::map_const_iterator beginMap() const { return endMap(); }
	virtual LLSD::map_const_iterator endMap() const { static const std::map<String, LLSD> empty; return empty.end(); }
	virtual LLSD::array_const_iterator beginArray() const { return endArray(); }
	virtual LLSD::array_const_iterator endArray() const { static const std::vector<LLSD> empty; return empty.end(); }

	static const LLSD& undef();

	static U32 sAllocationCount;
	static U32 sOutstandingCount;
};

namespace
{
	std::string base64Encode(const std::vector<U8> binary)
	{
		int binlen = binary.size();
		if (binlen == 0)
		{
			return std::string();
		}

		std::stringstream coded_stream;
		int len = apr_base64_encode_len(binlen);
		std::vector<char> encoded;
		if(len)
		{
			encoded.resize(len);
			len = apr_base64_encode_binary(&encoded[0], &binary[0], binlen);
			encoded.resize(len);
		}
		// len includes trailing null
		return std::string(&encoded[0]);
	}

	std::vector<U8> base64Decode(const std::string string)
	{
		// Strip whitespace
		std::ostringstream stripped;
		for (std::string::const_iterator i= string.begin();
			 i != string.end();
			 i++)
		{
			switch (char c = *i)
			{
			case ' ':
			case '\t':
			case '\r':
			case '\n':
				break;

			default:
				stripped << c;
			}
		}

		S32 len = apr_base64_decode_len(stripped.str().c_str());
		std::vector<U8> data;
		data.resize(len);
		len = apr_base64_decode_binary(&data[0], stripped.str().c_str());
		data.resize(len);
		return data;
	}
}

namespace
{
	template<LLSD::Type T, class Data, class DataRef = Data>
	class ImplBase : public LLSD::Impl
		///< This class handles most of the work for a subclass of Impl
		//   for a given simple data type.  Subclasses of this provide the
		//   conversion functions and a constructor.
	{
	protected:
		Data mValue;

		typedef ImplBase Base;

	public:
		ImplBase(DataRef value) : mValue(value) { }

		virtual LLSD::Type type() const { return T; }

		using LLSD::Impl::assign; // Unhiding base class virtuals...
		virtual void assign(LLSD::Impl*& var, DataRef value) {
			if (shared())
			{
				Impl::assign(var, value);
			}
			else
			{
				mValue = value;
			}
		}
	};


	class ImplBoolean
		: public ImplBase<LLSD::TypeBoolean, LLSD::Boolean>
	{
	public:
		ImplBoolean(LLSD::Boolean v) : Base(v) { }

		virtual LLSD::Boolean	asBoolean() const	{ return mValue; }
		virtual LLSD::Integer	asInteger() const	{ return mValue ? 1 : 0; }
		virtual LLSD::Real		asReal() const		{ return mValue ? 1 : 0; }
		virtual LLSD::String	asString() const;
	};

	LLSD::String ImplBoolean::asString() const
		// *NOTE: The reason that false is not converted to "false" is
		// because that would break roundtripping,
		// e.g. LLSD(false).asString().asBoolean().  There are many
		// reasons for wanting LLSD("false").asBoolean() == true, such
		// as "everything else seems to work that way".
		{ return mValue ? "true" : ""; }


	class ImplInteger
		: public ImplBase<LLSD::TypeInteger, LLSD::Integer>
	{
	public:
		ImplInteger(LLSD::Integer v) : Base(v) { }

		virtual LLSD::Boolean	asBoolean() const	{ return mValue != 0; }
		virtual LLSD::Integer	asInteger() const	{ return mValue; }
		virtual LLSD::Real		asReal() const		{ return mValue; }
		virtual LLSD::String	asString() const;
	};

	LLSD::String ImplInteger::asString() const
	{
		std::ostringstream s;
		s << mValue;
		return s.str();
	}


	class ImplReal
		: public ImplBase<LLSD::TypeReal, LLSD::Real>
	{
	public:
		ImplReal(LLSD::Real v) : Base(v) { }

		virtual LLSD::Boolean	asBoolean() const;
		virtual LLSD::Integer	asInteger() const;
		virtual LLSD::Real		asReal() const		{ return mValue; }
		virtual LLSD::String	asString() const;
	};

	LLSD::Boolean ImplReal::asBoolean() const
		{ return !std::isnan(mValue)  &&  mValue != 0.0; }

	LLSD::Integer ImplReal::asInteger() const
		{ return !std::isnan(mValue) ? (LLSD::Integer)mValue : 0; }

	LLSD::String ImplReal::asString() const
	{
		std::ostringstream s;
		s << mValue;
		return s.str();
	}

	class ImplString
		: public ImplBase<LLSD::TypeString, LLSD::String, const LLSD::String&>
	{
	public:
		ImplString(const LLSD::String& v) : Base(v) { }

		virtual LLSD::Boolean	asBoolean() const	{ return !mValue.empty(); }
		virtual LLSD::Integer	asInteger() const;
		virtual LLSD::Real		asReal() const;
		virtual LLSD::String	asString() const	{ return mValue; }
		virtual LLSD::UUID		asUUID() const	{ return LLSD::UUID(mValue); }
		virtual LLSD::Date		asDate() const	{ return LLSD::Date(mValue); }
		virtual LLSD::URI		asURI() const	{ return LLSD::URI(mValue); }
		virtual LLSD::Binary    asBinary() const { return LLSD::Binary(base64Decode(mValue)); }
	};

	LLSD::Integer	ImplString::asInteger() const
	{
		// This must treat "1.23" not as an error, but as a number, which is
		// then truncated down to an integer.  Hence, this code doesn't call
		// std::istringstream::operator>>(int&), which would not consume the
		// ".23" portion.

		return (int)asReal();
	}

	LLSD::Real		ImplString::asReal() const
	{
		F64 v = 0.0;
		std::istringstream i_stream(mValue);
		i_stream >> v;

		// we would probably like to ignore all trailing whitespace as
		// well, but for now, simply eat the next character, and make
		// sure we reached the end of the string.
		return (i_stream.eof() ? v : 0.0);
	}


	class ImplUUID
		: public ImplBase<LLSD::TypeUUID, LLSD::UUID, const LLSD::UUID&>
	{
	public:
		ImplUUID(const LLSD::UUID& v) : Base(v) { }

		virtual LLSD::String	asString() const{ return mValue.asString(); }
		virtual LLSD::UUID		asUUID() const	{ return mValue; }
	};


	class ImplDate
		: public ImplBase<LLSD::TypeDate, LLSD::Date, const LLSD::Date&>
	{
	public:
		ImplDate(const LLSD::Date& v)
			: ImplBase<LLSD::TypeDate, LLSD::Date, const LLSD::Date&>(v)
			{ }

		virtual LLSD::Integer asInteger() const
		{
			return (LLSD::Integer)(mValue.secondsSinceEpoch());
		}
		virtual LLSD::Real asReal() const
		{
			return mValue.secondsSinceEpoch();
		}
		virtual LLSD::String	asString() const{ return mValue.asString(); }
		virtual LLSD::Date		asDate() const	{ return mValue; }
	};


	class ImplURI
		: public ImplBase<LLSD::TypeURI, LLSD::URI, const LLSD::URI&>
	{
	public:
		ImplURI(const LLSD::URI& v) : Base(v) { }

		virtual LLSD::String	asString() const{ return mValue.asString(); }
		virtual LLSD::URI		asURI() const	{ return mValue; }
	};


	class ImplBinary
		: public ImplBase<LLSD::TypeBinary, LLSD::Binary, const LLSD::Binary&>
	{
	public:
		ImplBinary(const LLSD::Binary& v) : Base(v) { }

		virtual LLSD::Binary	asBinary() const{ return mValue; }
		virtual LLSD::String    asString() const { return LLSD::String(base64Encode(mValue)); }
	};


	class ImplMap : public LLSD::Impl
	{
	private:
		typedef std::map<LLSD::String, LLSD>	DataMap;

		DataMap mData;

	protected:
		ImplMap(const DataMap& data) : mData(data) { }

	public:
		ImplMap() { }

		virtual ImplMap& makeMap(LLSD::Impl*&);

		virtual LLSD::Type type() const { return LLSD::TypeMap; }

		virtual LLSD::Boolean asBoolean() const { return !mData.empty(); }

		virtual bool has(const LLSD::String&) const;

		using LLSD::Impl::get; // Unhiding get(LLSD::Integer)
		using LLSD::Impl::erase; // Unhiding erase(LLSD::Integer)
		using LLSD::Impl::ref; // Unhiding ref(LLSD::Integer)
		virtual LLSD get(const LLSD::String&) const;
		void insert(const LLSD::String& k, const LLSD& v);
		virtual void erase(const LLSD::String&);
		              LLSD& ref(const LLSD::String&);
		virtual const LLSD& ref(const LLSD::String&) const;

		virtual int size() const { return mData.size(); }

		LLSD::map_iterator beginMap() { return mData.begin(); }
		LLSD::map_iterator endMap() { return mData.end(); }
		virtual LLSD::map_const_iterator beginMap() const { return mData.begin(); }
		virtual LLSD::map_const_iterator endMap() const { return mData.end(); }
	};

	ImplMap& ImplMap::makeMap(LLSD::Impl*& var)
	{
		if (shared())
		{
			ImplMap* i = new ImplMap(mData);
			Impl::assign(var, i);
			return *i;
		}
		else
		{
			return *this;
		}
	}

	bool ImplMap::has(const LLSD::String& k) const
	{
		DataMap::const_iterator i = mData.find(k);
		return i != mData.end();
	}

	LLSD ImplMap::get(const LLSD::String& k) const
	{
		DataMap::const_iterator i = mData.find(k);
		return (i != mData.end()) ? i->second : LLSD();
	}

	void ImplMap::insert(const LLSD::String& k, const LLSD& v)
	{
		mData.insert(DataMap::value_type(k, v));
	}

	void ImplMap::erase(const LLSD::String& k)
	{
		mData.erase(k);
	}

	LLSD& ImplMap::ref(const LLSD::String& k)
	{
		return mData[k];
	}

	const LLSD& ImplMap::ref(const LLSD::String& k) const
	{
		DataMap::const_iterator i = mData.lower_bound(k);
		if (i == mData.end()  ||  mData.key_comp()(k, i->first))
		{
			return undef();
		}

		return i->second;
	}

	class ImplArray : public LLSD::Impl
	{
	private:
		typedef std::vector<LLSD>	DataVector;

		DataVector mData;

	protected:
		ImplArray(const DataVector& data) : mData(data) { }

	public:
		ImplArray() { }

		virtual ImplArray& makeArray(Impl*&);

		virtual LLSD::Type type() const { return LLSD::TypeArray; }

		virtual LLSD::Boolean asBoolean() const { return !mData.empty(); }

		using LLSD::Impl::get; // Unhiding get(LLSD::String)
		using LLSD::Impl::erase; // Unhiding erase(LLSD::String)
		using LLSD::Impl::ref; // Unhiding ref(LLSD::String)
		virtual int size() const;
		virtual LLSD get(LLSD::Integer) const;
		        void set(LLSD::Integer, const LLSD&);
		        void insert(LLSD::Integer, const LLSD&);
		        void append(const LLSD&);
		virtual void erase(LLSD::Integer);
		              LLSD& ref(LLSD::Integer);
		virtual const LLSD& ref(LLSD::Integer) const;

		LLSD::array_iterator beginArray() { return mData.begin(); }
		LLSD::array_iterator endArray() { return mData.end(); }
		virtual LLSD::array_const_iterator beginArray() const { return mData.begin(); }
		virtual LLSD::array_const_iterator endArray() const { return mData.end(); }
	};

	ImplArray& ImplArray::makeArray(Impl*& var)
	{
		if (shared())
		{
			ImplArray* i = new ImplArray(mData);
			Impl::assign(var, i);
			return *i;
		}
		else
		{
			return *this;
		}
	}

	int ImplArray::size() const		{ return mData.size(); }

	LLSD ImplArray::get(LLSD::Integer i) const
	{
		if (i < 0) { return LLSD(); }
		DataVector::size_type index = i;

		return (index < mData.size()) ? mData[index] : LLSD();
	}

	void ImplArray::set(LLSD::Integer i, const LLSD& v)
	{
		if (i < 0) { return; }
		DataVector::size_type index = i;

		if (index >= mData.size())
		{
			mData.resize(index + 1);
		}

		mData[index] = v;
	}

	void ImplArray::insert(LLSD::Integer i, const LLSD& v)
	{
		if (i < 0) {
			return;
		}
		DataVector::size_type index = i;

		if (index >= mData.size())
		{
			mData.resize(index + 1);
		}

		mData.insert(mData.begin() + index, v);
	}

	void ImplArray::append(const LLSD& v)
	{
		mData.push_back(v);
	}

	void ImplArray::erase(LLSD::Integer i)
	{
		if (i < 0) { return; }
		DataVector::size_type index = i;

		if (index < mData.size())
		{
			mData.erase(mData.begin() + index);
		}
	}

	LLSD& ImplArray::ref(LLSD::Integer i)
	{
		DataVector::size_type index = i >= 0 ? i : 0;

		if (index >= mData.size())
		{
			mData.resize(i + 1);
		}

		return mData[index];
	}

	const LLSD& ImplArray::ref(LLSD::Integer i) const
	{
		if (i < 0) { return undef(); }
		DataVector::size_type index = i;

		if (index >= mData.size())
		{
			return undef();
		}

		return mData[index];
	}
}

LLSD::Impl::Impl()
	: mUseCount(0)
{
	++sAllocationCount;
	++sOutstandingCount;
}

LLSD::Impl::Impl(StaticAllocationMarker)
	: mUseCount(0)
{
}

LLSD::Impl::~Impl()
{
	--sOutstandingCount;
}

void LLSD::Impl::reset(Impl*& var, Impl* impl)
{
	if (impl) ++impl->mUseCount;
	if (var  &&  --var->mUseCount == 0)
	{
		delete var;
	}
	var = impl;
}

LLSD::Impl& LLSD::Impl::safe(Impl* impl)
{
	static Impl theUndefined(STATIC);
	return impl ? *impl : theUndefined;
}

const LLSD::Impl& LLSD::Impl::safe(const Impl* impl)
{
	static Impl theUndefined(STATIC);
	return impl ? *impl : theUndefined;
}

ImplMap& LLSD::Impl::makeMap(Impl*& var)
{
	ImplMap* im = new ImplMap;
	reset(var, im);
	return *im;
}

ImplArray& LLSD::Impl::makeArray(Impl*& var)
{
	ImplArray* ia = new ImplArray;
	reset(var, ia);
	return *ia;
}


void LLSD::Impl::assign(Impl*& var, const Impl* other)
{
	reset(var, const_cast<Impl*>(other));
}

void LLSD::Impl::assignUndefined(Impl*& var)
{
	reset(var, 0);
}

void LLSD::Impl::assign(Impl*& var, LLSD::Boolean v)
{
	reset(var, new ImplBoolean(v));
}

void LLSD::Impl::assign(Impl*& var, LLSD::Integer v)
{
	reset(var, new ImplInteger(v));
}

void LLSD::Impl::assign(Impl*& var, LLSD::Real v)
{
	reset(var, new ImplReal(v));
}

void LLSD::Impl::assign(Impl*& var, const LLSD::String& v)
{
	reset(var, new ImplString(v));
}

void LLSD::Impl::assign(Impl*& var, const LLSD::UUID& v)
{
	reset(var, new ImplUUID(v));
}

void LLSD::Impl::assign(Impl*& var, const LLSD::Date& v)
{
	reset(var, new ImplDate(v));
}

void LLSD::Impl::assign(Impl*& var, const LLSD::URI& v)
{
	reset(var, new ImplURI(v));
}

void LLSD::Impl::assign(Impl*& var, const LLSD::Binary& v)
{
	reset(var, new ImplBinary(v));
}


const LLSD& LLSD::Impl::undef()
{
	static const LLSD immutableUndefined;
	return immutableUndefined;
}

U32 LLSD::Impl::sAllocationCount = 0;
U32 LLSD::Impl::sOutstandingCount = 0;



namespace
{
	inline LLSD::Impl& safe(LLSD::Impl* impl)
		{ return LLSD::Impl::safe(impl); }

	inline const LLSD::Impl& safe(const LLSD::Impl* impl)
		{ return LLSD::Impl::safe(impl); }

	inline ImplMap& makeMap(LLSD::Impl*& var)
		{ return safe(var).makeMap(var); }

	inline ImplArray& makeArray(LLSD::Impl*& var)
		{ return safe(var).makeArray(var); }
}


LLSD::LLSD()							: impl(0)	{ }
LLSD::~LLSD()							{ Impl::reset(impl, 0); }

LLSD::LLSD(const LLSD& other)			: impl(0) { assign(other); }
void LLSD::assign(const LLSD& other)	{ Impl::assign(impl, other.impl); }


void LLSD::clear()						{ Impl::assignUndefined(impl); }

LLSD::Type LLSD::type() const			{ return safe(impl).type(); }

// Scaler Constructors
LLSD::LLSD(Boolean v)					: impl(0) { assign(v); }
LLSD::LLSD(Integer v)					: impl(0) { assign(v); }
LLSD::LLSD(Real v)						: impl(0) { assign(v); }
LLSD::LLSD(const UUID& v)				: impl(0) { assign(v); }
LLSD::LLSD(const String& v)				: impl(0) { assign(v); }
LLSD::LLSD(const Date& v)				: impl(0) { assign(v); }
LLSD::LLSD(const URI& v)				: impl(0) { assign(v); }
LLSD::LLSD(const Binary& v)				: impl(0) { assign(v); }

// Convenience Constructors
LLSD::LLSD(F32 v)						: impl(0) { assign((Real)v); }

// Scalar Assignment
void LLSD::assign(Boolean v)			{ safe(impl).assign(impl, v); }
void LLSD::assign(Integer v)			{ safe(impl).assign(impl, v); }
void LLSD::assign(Real v)				{ safe(impl).assign(impl, v); }
void LLSD::assign(const String& v)		{ safe(impl).assign(impl, v); }
void LLSD::assign(const UUID& v)		{ safe(impl).assign(impl, v); }
void LLSD::assign(const Date& v)		{ safe(impl).assign(impl, v); }
void LLSD::assign(const URI& v)			{ safe(impl).assign(impl, v); }
void LLSD::assign(const Binary& v)		{ safe(impl).assign(impl, v); }

// Scalar Accessors
LLSD::Boolean	LLSD::asBoolean() const	{ return safe(impl).asBoolean(); }
LLSD::Integer	LLSD::asInteger() const	{ return safe(impl).asInteger(); }
LLSD::Real		LLSD::asReal() const	{ return safe(impl).asReal(); }
LLSD::String	LLSD::asString() const	{ return safe(impl).asString(); }
LLSD::UUID		LLSD::asUUID() const	{ return safe(impl).asUUID(); }
LLSD::Date		LLSD::asDate() const	{ return safe(impl).asDate(); }
LLSD::URI		LLSD::asURI() const		{ return safe(impl).asURI(); }
LLSD::Binary	LLSD::asBinary() const	{ return safe(impl).asBinary(); }

// const char * helpers
LLSD::LLSD(const char* v)				: impl(0) { assign(v); }
void LLSD::assign(const char* v)
{
	if(v) assign(std::string(v));
	else assign(std::string());
}


LLSD LLSD::emptyMap()
{
	LLSD v;
	makeMap(v.impl);
	return v;
}

bool LLSD::has(const String& k) const	{ return safe(impl).has(k); }
LLSD LLSD::get(const String& k) const	{ return safe(impl).get(k); }
void LLSD::insert(const String& k, const LLSD& v) {	makeMap(impl).insert(k, v); }

LLSD& LLSD::with(const String& k, const LLSD& v)
										{
											makeMap(impl).insert(k, v);
											return *this;
										}
void LLSD::erase(const String& k)		{ makeMap(impl).erase(k); }

LLSD&		LLSD::operator[](const String& k)
										{ return makeMap(impl).ref(k); }
const LLSD& LLSD::operator[](const String& k) const
										{ return safe(impl).ref(k); }


LLSD LLSD::emptyArray()
{
	LLSD v;
	makeArray(v.impl);
	return v;
}

int LLSD::size() const					{ return safe(impl).size(); }

LLSD LLSD::get(Integer i) const			{ return safe(impl).get(i); }
void LLSD::set(Integer i, const LLSD& v){ makeArray(impl).set(i, v); }
void LLSD::insert(Integer i, const LLSD& v) { makeArray(impl).insert(i, v); }

LLSD& LLSD::with(Integer i, const LLSD& v)
										{
											makeArray(impl).insert(i, v);
											return *this;
										}
void LLSD::append(const LLSD& v)		{ makeArray(impl).append(v); }
void LLSD::erase(Integer i)				{ makeArray(impl).erase(i); }

LLSD&		LLSD::operator[](Integer i)
										{ return makeArray(impl).ref(i); }
const LLSD& LLSD::operator[](Integer i) const
										{ return safe(impl).ref(i); }

U32 LLSD::allocationCount()				{ return Impl::sAllocationCount; }
U32 LLSD::outstandingCount()			{ return Impl::sOutstandingCount; }

const char *LLSD::dump(const LLSD &llsd)
{
	std::ostringstream out;
	out << LLSDToLog(llsd);

	static std::string string_for_dump;
	// static because, for the debugger, we return a char* into the string

	string_for_dump = out.str();
	return string_for_dump.c_str();
}

LLSD::map_iterator			LLSD::beginMap()		{ return makeMap(impl).beginMap(); }
LLSD::map_iterator			LLSD::endMap()			{ return makeMap(impl).endMap(); }
LLSD::map_const_iterator	LLSD::beginMap() const	{ return safe(impl).beginMap(); }
LLSD::map_const_iterator	LLSD::endMap() const	{ return safe(impl).endMap(); }

LLSD::array_iterator		LLSD::beginArray()		{ return makeArray(impl).beginArray(); }
LLSD::array_iterator		LLSD::endArray()		{ return makeArray(impl).endArray(); }
LLSD::array_const_iterator	LLSD::beginArray() const{ return safe(impl).beginArray(); }
LLSD::array_const_iterator	LLSD::endArray() const	{ return safe(impl).endArray(); }
