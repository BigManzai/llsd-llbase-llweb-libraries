/** 
 * @file testdata.cpp
 * @brief test data class for LLSD timing tests.
 *
 * $LicenseInfo:firstyear=2009&license=mit$
 * 
 * Copyright (c) 2009, Linden Research, Inc.
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

#include "testdata.h"
#include <iostream>

void LogStats::report()
{
    using namespace std;
    
    cout << "LogStats { in = " << countIn
                    << ", out = " << countOut
                    << ", misc = " << countMisc
                    << ", error = " << countError << endl;
    cout << "    attachmentsHistogram = [";
    bool first = true;
    for (Histogram::const_iterator i = attachmentsHistogram.begin();
         i != attachmentsHistogram.end(); ++i)
    {
        if (!first)
        {
            cout << ", ";
        }
        first = false;
        cout << *i;
    }
    cout << "]" << endl;
}



     LogBase::~LogBase()                { }
LLSD LogBase::toLLSD() const            { return LLSD(); }
void LogBase::fromLLSD(const LLSD&)     { }
void LogBase::tally(LogStats& st) const { ++st.countError; }


class LogItem : public LogBase
{
protected:
	virtual LLSD toLLSD() const;
	virtual void fromLLSD(const LLSD& in);

private:
	LLUUID		mAgent;
	LLDate		mTime;
};

LLSD LogItem::toLLSD() const
{
	LLSD r = LogBase::toLLSD();
	r["agent"] = mAgent;
	r["time"]  = mTime;
	return r;
}

void LogItem::fromLLSD(const LLSD& in)
{
	LogBase::fromLLSD(in);
	mAgent = in["agent"];
	mTime  = in["time"];
}


class LogIn : public LogItem
{
	virtual LLSD toLLSD() const;
	virtual void fromLLSD(const LLSD& in);
	virtual void tally(LogStats&) const;

	bool		mFirstTime;
	LLURI		mReferer;
};

LLSD LogIn::toLLSD() const
{
	LLSD r = LogItem::toLLSD();
	r["event"]	   = "login";
	r["firsttime"] = mFirstTime;
	r["referer"]   = mReferer;
	return r;
}

void LogIn::tally(LogStats& st) const
{
	++st.countIn;
}

void LogIn::fromLLSD(const LLSD& in)
{
	LogItem::fromLLSD(in);
	mFirstTime = in["firsttime"];
	mReferer   = in["referer"];
}


class LogOut : public LogItem
{
	virtual LLSD toLLSD() const;
	virtual void fromLLSD(const LLSD& in);
	virtual void tally(LogStats&) const;

	struct Attachment
	{
		std::string	where;
		LLUUID		what;
	};

	typedef std::vector<Attachment> Attachments;
	Attachments	mAttachments;
};

LLSD LogOut::toLLSD() const
{
	LLSD as;
	for (Attachments::const_iterator i = mAttachments.begin();
			i != mAttachments.end();
			++i)
	{
		LLSD a;
		a["point"] = i->where;
		a["inv"] = i->what;
		as.append(a);
	}

	LLSD r = LogItem::toLLSD();
	r["event"]        = "logout";
	r["attachments"] = as;
	return r;
}

void LogOut::fromLLSD(const LLSD& in)
{
	LogItem::fromLLSD(in);

	LLSD as = in["attachments"];
	for (LLSD::array_const_iterator i = as.beginArray(); i != as.endArray(); ++i)
	{
		Attachment a;
		a.where = (*i)["point"].asString();
		a.what  = (*i)["inv"];
		mAttachments.push_back(a);
	}
}

void LogOut::tally(LogStats& st) const
{
	++st.countOut;
	Attachments::size_type n = mAttachments.size();
	if (n >= st.attachmentsHistogram.size())
	{
		st.attachmentsHistogram.resize(n+1, 0);
	}
	st.attachmentsHistogram[n] += 1;
}


class LogMisc : public LogItem
{
	virtual LLSD toLLSD() const;
	virtual void fromLLSD(const LLSD& in);
	virtual void tally(LogStats&) const;

	S32	mThis;
	F64	mThat;
};

LLSD LogMisc::toLLSD() const
{
	LLSD r = LogItem::toLLSD();
	r["event"] = "misc";
	r["this"] = mThis;
	r["that"] = mThat;
	return r;
}

void LogMisc::fromLLSD(const LLSD& in)
{
	LogItem::fromLLSD(in);
	mThis = in["this"];
	mThat = in["that"];
}

void LogMisc::tally(LogStats& st) const
{
	++st.countMisc;
}




LogBase* LogBase::buildFromLLSD(const LLSD& in)
{
	std::string event = in["event"];

	LogBase* l;

	     if (event == "login")	l = new LogIn();
	else if (event == "logout") l = new LogOut();
	else if (event == "misc")	l = new LogMisc();
	else						l = new LogBase();

	l->fromLLSD(in);
	return l;
}

LLSD LogBase::serailizeToLLSD(const LogBase& log)
{
	return log.toLLSD();
}




