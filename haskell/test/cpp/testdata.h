/** 
 * @file testdata.h
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

#include <vector>
#include "llsd.h"

struct LogStats
{
    LogStats() : countIn(0), countOut(0), countMisc(0), countError(0) { }
    
	int		countIn;
	int		countOut;
	int		countMisc;
	int		countError;

	typedef std::vector<int> Histogram;
	Histogram	attachmentsHistogram;
	
	void report();
};


class LogBase
{
public:
    virtual ~LogBase();
	static LogBase* buildFromLLSD(const LLSD& in);
	static LLSD serailizeToLLSD(const LogBase& log);

	virtual void tally(LogStats&) const;

protected:
	virtual LLSD toLLSD() const;
	virtual void fromLLSD(const LLSD& in);
};


