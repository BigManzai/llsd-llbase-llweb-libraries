/** 
 * @file test.cpp
 * @brief Entry point for the test app.
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

/** 
 *
 * You can add tests by creating a new cpp file in this directory, and
 * rebuilding. There are at most 50 tests per testgroup without a
 * little bit of template parameter and makefile tweaking.
 *
 */

#include <fstream>

#include "apr_pools.h"
#include "apr_getopt.h"

#include "lltut.h"


namespace tut
{
    test_runner_singleton runner;
}

class LLTestCallback : public tut::callback
{
public:
	LLTestCallback(bool verbose_mode, std::ostream *stream) :
		mVerboseMode(verbose_mode),
		mTotalTests(0),
		mPassedTests(0),
		mFailedTests(0),
		mSkippedTests(0),
		mStream(stream)
	{
	}

	void run_started()
	{
		//std::cout << "run_started" << std::endl;
	}

	void test_completed(const tut::test_result& tr)
	{
		++mTotalTests;
		std::ostringstream out;
		out << "[" << tr.group << ", " << tr.test << "] ";
		switch(tr.result)
		{
		case tut::test_result::ok:
			++mPassedTests;
			out << "ok";
			break;
		case tut::test_result::fail:
			++mFailedTests;
			out << "fail";
			break;
		case tut::test_result::ex:
			++mFailedTests;
			out << "exception";
			break;
		case tut::test_result::warn:
			++mFailedTests;
			out << "test destructor throw";
			break;
		case tut::test_result::term:
			++mFailedTests;
			out << "abnormal termination";
			break;
		case tut::test_result::skip:
			++mSkippedTests;
			out << "skipped known failure";
			break;
		default:
			++mFailedTests;
			out << "unknown";
		}
		if(mVerboseMode || (tr.result != tut::test_result::ok))
		{
			if(!tr.message.empty())
			{
				out << ": '" << tr.message << "'";
			}
			if (mStream)
			{
				*mStream << out.str() << std::endl;
			}
			
			std::cout << out.str() << std::endl;
		}
	}

	void run_completed()
	{
		if (mStream)
		{
			run_completed_(*mStream);
		}
		run_completed_(std::cout);
	}

	int getFailedTests() const { return mFailedTests; }
	
private:
	void run_completed_(std::ostream &stream)
	{
		stream << "\tTotal Tests:\t" << mTotalTests << std::endl;
		stream << "\tPassed Tests:\t" << mPassedTests;
		if (mPassedTests == mTotalTests)
		{
			stream << "\tYAY!! \\o/";
		}
		stream << std::endl;

		if (mSkippedTests > 0)
		{
			stream << "\tSkipped known failures:\t" << mSkippedTests
				<< std::endl;
		}

		if(mFailedTests > 0)
		{
			stream << "*********************************" << std::endl;
			stream << "Failed Tests:\t" << mFailedTests << std::endl;
			stream << "Please report or fix the problem." << std::endl;
			stream << "*********************************" << std::endl;
		}
	}

protected:
	bool mVerboseMode;
	int mTotalTests;
	int mPassedTests;
	int mFailedTests;
	int mSkippedTests;
	std::ostream *mStream;
};

static const apr_getopt_option_t TEST_CL_OPTIONS[] =
{
	{"help", 'h', 0, "Print the help message."},
	{"list", 'l', 0, "List available test groups."},
	{"verbose", 'v', 0, "Verbose output."},
	{"group", 'g', 1, "Run test group specified by option argument."},
	{"output", 'o', 1, "Write output to the named file."},
	{"touch", 't', 1, "Touch the given file if all tests succeed"},
	{"wait", 'w', 0, "Wait for input before exit."},
	{0, 0, 0, 0}
};

void stream_usage(std::ostream& s, const char* app)
{
	s << "Usage: " << app << " [OPTIONS]" << std::endl
	  << std::endl;

	s << "This application runs the unit tests." << std::endl << std::endl;

	s << "Options: " << std::endl;
	const apr_getopt_option_t* option = &TEST_CL_OPTIONS[0];
	while(option->name)
	{
		s << "  ";
		s << "  -" << (char)option->optch << ", --" << option->name
		  << std::endl;
		s << "\t" << option->description << std::endl << std::endl;
		++option;
	}

	s << "Examples:" << std::endl;
	s << "  " << app << " --verbose" << std::endl;
	s << "\tRun all the tests and report all results." << std::endl;
	s << "  " << app << " --list" << std::endl;
	s << "\tList all available test groups." << std::endl;
	s << "  " << app << " --group=uuid" << std::endl;
	s << "\tRun the test group 'uuid'." << std::endl;
}

void stream_groups(std::ostream& s, const char* app)
{
	s << "Registered test groups:" << std::endl;
	tut::groupnames gl = tut::runner.get().list_groups();
	tut::groupnames::const_iterator it = gl.begin();
	tut::groupnames::const_iterator end = gl.end();
	for(; it != end; ++it)
	{
		s << "  " << *(it) << std::endl;
	}
}

void wouldHaveCrashed(const std::string& message)
{
	tut::fail("llerrs message: " + message);
}

int main(int argc, char **argv)
{
	apr_initialize();
	apr_pool_t* pool = NULL;
	if(APR_SUCCESS != apr_pool_create(&pool, NULL))
	{
		std::cerr << "Unable to initialize pool" << std::endl;
		return 1;
	}
	apr_getopt_t* os = NULL;
	if(APR_SUCCESS != apr_getopt_init(&os, pool, argc, argv))
	{
		std::cerr << "Unable to  pool" << std::endl;
		return 1;
	}

	// values used for controlling application
	bool verbose_mode = false;
	bool wait_at_exit = false;
	std::string test_group;

	// values use for options parsing
	apr_status_t apr_err;
	const char* opt_arg = NULL;
	int opt_id = 0;
	std::ofstream *output = NULL;
	const char *touch = NULL;
	
	while(true)
	{
		apr_err = apr_getopt_long(os, TEST_CL_OPTIONS, &opt_id, &opt_arg);
		if(APR_STATUS_IS_EOF(apr_err)) break;
		if(apr_err)
		{
			char buf[255];		/* Flawfinder: ignore */
			std::cerr << "Error parsing options: "
					  << apr_strerror(apr_err, buf, 255) << std::endl;
			return 1;
		}
		switch (opt_id)
		{
		case 'g':
			test_group.assign(opt_arg);
			break;
		case 'h':
			stream_usage(std::cout, argv[0]);
			return 0;
			break;
		case 'l':
			stream_groups(std::cout, argv[0]);
			return 0;
		case 'v':
			verbose_mode = true;
			break;
		case 'o':
			output = new std::ofstream;
			output->open(opt_arg);
			break;
		case 't':
			touch = opt_arg;
			break;
		case 'w':
			wait_at_exit = true;
			break;
		default:
			stream_usage(std::cerr, argv[0]);
			return 1;
			break;
		}
	}

	// run the tests
	LLTestCallback callback(verbose_mode, output);
	tut::runner.get().set_callback(&callback);
	
	if(test_group.empty())
	{
		tut::runner.get().run_tests();
	}
	else
	{
		tut::runner.get().run_tests(test_group);
	}

	bool success = (callback.getFailedTests() == 0);

	if (wait_at_exit)
	{
		std::cerr << "Press return to exit..." << std::endl;
		std::cin.get();
	}
	
	if (output)
	{
		output->close();
		delete output;
	}

	if (touch && success)
	{
		std::ofstream s;
		s.open(touch);
		s << "ok" << std::endl;
		s.close();
	}
	
	apr_terminate();
	
	int retval = (success ? 0 : 1);
	return retval;
}
