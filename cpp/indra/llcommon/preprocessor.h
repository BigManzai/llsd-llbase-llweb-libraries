/** 
 * @file preprocessor.h
 * @brief This file should be included in all Linden Lab files and
 * should only contain special preprocessor directives
 *
 * $LicenseInfo:firstyear=2001&license=mit$
 * 
 * Copyright (c) 2001-2010, Linden Research, Inc.
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

#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H



// Figure out differences between compilers
#if defined(__GNUC__)
	#define GCC_VERSION (__GNUC__ * 10000 \
						+ __GNUC_MINOR__ * 100 \
						+ __GNUC_PATCHLEVEL__)
	#ifndef LL_GNUC
		#define LL_GNUC 1
	#endif
#elif defined(__MSVC_VER__) || defined(_MSC_VER)
	#ifndef LL_MSVC
		#define LL_MSVC 1
	#endif
	#if _MSC_VER < 1400
		#define LL_MSVC7 //Visual C++ 2003 or earlier
	#endif
#endif


// Static linking with apr on windows needs to be declared.
#if LL_WINDOWS && !LL_COMMON_LINK_SHARED
#ifndef APR_DECLARE_STATIC
#define APR_DECLARE_STATIC // For APR on Windows
#endif
#ifndef APU_DECLARE_STATIC
#define APU_DECLARE_STATIC // For APR util on Windows
#endif
#endif

#if defined(LL_WINDOWS)
#ifndef XML_STATIC
#define XML_STATIC
#endif
#endif	//	LL_WINDOWS


// Deal with VC6 problems
#if LL_MSVC
#pragma warning( 3	     : 4701 )	// "local variable used without being initialized"  Treat this as level 3, not level 4.
#pragma warning( 3	     : 4702 )	// "unreachable code"  Treat this as level 3, not level 4.
#pragma warning( 3	     : 4189 )	// "local variable initialized but not referenced"  Treat this as level 3, not level 4.
//#pragma warning( 3	: 4018 )	// "signed/unsigned mismatch"  Treat this as level 3, not level 4.
#pragma warning( 3      :  4263 )	// 'function' : member function does not override any base class virtual member function
#pragma warning( 3      :  4264 )	// "'virtual_function' : no override available for virtual member function from base 'class'; function is hidden"
#pragma warning( 3       : 4265 )	// "class has virtual functions, but destructor is not virtual"
#pragma warning( 3      :  4266 )	// 'function' : no override available for virtual member function from base 'type'; function is hidden
#pragma warning (disable : 4180)	// qualifier applied to function type has no meaning; ignored
#pragma warning( disable : 4284 )	// silly MS warning deep inside their <map> include file
#pragma warning( disable : 4503 )	// 'decorated name length exceeded, name was truncated'. Does not seem to affect compilation.
#pragma warning( disable : 4800 )	// 'BOOL' : forcing value to bool 'true' or 'false' (performance warning)
#pragma warning( disable : 4996 )	// warning: deprecated

// Linker optimization with "extern template" generates these warnings
#pragma warning( disable : 4231 )	// nonstandard extension used : 'extern' before template explicit instantiation
#pragma warning( disable : 4506 )   // no definition for inline function

// level 4 warnings that we need to disable:
#pragma warning (disable : 4100) // unreferenced formal parameter
#pragma warning (disable : 4127) // conditional expression is constant (e.g. while(1) )
#pragma warning (disable : 4244) // possible loss of data on conversions
#pragma warning (disable : 4396) // the inline specifier cannot be used when a friend declaration refers to a specialization of a function template
#pragma warning (disable : 4512) // assignment operator could not be generated
#pragma warning (disable : 4706) // assignment within conditional (even if((x = y)) )

#pragma warning (disable : 4251) // member needs to have dll-interface to be used by clients of class
#pragma warning (disable : 4275) // non dll-interface class used as base for dll-interface class
#endif	//	LL_MSVC


#endif	//	PREPROCESSOR_H
