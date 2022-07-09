# file llsdhttp.py
# package llweb.llsdhttp
# Functions to ease moving llsd over http.
#
# $LicenseInfo:firstyear=2006&license=mit$
#
# Copyright (c) 2006-2009, Linden Research, Inc.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# $/LicenseInfo$
 
"""
This module is a wrapper for llsd through the httpc client suite. This
module imports httpc.ConnectionError, httpc.Retriable, and all derived
exceptions so will not need to import httpc to catch client exceptions.
""" 


import urlparse

import httpc

from llbase import llsd


suite = httpc.HttpSuite(llsd.format_xml, llsd.parse, 'application/llsd+xml')
"""
An HTTP client for application/llsd+xml. see httpc.HttpSuite
"""

delete = suite.delete
"Wrapper for llweb.httpc.HttpSuite.delete for application/llsd+xml."

delete_ = suite.delete_
"Wrapper for llweb.httpc.HttpSuite.delete_ for application/llsd+xml."

get = suite.get
"Wrapper for llweb.httpc.HttpSuite.get for application/llsd+xml."

get_ = suite.get_
"Wrapper for llweb.httpc.HttpSuite.get_ for application/llsd+xml."

head = suite.head
"Wrapper for llweb.httpc.HttpSuite.head for application/llsd+xml."

head_ = suite.head_
"Wrapper for llweb.httpc.HttpSuite.head_ for application/llsd+xml."

options = suite.options
"Wrapper for llweb.httpc.HttpSuite.options for application/llsd+xml."

options_ = suite.options_
"Wrapper for llweb.httpc.HttpSuite.options_ for application/llsd+xml."

post = suite.post
"Wrapper for llweb.httpc.HttpSuite.post for application/llsd+xml."

post_ = suite.post_
"Wrapper for llweb.httpc.HttpSuite.post_ for application/llsd+xml."

put = suite.put
"Wrapper for llweb.httpc.HttpSuite.put for application/llsd+xml."

put_ = suite.put_
"Wrapper for llweb.httpc.HttpSuite.put_ for application/llsd+xml."

request = suite.request
"Wrapper for llweb.httpc.HttpSuite.request for application/llsd+xml."

request_ = suite.request_
"Wrapper for llweb.httpc.HttpSuite.request_ for application/llsd+xml."

# import every httpc error exception into our namespace for convenience
for __temp in httpc.status_to_error_map.itervalues():
    globals()[__temp.__name__] = __temp

##
# @brief Detailed exception class for reporting on http connection problems.
#
# There are lots of subclasses so you can use closely-specified
# exception clauses.
ConnectionError = httpc.ConnectionError

##
# @brief HTTP error which is recoverable
# @see httpc.ConnectionError
Retriable = httpc.Retriable

for __temp in (httpc.ConnectionError,):
    globals()[__temp.__name__] = __temp
