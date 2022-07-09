<?php
/**
* @file index.php
* @brief Runs the LLSD unit test suite.
*
* $LicenseInfo:firstyear=2007&license=mit$
* 
* Copyright (c) 2007-2010, Linden Research, Inc.
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

if (! defined('SIMPLE_TEST')) {
    define('SIMPLE_TEST', 'simpletest/');
}

if (! defined('TEST_DIR')) {
    define('TEST_DIR', 'test/');
}

if (! defined('LLSD_DIR')) {
    define('LLSD_DIR', 'llsd/');
}


require_once(SIMPLE_TEST . 'unit_tester.php');
require_once(SIMPLE_TEST . 'reporter.php');

require_once(LLSD_DIR . 'llsd_classes.php');
require_once(LLSD_DIR . 'llsd_encode.php');
require_once(LLSD_DIR . 'llsd_decode.php');

require_once(TEST_DIR . 'LLSDTest.php');


?>
