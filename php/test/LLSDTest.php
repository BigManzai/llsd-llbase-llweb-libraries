<?php
/**
* @file LLSDTest.php
* @brief A small unit test suite for LLSD XML serialization.
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

class NullParser
{
	var $parser;
	
	function NullParser()
	{
		$this->parser = xml_parser_create();

		xml_parser_set_option($this->parser, XML_OPTION_CASE_FOLDING, False);
		xml_set_object($this->parser, $this);
		xml_set_element_handler($this->parser, 'tag_open', 'tag_close');
		xml_set_character_data_handler($this->parser, 'cdata');
	}
	
	function parse($data)
	{
		xml_parse($this->parser, $data);
	}

	function tag_open($parser, $tag, $attributes) { }
	function tag_close($parser, $tag) { }
	function cdata($parser, $cdata) { }
}

function null_decode($str)
{
	$nullParser = new NullParser();
	$nullParser->parse($str);
}

function null_walk(&$node)
{
	if (is_array($node))
	{
		reset($node);
		while (list($key,$value) = each($node))
		{
			null_walk($value);
		}
	}
}

function null_output(&$str)
{
	ob_start();
	$l = strlen($str);
	$fixedStr = str_repeat('x', 20);
	for ($i = 0; $i < $l; $i += 20)
		# echo htmlspecialchars(substr($str, $i, 20), ENT_NOQUOTES);
		  # outrageously slow, and makes test unrealistic
		echo htmlspecialchars($fixedStr, ENT_NOQUOTES);
		  # closer to realistic measurement of the cost of generating output
	ob_get_clean();
}


class LLSDTest extends UnitTestCase
{
	function ensureFormat( $in, $expected )
	{
		$actual = llsd_encode($in);
		$this->assertEqual($expected, $actual);
	}

	function ensureSame( $actual_value, $expected_value )
	{
		$expected_string = print_r($expected_value,1);
		$actual_string = print_r($actual_value,1);

		$this->assertEqual($expected_string, $actual_string);
	}
	
	function ensureParse( $in, $expected_value )
	{
		$actual_value = llsd_decode($in);
		$this->ensureSame($actual_value, $expected_value);
	}

	function ensureRoundTrip ( $in )
	{
		$this->ensureParse(llsd_encode($in), $in);
	}


	function testFormatAtomics()
	{
		$this->ensureFormat(null, "<llsd><undef/></llsd>");
		$this->ensureFormat('', '<llsd><string></string></llsd>');
			// NB: Formatter doesn't currently collapse empty tags
			// $this->ensureFormat('', '<llsd><string/></llsd>');
		$this->ensureFormat('foobar', '<llsd><string>foobar</string></llsd>');
		$this->ensureFormat(3463, '<llsd><integer>3463</integer></llsd>');

		$v = new llsd_UUID;
		$this->ensureFormat($v, 
			'<llsd><uuid>00000000-0000-0000-0000-000000000000</uuid></llsd>');
			// NB: Formatter doesn't currently collapse empty tags
			// $this->ensureFormat($v, '<llsd><uuid/></llsd>');
		$v->Set('c96f9b1e-f589-4100-9774-d98643ce0bed');
		$this->ensureFormat($v,
			'<llsd><uuid>c96f9b1e-f589-4100-9774-d98643ce0bed</uuid></llsd>');

		$v = new llsd_URI;
		$v->Set("https://secondlife.com/login");
		$this->ensureFormat($v,
			'<llsd><uri>https://secondlife.com/login</uri></llsd>');
		
		$v = new llsd_Date;
		$v->Set("2006-04-24T16:11:33Z");
		$this->ensureFormat($v,
			'<llsd><date>2006-04-24T16:11:33Z</date></llsd>');

		// *FIX: test binary
	}

	function testFormatBoolean()
	{
		$this->ensureFormat(true, "<llsd><boolean>true</boolean></llsd>");
		$this->ensureFormat(false, "<llsd><boolean>false</boolean></llsd>");
		// NB: Formatter doesn't support numeric boolean option
		// $this->ensureFormat(true, "<llsd><boolean>1</boolean></llsd>");
		// $this->ensureFormat(false, "<llsd><boolean>0</boolean></llsd>");
	}

	function testFormatReal()
	{
		// NB: Formatter doesn't support numeric format option
		$this->ensureFormat(1.0, '<llsd><real>1</real></llsd>');
		$this->ensureFormat(-34379.0438, '<llsd><real>-34379.0438</real></llsd>');
		$this->ensureFormat(0.0, '<llsd><real>0</real></llsd>');
	}

	function testFormatArray()
	{
		$v = array();
		$this->ensureFormat($v, '<llsd><array></array></llsd>');
			// NB: Formatter doesn't currently collapse empty tags
			// $this->ensureFormat($v, '<llsd><array/></llsd>');

		$v[] = null;
		$this->ensureFormat($v,
			'<llsd><array><undef/></array></llsd>');

		$v[] = 1;
		$this->ensureFormat($v,
			'<llsd><array><undef/><integer>1</integer></array></llsd>');
	}

	function testFormatMap()
	{
		$v = array();
		// NB: PHP can't distinguish between an empty array or empty map
		// $this->ensureFormat($v, '<llsd><map></map></llsd>');
			// NB: Formatter doesn't currently collapse empty tags
			// $this->ensureFormat($v, '<llsd><map/></llsd>');

		$v['foo'] = 'bar';
		$this->ensureFormat($v,
			'<llsd><map><key>foo</key><string>bar</string></map></llsd>');

		$v['baz'] = null;
		$this->ensureFormat($v,
			'<llsd><map><key>foo</key><string>bar</string><key>baz</key><undef/></map></llsd>');
	}


	function testSimples()
	{
		$this->ensureParse("<llsd><undef/></llsd>", null);
		$this->ensureParse("<llsd><boolean>true</boolean></llsd>", true);
		$this->ensureParse("<llsd><boolean>false</boolean></llsd>", false);
		$this->ensureParse("<llsd><boolean>1</boolean></llsd>", true);
		$this->ensureParse("<llsd><boolean>0</boolean></llsd>", false);
		$this->ensureParse("<llsd><integer>-1</integer></llsd>", -1);
		$this->ensureParse("<llsd><integer>42</integer></llsd>", 42);
		$this->ensureParse("<llsd><real>-1234.5</real></llsd>", -1234.5);
		$this->ensureParse("<llsd><real>0.0</real></llsd>", 0.0);
		$this->ensureParse("<llsd><real>3.14159</real></llsd>", 3.14159);
		$this->ensureParse("<llsd><string></string></llsd>", "");
		$this->ensureParse("<llsd><string>foo</string></llsd>", "foo");

		$u = new llsd_UUID;
		$this->ensureParse("<llsd><uuid/></llsd>", $u);
		$u->Set('fc0824ad-e870-46f3-9d12-985d55732336');
		$this->ensureParse(
			"<llsd><uuid>fc0824ad-e870-46f3-9d12-985d55732336</uuid></llsd>", $u);
	}

	function testLongString()
	{
		$v = <<<LONGSTRING
Second Life is a 3-D virtual world entirely built and owned by its residents. 
Since opening to the public in 2003, it has grown explosively and today is 
inhabited by nearly 100,000 people from around the globe.

From the moment you enter the World you'll discover a vast digital continent, 
teeming with people, entertainment, experiences and opportunity. Once you've 
explored a bit, perhaps you'll find a perfect parcel of land to build your 
house or business.

You'll also be surrounded by the Creations of your fellow residents. Because 
residents retain the rights to their digital creations, they can buy, sell 
and trade with other residents.

The Marketplace currently supports millions of US dollars in monthly 
transactions. This commerce is handled with the in-world currency, the Linden 
dollar, which can be converted to US dollars at several thriving online 
currency exchanges.

Welcome to Second Life. We look forward to seeing you in-world!
LONGSTRING
		;
		$this->ensureRoundTrip($v);
	}

	function testUnicode()
	{
		$block_size = 0x002000;
		for ($block = 0x000000; $block <= 0x10ffff; $block += $block_size)
		{
			$s = '';
			for ($c = $block; $c < $block + $block_size; ++$c)
			{
				//Skip "banned" and improper characters
				if ($c <= 0x000001f
					&& $c != 0x000009
					&& $c != 0x00000a)
				{
					// see XML standard, sections 2.2 and 4.1
					continue;
				}
				if (0x00d800 <= $c  &&  $c <= 0x00dfff) { continue; }
				if (0x00fdd0 <= $c  &&  $c <= 0x00fdef) { continue; }
				if (($c & 0x00fffe) == 0x00fffe) { continue; }		
					// see Unicode standard, section 15.8 

				// Encode in UTF8
				if ($c <= 0x00007f)
				{
					$s .= chr($c & 0x7f);
				}
				else if ($c <= 0x0007ff)
				{
					$s .= chr(0xc0 | (($c >> 6) & 0x1f));
					$s .= chr(0x80 | (($c >> 0) & 0x3f));
				}
				else if ($c <= 0x00ffff)
				{
					$s .= chr(0xe0 | (($c >> 12) & 0x0f));
					$s .= chr(0x80 | (($c >>  6) & 0x3f));
					$s .= chr(0x80 | (($c >>  0) & 0x3f));
				}
				else
				{
					$s .= chr(0xf0 | (($c >> 18) & 0x07));
					$s .= chr(0x80 | (($c >> 12) & 0x3f));
					$s .= chr(0x80 | (($c >>  6) & 0x3f));
					$s .= chr(0x80 | (($c >>  0) & 0x3f));
				}
			}

			$this->ensureRoundTrip($s);
		}
	}

	function testBasics()
	{
		$v = array();
		$v["amy"] = 23;
		$v["bob"][0] = "this";
		$v["bob"][1] = "is";
		$v["bob"][2] = "a";
		$v["bob"][3] = 42;
		$v["bob"][4] = True;
		$v["cam"] = null;
		$v["deb"] = 3.14159;

		$this->ensureParse("
			<llsd>
				<map>
					<key>amy</key>
					<integer>23</integer>
					<key>bob</key>
					<array>
						<string>this</string>
						<string>is</string>
						<string>a</string>
						<integer>42</integer>
						<boolean>true</boolean>
					</array>
					<key>cam</key>
					<undef/>
					<key>deb</key>
					<real>3.14159</real>
				</map>
			</llsd>", $v );
	}

	function makeMap( $width, $depth )
	{
		$v = array();

		if ($depth == 0)
		{
			$v['foo'] = 'bar';
			return $v;
		}

		for ( $i = 0; $i < $width; ++$i )
		{
			$v["child $i"] = $this->makeMap( $width, $depth - 1);
		}

		return $v;
	}

	function testStructures()
	{
		$v = array();
		$v["name"] = 'luke';
		$v["age"] = 3;
		$this->ensureRoundTrip($v);
		
		$v = array();
		$v['a']['one'] = true;
		$v['b']['two'] = false;
		$this->ensureRoundTrip($v);
		
		$v = array();
		$v[] = 'ali';
		$v[] = 28;
		$this->ensureRoundTrip($v);

		$v = array();
		$v[0][0] = true;
		$v[1][0] = false;
		$this->ensureRoundTrip($v);

		$v = $this->makeMap(10, 3);	// 10^3 maps! (10^4 exhausts memory)
		$this->ensureRoundTrip($v);
	}
	
	function testTime()
	{
										$t0 = microtime(true);
		$v = $this->makeMap(10, 3);		$t1 = microtime(true);
		null_walk($t1);					$t2 = microtime(true);
		$enc = llsd_encode($v);			$t3 = microtime(true);
		null_output($enc);				$t4 = microtime(true);
		null_decode($enc);				$t5 = microtime(true);
		$dec = llsd_decode($enc);		$t6 = microtime(true);
		
		$this->ensureSame($dec, $v);
		
		$t6 -= $t5; // time to decode LLSD
		$t5 -= $t4; // time to parse the XML, but do nothing
		$t4 -= $t3;	// time to output the XML string
		$t3 -= $t2; // time to encode LLSD
		$t2 -= $t1; // time to walk structures
		$t1 -= $t0; // time to build structures
		
		$alwaysReport = true;
		
		$encodingOK = $t3 < (($t2 + $t4) * 3);
		$encodingMessage = <<<MSG
Encoding took $t3 seconds,
but should take no more than 3x the time to
walk the structure ($t2)
and output an equivalent length string ($t4)
MSG;
		$this->assertTrue($encodingOK, $encodingMessage);
		if ($alwaysReport && $encodingOK)
            $this->dump("Passed", $encodingMessage);

		$decodingOK = $t6 < (($t1 + $t5) * 3);
		$decodingMessage = <<<MSG
Decoding took $t6 seconds,
but should take no more than 3x the time to
build the data ($t1)
and the raw XML parse time ($t5) 
MSG;
		$this->assertTrue($decodingOK, $decodingMessage);
		if ($alwaysReport && $decodingOK)
			$this->dump("Passed", $decodingMessage);
	}
	
	function testMemory()
	{
										$m0 = memory_get_usage();
		$v = $this->makeMap(10, 3);		$m1 = memory_get_usage();
		$enc = llsd_encode($v);			$m2 = memory_get_usage();
		$dec = llsd_decode($enc);		$m3 = memory_get_usage();
		
		$this->ensureSame($dec, $v);
		
		$m3 -= $m2; // memory used to decode LLSD
		$m2 -= $m1; // memory used to encode LLSD
		$m1 -= $m0; // memory used to build structures
		
		$alwaysReport = true;
		
		$l = strlen($enc);
		$encodingOK = $m2 < ($l * 1.25);
		$encodingMessage = <<<MSG
Encoding allocated $m2 bytes,
but should take no more than 1.25x the XML output ($l)
MSG;
		$this->assertTrue($encodingOK, $encodingMessage);
		if ($alwaysReport && $encodingOK)
            $this->dump("Passed", $encodingMessage);
		
		$decodingOK = $m3 < ($m1 * 1.25);
		$decodingMessage = <<<MSG
Decoding allocated $m3 bytes,
but should take no more than 1.25x what the data structures take ($m1)
MSG;
		$this->assertTrue($decodingOK, $decodingMessage);
		if ($alwaysReport && $decodingOK)
			$this->dump("Passed", $decodingMessage);
	}
}

$this_class = basename(__FILE__, '.php');

// XXX: Don't do a 'require_once()' here.  For some reason PHP gets confused and will only 
// execute one test if you do.
require('test_exec.php');


?>
