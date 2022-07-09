<?php
/**
* @file llsd_endcode.php
* @brief Serialize LLSD to XML.
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

// VERY IMPORTANT
// Read the  optimization notes at the end of this file before editing.

class LLSD_Encoder
{
	function encode(&$node)
	{
		ob_start();
		echo '<llsd>';
		$this->encode_node($node);
		echo '</llsd>';
		return ob_get_clean();
	}

	function encode_node(&$node)
	{
		switch (gettype($node))
		{
			case 'array':	// if (is_array($node))
				if ($this->detect_map($node))
				{
					echo '<map>';
					foreach ($node as $key => &$value)
					{
						echo '<key>',
							htmlspecialchars($key, ENT_NOQUOTES),
							'</key>';
						$this->encode_node($value);
					}
					echo '</map>';
				}
				else
				{
					echo '<array>';
					foreach ($node as &$value)
					{
						$this->encode_node($value);
					}
					echo '</array>';
				}
				break;
			
			case 'integer':	// else if (is_int($node))
				echo '<integer>',
						htmlspecialchars($node, ENT_NOQUOTES),
						'</integer>';
				break;

			case 'double': // else if (is_float($node))
				echo '<real>',
						htmlspecialchars($node, ENT_NOQUOTES),
						'</real>';
				break;
			
			case 'boolean': // else if (is_bool($node))
				if ($node)	echo '<boolean>true</boolean>';
				else		echo '<boolean>false</boolean>';
				break;
			
			case 'object': // else if (is_object($node))
				switch (get_class($node))
				{
					case "llsd_UUID":
						echo '<uuid>',
							htmlspecialchars($node->Get(), ENT_NOQUOTES),
							'</uuid>';
						break;
						
					case "llsd_URI":
						echo '<uri>',
							htmlspecialchars($node->Get(), ENT_NOQUOTES),
							'</uri>';
						break;
						
					case "llsd_Date":
						echo '<date>',
							htmlspecialchars($node->Get(), ENT_NOQUOTES),
							'</date>';
						break;
						
					case "llsd_Binary":
						echo '<binary encoding="',
							htmlspecialchars($node->GetEncoding(), ENT_QUOTES),
							'">';
						$this->encode_binary($node);
						echo '</binary>';
						break;
						
					default:
						echo '<string>',
							htmlspecialchars($node, ENT_NOQUOTES),
							 '</string>';
						break;
				}
				break;
			
			case 'NULL': // else if ($node === null)
				echo '<undef/>';
				break;
			
			default: //else
				echo '<string>',
						htmlspecialchars($node, ENT_NOQUOTES),
						'</string>';
		}
	}


	function detect_map(&$node)
	{
		// This routine accounts for only about 10% of the time
		$index = 0;
		foreach ($node as $key => &$value)
		{
			if ($key !== $index) return true;
			++$index;
		}
		return false;
	}


	function encode_string(&$node)
	{
		# NB: This function has been in-lined into encode_node()
		echo htmlspecialchars($node, ENT_NOQUOTES);
			# NB: DO NOT add a charset argument ('UTF-8')
			# In that case, PHP only supports 16-bit Unicode chars.
			# Which is horribly, horribly broken.
	}
			
	function encode_attribute(&$node)
	{
		# NB: This function has been in-lined into encode_node()
		echo htmlspecialchars($node, ENT_QUOTES);
			# NB: DO NOT add a charset argument ('UTF-8')
			# In that case, PHP only supports 16-bit Unicode chars.
			# Which is horribly, horribly broken.
	}

	function encode_binary(&$node)
	{
		$encoding = $node->GetEncoding();

		if ($encoding == "base64")
		{
			echo base64_encode($node->Value);
		}
		else
		{
			echo '';
		}
	}

}

function llsd_encode(&$node)
{
	$encoder = new LLSD_Encoder();
	return $encoder->encode($node);
}

/* OPTIMIZATION

This file has been heavily optimized as it has been shown to be one of the
significant bottlenecks in the system. Due to the vagaries of PHP, many of
these optimizations are counter to good coding practice. Do not undo these
changes without careful profiling and understanding. All of these methods
were confirmed by careful timing tests. Many are listed among common PHP
optimizations by other programmers on the Internet.

1) Generating the string.
Three output methods were compared, and are shown with relative timing:
	concatenating strings       1.00 (baseline)
	writing to php://temp       1.14
	output buffering            0.90
	
Savings: 10%.


2) Using switch(gettype($node))
The PHP manual cautions on using gettype() in that they don't guarantee that
the returned strings won't change in future versions of PHP. The unit tests
will catch if this changes in a way that breaks this code.

The recommend way of using a cascade of if tests on is_array(), is_int(), etc.
is retained in the comments here for reference, but should not be used.

Savings: 3.5%


3) Inlining calls
Function call overhead, even to compiled in library functions, is very
expensive in PHP. Calls to encode_string and encode_attribute were inlined
into encode_node. The functions were left in, as the comments therein are
extremely important for future programmers.

Savings: 10%

4) Combining echo statements
Echo in PHP is not a function, and can take a number of values in a single
statement. Though the code would be clearer with multiple echo statements in
a many places, they have been combined into one statement. Note that it is not
worth returning a string from a called function just so that it can be combined
into a single echo statement of the caller. In those cases, it is best to just
echo from within the called function.

Savings: 6%

5) Using foreach
While the PHP manual states that foreach($a as $k => $v) is the same as
reset($k); while (($k, $v) = each($a)), the foreach version is faster.

Savings: 24%

6) Using & in foreach
Coding foreach with a reference on the value yields an improvement.

Savings: 6%
-----------
Total Savings after all optimizations: 46%, or 1.8x faster!

*/

?>
