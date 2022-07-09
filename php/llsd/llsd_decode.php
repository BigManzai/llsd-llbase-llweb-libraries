<?php
/**
* @file llsd_decode.php
* @brief Parse XML serialized LLSD.
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

class LLSDParser
{
	var $parser;

	var $result;
	var $inLLSDElement;

	var $stack;
	var $keyStack;
	/*  These parallel stacks represent the state of nested structures being
		built up.  The deepest structure being built is at the end.  When each
		data value is completely parsed, it is stored in the array at the end
		of $stack.  If the end of the $keyStack is a string, then that is the
		key to store it under if it is False, then we are building an array and
		the new value is placed at the end.  If the stacks are empty when we
		finish parsing a value, then it is the top level value and it is the
		$result.  */

	var $depth;
	var $skipping;
	var $skipThrough;

	var $currentContent;
	var $currentEncoding;


	// *TODO: This is really only parsing "XML".
	//        It does not validate that the XML is actually
	//        valid LLSD.  We /may/ want to fix this someday.
	//        Or if it's a performance hit, we may want to at 
	//        least check for valid LLSD in development mode,
	//        but not in production.
	//
	function LLSDParser()
	{
		$this->parser = xml_parser_create();
		$this->result = null;
		$this->inLLSDElement = false;

		$this->stack 	= array();
		$this->keyStack	= array();

		$this->depth = 0;
		$this->skipping = false;
		$this->skipThrough;

		$this->currentContent = '';

		xml_parser_set_option($this->parser, XML_OPTION_CASE_FOLDING, False);
		xml_set_object($this->parser, $this);
		xml_set_element_handler($this->parser, 'tag_open', 'tag_close');
		xml_set_character_data_handler($this->parser, 'cdata');
	}
	
	function GetLLSDObject()
	{
		return $this->result;
	}
	
	function parse($data)
	{
		$result = xml_parse($this->parser, $data);

		if( $result == 0 )
		{
			$errno  = xml_get_error_code( $this->parser );
			$errstr = xml_error_string( $errno );

			$line   = xml_get_current_line_number( $this->parser );
			$col    = xml_get_current_column_number( $this->parser );

			$msg = "$errstr (line $line, col $col)";

			throw new Exception( $msg, $errno );
		}
	}

	function startSkipping()
	{
		$this->skipping = true;
		$this->skipThrough = $this->depth;
	}
	
	function tag_open($parser, $tag, $attributes)
	{
		$this->depth += 1;
		if ($this->skipping)
			return;

		$this->currentContent = '';

		switch ($tag)
		{
			case 'llsd':
				if ($this->inLLSDElement)
					return $this->startSkipping();
				$this->inLLSDElement = true;
				return;

			case 'key':
				if (empty($this->keyStack) or end($this->keyStack) === false)
					return $this->startSkipping();
				return;
		}

		if (!$this->inLLSDElement)
			return $this->startSkipping();

		switch ($tag)
		{
			case 'binary':
				$this->currentEncoding = $attributes['encoding'];
				break;

			case 'map':
				$this->stack[] = array();
				$this->keyStack[] = true;
				break;

			case 'array':
				$this->stack[] = array();
				$this->keyStack[] = false;
				break;
		}
	}
	
	function tag_close($parser, $tag)
	{
		$this->depth -= 1;
		if ($this->skipping)
		{
			if ($this->depth < $this->skipThrough)
			{
				$this->skipping = false;
			}
			return;
		}
		switch ($tag)
		{
			case 'llsd':
				$this->inLLSDElement = false;
				return;
			
			case 'key':
				array_pop($this->keyStack);
				$this->keyStack[] = $this->currentContent;
				return;
		}
		if (!$this->inLLSDElement) return;

		$content = $this->currentContent;
		$value = null;
		switch ($tag)
		{
			case 'undef':
				$value = null;
				break;

			case 'boolean':
				$value = $content == 'true'  ||  $content == '1';
				break;

			case 'integer':
				$value = (int)$content;
				break;

			case 'real':
				$value = (float)$content;
				break;

			case 'string':
				$value = (string)$content;
				break;

			case 'uuid':
				$value = new llsd_UUID;
				$value->Set($content);
				break;

			case 'date':
				$value = new llsd_Date;
				$value->Set($content);
				break;
			
			case 'uri':
				$value = new llsd_URI;
				$value->Set($content);
				break;

			case 'binary':
				$value = new llsd_Binary;
				$value->Set($content, $this->currentEncoding);
				break;

			case 'array':
			case 'map':
				$value = array_pop($this->stack);
				array_pop($this->keyStack);
				break;

			default:
				$value = null;
				break;
		}
		if (empty($this->stack))
		{
			$this->result = $value;
		}
		else
		{
			$n = count($this->stack) - 1;
			$struct = &$this->stack[$n];
			$key = $this->keyStack[$n];
			if ($key === false)
			{
				$struct[] = $value;
			}
			else
			{
				$struct[$key] = $value;
			}
		}
	}
	
	function cdata($parser, $cdata)
	{
		if ($this->skipping)
			return;
		$this->currentContent .= $cdata;
	}
}
	
function llsd_decode($str)
{
	$LLSDParser = new LLSDParser();
	$LLSDParser->parse($str);
	return $LLSDParser->GetLLSDObject();
}
	
?>
