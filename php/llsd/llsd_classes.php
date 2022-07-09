<?php
/**
* @file llsd_classes.php
* @brief Classes used to encode LLSD objects in PHP native objects.
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

class llsd_UUID
{
    static $_NULL_UUID = '00000000-0000-0000-0000-000000000000';
	var $Value;

	function llsd_UUID()
	{
		$this->Value = self::$_NULL_UUID;
	}

	function CheckUUID($UUID)
	{
        $uuidlen = strlen($UUID);
        if (32 === $uuidlen)
        {
            return ctype_xdigit($UUID);
        }
        elseif (36 === $uuidlen)
        {
            // We support UUIDs in 8-4-4-4-12 form
            return ctype_xdigit(substr($UUID, 0, 8)) &&
                   ($UUID[8] == '-') &&
                   ctype_xdigit(substr($UUID, 9, 4)) &&
                   ($UUID[13] == '-') &&
                   ctype_xdigit(substr($UUID, 14, 4)) &&
                   ($UUID[18] == '-') &&
                   ctype_xdigit(substr($UUID, 19, 4)) &&
                   ($UUID[23] == '-') &&
                   ctype_xdigit(substr($UUID, 24, 12));
        }
        return False;
	}

	function Set($UUID)
	{
        if ($UUID == '')
        {
            $this->Value = self::$_NULL_UUID;
            return true;
        }

		if ($this->CheckUUID($UUID))
		{
			$this->Value = $UUID;
			return true;
		}
		else
		{
			unset($this->Value);
			throw new ImproperInvocationException('Invalid UUID string passed to class');
		}
	}

	function Get()
	{
		if (isset($this->Value))
		{
			// Return actual UUID
			return $this->Value;
		}
		else
		{
			// Null UUID - Mimics Python's UUID class
			return self::$_NULL_UUID;
		}
	}

    function __toString()
    {
        return $this->Get();
    }

};

class llsd_URI
{
	var $Value;

	function llsd_URI()
	{
		$this->Value = '';
	}

	function Set($URI)
	{
		$this->Value = $URI;
		return true;
	}

	function Get()
	{
		return $this->Value;
	}
};

class llsd_Date
{
	var $Value;

	function llsd_Date()
	{
		$this->Value = '';
	}

	function Set($Date)
	{
		$this->Value = $Date;
		return true;
	}

	function Get()
	{
		return $this->Value;
	}
};

class llsd_Undef
{
	function __toString() 
	{
		return undef;
	}
};

class llsd_Binary
{
	var $Value;
	var $Encoding;

	function Set($EncodedData = '', $Encoding = 'base64')
	{
		switch ($Encoding)
		{
			case 'base64':
				$this->Value = base64_decode($EncodedData);

				if ($this->Value === FALSE)
				{
					// Decode failed
					unset($this->Value);
					return false;
				}

				break;

			default:
				break;
		}

		if (isset($this->Value))
		{
			// Decode successful
			$this->Encoding = $Encoding;
			return true;
		}
		else
		{
			// Data invalid
			return false;
		}
	}

	function GetData()
	{
		if (isset($this->Value))
			return $this->Value;
		else
			return false;
	}

	function GetEncoding()
	{
		if (isset($this->Encoding))
			return $this->Encoding;
		else
			return false;
	}

    public function __toString() {
        return $this->GetData();
    }
};

?>
