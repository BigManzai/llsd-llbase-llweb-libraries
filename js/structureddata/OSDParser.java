/**
 * A library to interact with Virtual Worlds such as OpenSim
 * Copyright (C) 2012  Jitendra Chauhan, Email: jitendra.chauhan@gmail.com
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package com.ngt.jopenmetaverse.shared.structureddata;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import com.ngt.jopenmetaverse.shared.structureddata.llsd.BinaryLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.JsonLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.XmlLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.util.Utils;


public class OSDParser
{
    static final String LLSD_BINARY_HEADER = "<? llsd/binary ?>";
    static final String LLSD_XML_HEADER = "<llsd>";
    static final String LLSD_XML_ALT_HEADER = "<?xml";
    static final String LLSD_XML_ALT2_HEADER = "<? llsd/xml ?>";

    public static OSD Deserialize(byte[] data) throws IOException, OSDException
    {
        String header = Utils.bytesToString(data, 0, data.length >= 17 ? 17 : data.length, "US-ASCII");

        if (header.startsWith(LLSD_BINARY_HEADER))
        {
            return BinaryLLSDOSDParser.DeserializeLLSDBinary(data);
        }
        else if (header.startsWith(LLSD_XML_HEADER) ||
            header.startsWith(LLSD_XML_ALT_HEADER) ||
            header.startsWith(LLSD_XML_ALT2_HEADER))
        {
            return XmlLLSDOSDParser.DeserializeLLSDXml(data);
        }
        else
        {
            return JsonLLSDOSDParser.DeserializeLLSDJson(data);
        }
    }

    public static OSD Deserialize(String data) throws UnsupportedEncodingException, IOException, OSDException
    {
        if (data.startsWith(LLSD_BINARY_HEADER, 0))
        {
            return BinaryLLSDOSDParser.DeserializeLLSDBinary(data.getBytes("UTF8"));
        }
        else if (data.startsWith(LLSD_XML_HEADER, 0) ||
            data.startsWith(LLSD_XML_ALT_HEADER, 0) ||
            data.startsWith(LLSD_XML_ALT2_HEADER, 0))
        {
            return XmlLLSDOSDParser.DeserializeLLSDXml(data);
        }
        else
        {
            return JsonLLSDOSDParser.DeserializeLLSDJson(data);
        }
    }

    public static OSD Deserialize(InputStream stream) throws OSDException, IOException
    {
        if (stream.markSupported())
        {
            byte[] headerData = new byte[14];
            stream.mark(15);
            stream.read(headerData, 0, 14);
            stream.reset();
            String header = new String(headerData, "US-ASCII");
//            String header = Encoding.ASCII.GetString(headerData);

            if (header.startsWith(LLSD_BINARY_HEADER))
                return BinaryLLSDOSDParser.DeserializeLLSDBinary(stream);
            else if (header.startsWith(LLSD_XML_HEADER) || header.startsWith(LLSD_XML_ALT_HEADER) || header.startsWith(LLSD_XML_ALT2_HEADER))
            {
                return XmlLLSDOSDParser.DeserializeLLSDXml(stream);
            }
            else
            {
                return JsonLLSDOSDParser.DeserializeLLSDJson(stream);
            }
        }
        else
        {
            throw new OSDException("Cannot deserialize structured data from unseekable streams");
        }
    }
}