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
package com.ngt.jopenmetaverse.shared.structureddata.llsd;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Map;

import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDException;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;

    public class BinaryLLSDOSDParser
    {
        private static final int initialBufferSize = 128;
        private static final int int32Length = 4;
        private static final int doubleLength = 8;

        private static final String llsdBinaryHead = "<? llsd/binary ?>";
        private static final String llsdBinaryHead2 = "<?llsd/binary?>";
        private static final byte undefBinaryValue = (byte)'!';
        private static final byte trueBinaryValue = (byte)'1';
        private static final byte falseBinaryValue = (byte)'0';
        private static final byte integerBinaryMarker = (byte)'i';
        private static final byte realBinaryMarker = (byte)'r';
        private static final byte uuidBinaryMarker = (byte)'u';
        private static final byte binaryBinaryMarker = (byte)'b';
        private static final byte StringBinaryMarker = (byte)'s';
        private static final byte uriBinaryMarker = (byte)'l';
        private static final byte dateBinaryMarker = (byte)'d';
        private static final byte arrayBeginBinaryMarker = (byte)'[';
        private static final byte arrayEndBinaryMarker = (byte)']';
        private static final byte mapBeginBinaryMarker = (byte)'{';
        private static final byte mapEndBinaryMarker = (byte)'}';
        private static final byte keyBinaryMarker = (byte)'k';

        private static final byte[] llsdBinaryHeadBytes = llsdBinaryHead.getBytes(Charset.forName("US-ASCII"));

        /// <summary>
        /// Deserializes binary LLSD
        /// </summary>
        /// <param name="binaryData">Serialized data</param>
        /// <returns>OSD containting deserialized data</returns>
        public static OSD DeserializeLLSDBinary(byte[] binaryData) throws IOException, OSDException
        {
//            //System.out.println( Utils.bytesToHexString(binaryData, "Input"));
        	ByteArrayInputStream stream = new ByteArrayInputStream(binaryData);
//        	//System.out.println(Utils.bytesToHexString(binaryData, "binaryData"));
            OSD osd = DeserializeLLSDBinary(stream);
            stream.close();
            return osd;
        }

        /// <summary>
        /// Deserializes binary LLSD
        /// </summary>
        /// <param name="stream">Stream to read the data from</param>
        /// <returns>OSD containting deserialized data</returns>
        public static OSD DeserializeLLSDBinary(InputStream stream) throws IOException, OSDException
        {
//            if (!stream.CanSeek)
//                throw new OSDException("Cannot deserialize binary LLSD from unseekable streams");

            SkipWhiteSpace(stream);
            if (!FindString(stream, llsdBinaryHead) && !FindString(stream, llsdBinaryHead2))
            {
            	/*FIXME enabling following line raises error when AssetMesh call this function to decode
            	 * Meshed Primitives 
            	*/
                //throw new OSDException("Failed to decode binary LLSD");
            }
            SkipWhiteSpace(stream);
            return ParseLLSDBinaryElement(stream);
        }

        /// <summary>
        /// Serializes OSD to binary format. It does no prepend header
        /// </summary>
        /// <param name="osd">OSD to serialize</param>
        /// <returns>Serialized data</returns>
        public static byte[] SerializeLLSDBinary(OSD osd) throws Exception
        {
            return SerializeLLSDBinary(osd, true);
        }

        /// <summary>
        /// Serializes OSD to binary format
        /// </summary>
        /// <param name="osd">OSD to serialize</param>
        /// <param name="prependHeader"></param>
        /// <returns>Serialized data</returns>
        public static byte[] SerializeLLSDBinary(OSD osd, boolean prependHeader) throws Exception
        {
            ByteArrayOutputStream stream = SerializeLLSDBinaryStream(osd, prependHeader);
            byte[] binaryData = stream.toByteArray();

            stream.close();

            return binaryData;
        }

        /// <summary>
        /// Serializes OSD to binary format. It does no prepend header
        /// </summary>
        /// <param name="data">OSD to serialize</param>
        /// <returns>Serialized data</returns>
        public static ByteArrayOutputStream SerializeLLSDBinaryStream(OSD data) throws Exception
        {
            return SerializeLLSDBinaryStream(data, true);
        }
        /// <summary>
        /// Serializes OSD to binary format
        /// </summary>
        /// <param name="data">OSD to serialize</param>
        /// <param name="prependHeader"></param>
        /// <returns>Serialized data</returns>
        public static ByteArrayOutputStream SerializeLLSDBinaryStream(OSD data, boolean prependHeader) throws Exception
        {
        	ByteArrayOutputStream stream = new ByteArrayOutputStream(initialBufferSize);

            if (prependHeader)
            {
                stream.write(llsdBinaryHeadBytes, 0, llsdBinaryHeadBytes.length);
            }
            SerializeLLSDBinaryElement(stream, data);
            return stream;
        }

        private static void SerializeLLSDBinaryElement(ByteArrayOutputStream stream, OSD osd) throws Exception
        {
            switch (osd.getType())
            {
                case Unknown:
                    stream.write(undefBinaryValue);
                    break;
                case Boolean:
                    stream.write(osd.asBinary(), 0, 1);
                    break;
                case Integer:
                    stream.write(integerBinaryMarker);
                    stream.write(osd.asBinary(), 0, int32Length);
                    break;
                case Real:
                    stream.write(realBinaryMarker);
                    stream.write(osd.asBinary(), 0, doubleLength);
                    break;
                case UUID:
                    stream.write(uuidBinaryMarker);
                    stream.write(osd.asBinary(), 0, 16);
                    break;
                case String:
                    stream.write(StringBinaryMarker);
                    byte[] rawString = osd.asBinary();
                    byte[] StringLengthNetEnd = HostToNetworkIntBytes(rawString.length);
                    stream.write(StringLengthNetEnd, 0, int32Length);
                    stream.write(rawString, 0, rawString.length);
                    break;
                case Binary:
                    stream.write(binaryBinaryMarker);
                    byte[] rawBinary = osd.asBinary();
                    byte[] binaryLengthNetEnd = HostToNetworkIntBytes(rawBinary.length);
                    stream.write(binaryLengthNetEnd, 0, int32Length);
                    stream.write(rawBinary, 0, rawBinary.length);
                    break;
                case Date:
                    stream.write(dateBinaryMarker);
                    stream.write(osd.asBinary(), 0, doubleLength);
                    break;
                case URI:
                    stream.write(uriBinaryMarker);
                    byte[] rawURI = osd.asBinary();
                    byte[] uriLengthNetEnd = HostToNetworkIntBytes(rawURI.length);
                    stream.write(uriLengthNetEnd, 0, int32Length);
                    stream.write(rawURI, 0, rawURI.length);
                    break;
                case Array:
                    SerializeLLSDBinaryArray(stream, (OSDArray)osd);
                    break;
                case Map:
                    SerializeLLSDBinaryMap(stream, (OSDMap)osd);
                    break;
                default:
                    throw new OSDException("Binary serialization: Not existing element discovered.");

            }
        }

        private static void SerializeLLSDBinaryArray(ByteArrayOutputStream stream, OSDArray osdArray) throws Exception
        {
            stream.write(arrayBeginBinaryMarker);
            byte[] binaryNumElementsHostEnd = HostToNetworkIntBytes(osdArray.count());
            stream.write(binaryNumElementsHostEnd, 0, int32Length);

            for(OSD osd :osdArray)
            {
                SerializeLLSDBinaryElement(stream, osd);
            }
            stream.write(arrayEndBinaryMarker);
        }

        private static void SerializeLLSDBinaryMap(ByteArrayOutputStream stream, OSDMap osdMap) throws Exception
        {
            stream.write(mapBeginBinaryMarker);
            byte[] binaryNumElementsNetEnd = HostToNetworkIntBytes(osdMap.count());
            stream.write(binaryNumElementsNetEnd, 0, int32Length);

            for (Map.Entry<String, OSD> kvp :osdMap.entrySet())
            {
                stream.write(keyBinaryMarker);
                byte[] binaryKey = kvp.getKey().getBytes("UTF-8");
                byte[] binaryKeyLength = HostToNetworkIntBytes(binaryKey.length);
                stream.write(binaryKeyLength, 0, int32Length);
                stream.write(binaryKey, 0, binaryKey.length);
                SerializeLLSDBinaryElement(stream, kvp.getValue());
            }
            stream.write(mapEndBinaryMarker);
        }

        private static OSD ParseLLSDBinaryElement(InputStream stream) throws IOException, OSDException
        {
            SkipWhiteSpace(stream);
            OSD osd;

            int marker = stream.read();
            if (marker < 0)
                throw new OSDException("Binary LLSD parsing: Unexpected end of stream.");

            switch ((byte)marker)
            {
                case undefBinaryValue:
                    osd = new OSD();
                    break;
                case trueBinaryValue:
                    osd = OSD.FromBoolean(true);
                    break;
                case falseBinaryValue:
                    osd = OSD.FromBoolean(false);
                    break;
                case integerBinaryMarker:
                    int integer = NetworkToHostInt(ConsumeBytes(stream, int32Length));
                    //System.out.println("Found Integer Binary Marker");
                    osd = OSD.FromInteger(integer);
                    break;
                case realBinaryMarker:
                    //System.out.println("Found Real Binary Marker");
                    double dbl = NetworkToHostDouble(ConsumeBytes(stream, doubleLength));
                    //System.out.println("Double Value: " + dbl);                    
                    osd = OSD.FromReal(dbl);
                    break;
                case uuidBinaryMarker:
                    //System.out.println("Found UUID Binary Marker");
                    osd = OSD.FromUUID(new UUID(ConsumeBytes(stream, 16), 0));
                    break;
                case binaryBinaryMarker:
                    int binaryLength = NetworkToHostInt(ConsumeBytes(stream, int32Length));
                    osd = OSD.FromBinary(ConsumeBytes(stream, binaryLength));
                    break;
                case StringBinaryMarker:
                    int StringLength = NetworkToHostInt(ConsumeBytes(stream, int32Length));
                    //System.out.print("Length of String:" + StringLength);
                    String ss = new String(ConsumeBytes(stream, StringLength), "UTF-8");
                    //System.out.print("Found Length:" + StringLength + "\n");
                    osd = OSD.FromString(ss);
                    break;
                case uriBinaryMarker:
                    int uriLength = NetworkToHostInt(ConsumeBytes(stream, int32Length));
                    String sUri = new String(ConsumeBytes(stream, uriLength), "UTF-8");
                    URI uri[] = new URI[1];
                    if(!Utils.tryParseUri(sUri, uri))
                    {
                        throw new OSDException("Binary LLSD parsing: Invalid Uri format detected.");
                    }
                    osd = OSD.FromUri(uri[0]);
                    break;
                case dateBinaryMarker:
                	//TODO Check why Little Endian Conversion is required in this case
                    double timestamp = Utils.bytesToDoubleLit(ConsumeBytes(stream, doubleLength), 0);
                    System.out.println("Timestamp: " + timestamp);
                    	Date date = Utils.unixTimeToDate((long)(timestamp));
                    osd = OSD.FromDate(date);
                    break;
                case arrayBeginBinaryMarker:
                    osd = ParseLLSDBinaryArray(stream);
                    break;
                case mapBeginBinaryMarker:
                    osd = ParseLLSDBinaryMap(stream);
                    break;
                default:
                    throw new OSDException("Binary LLSD parsing: Unknown type marker: " + marker);

            }
            return osd;
        }

        private static OSD ParseLLSDBinaryArray(InputStream stream) throws OSDException, IOException
        {
            int numElements = NetworkToHostInt(ConsumeBytes(stream, int32Length));
            int crrElement = 0;
            OSDArray osdArray = new OSDArray();
            while (crrElement < numElements)
            {
                osdArray.add(ParseLLSDBinaryElement(stream));
                crrElement++;
            }

            if (!FindByte(stream, arrayEndBinaryMarker))
                throw new OSDException("Binary LLSD parsing: Missing end marker in array.");

            return (OSD)osdArray;
        }

        private static OSD ParseLLSDBinaryMap(InputStream stream) throws IOException, OSDException
        {
            int numElements = NetworkToHostInt(ConsumeBytes(stream, int32Length));
            int crrElement = 0;
            OSDMap osdMap = new OSDMap();
            while (crrElement < numElements)
            {
                if (!FindByte(stream, keyBinaryMarker))
                    throw new OSDException("Binary LLSD parsing: Missing key marker in map.");
                int keyLength = NetworkToHostInt(ConsumeBytes(stream, int32Length));
                String key = new String(ConsumeBytes(stream, keyLength), "UTF-8");
                osdMap.put(key, ParseLLSDBinaryElement(stream));
                crrElement++;
            }

            if (!FindByte(stream, mapEndBinaryMarker))
                throw new OSDException("Binary LLSD parsing: Missing end marker in map.");

            return (OSD)osdMap;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="stream"></param>
        public static void SkipWhiteSpace(InputStream stream) throws IOException
        {
            int bt;
            int count = 0;
        	stream.mark(stream.available() + 1);

            while (((bt = stream.read()) > 0) &&
                ((byte)bt == ' ' || (byte)bt == '\t' ||
                  (byte)bt == '\n' || (byte)bt == '\r')
                 )
            {
            	//mark the current byte in the stream
            	count ++;
            }
            stream.reset();
            stream.skip(count);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="stream"></param>
        /// <param name="toFind"></param>
        /// <returns></returns>
        public static boolean FindByte(InputStream stream, byte toFind) throws IOException
        {
        	//mark the current position
        	stream.mark(2);
            int bt = stream.read();
            if (bt < 0)
                return false;
            if ((byte)bt == toFind)
                return true;
            else
            {
                stream.reset();
                return false;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="stream"></param>
        /// <param name="toFind"></param>
        /// <returns></returns>
        public static boolean FindString(InputStream stream, String toFind) throws IOException
        {
            int lastIndexToFind = toFind.length() - 1;
            int crrIndex = 0;
            boolean found = true;
            int count = 0;
            int bt = 0;
            stream.mark(stream.available() + 1);

//            //System.out.print("\nStream:[");
            while (found &&
                  ((bt = stream.read()) > 0) &&
                    (crrIndex <= lastIndexToFind)
                  )
            {
//            	//System.out.print(Character.toString((char)bt));
            	count ++;
                if (toFind.substring(crrIndex, crrIndex+1).toString().equalsIgnoreCase((Character.toString((char)bt))))
                {
                    found = true;
                    crrIndex++;
                }
                else
                    found = false;
            }

//        	//System.out.print("]\n Last Char:" + bt);
            
            if (found && crrIndex > lastIndexToFind)
            {
            	stream.reset();
            	stream.skip(count);
                //stream.Seek(-1L, SeekOrigin.Current);
                return true;
            }
            else
            {
                stream.reset();
                return false;
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="stream"></param>
        /// <param name="consumeBytes"></param>
        /// <returns></returns>
        public static byte[] ConsumeBytes(InputStream stream, int consumeBytes) throws IOException, OSDException
        {
            byte[] bytes = new byte[consumeBytes];
            if(consumeBytes > 0)
            {
            	if (stream.read(bytes, 0, consumeBytes) < consumeBytes)
                throw new OSDException("Binary LLSD parsing: Unexpected end of stream."); 
            }
//        	//System.out.print("\t" + Utils.bytesToHexString(bytes, "consumed bytes\t"));
            return bytes;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="binaryNetEnd"></param>
        /// <returns></returns>
        public static int NetworkToHostInt(byte[] binaryNetEnd)
        {
            if (binaryNetEnd == null)
                return -1;

            int intNetEnd = Utils.bytesToInt(binaryNetEnd, 0);
//            int intHostEnd = System.Net.IPAddress.NetworkToHostOrder(intNetEnd);
            int intHostEnd= intNetEnd;
            //System.out.println(intHostEnd);
            return intHostEnd;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="binaryNetEnd"></param>
        /// <returns></returns>
        public static double NetworkToHostDouble(byte[] binaryNetEnd)
        {
            if (binaryNetEnd == null)
                return -1d;
//            long longNetEnd = Utils.bytesToInt64Big(binaryNetEnd, 0);
        	//In Java, streams are in network byte by default
//            long longHostEnd = System.Net.IPAddress.NetworkToHostOrder(longNetEnd);
//            long longHostEnd = longNetEnd; 
//            byte[] binaryHostEnd = Utils.int64ToBytes(longHostEnd);
            return Utils.bytesToDouble(binaryNetEnd, 0);
        }
        
        /// <summary>
        /// 
        /// </summary>
        /// <param name="intHostEnd"></param>
        /// <returns></returns>   
        public static byte[] HostToNetworkIntBytes(int intHostEnd)
        {
        	int intNetEnd= intHostEnd;
        	//In Java, streams are in network byte by default
//            int intNetEnd = System.Net.IPAddress.HostToNetworkOrder(intHostEnd);
            byte[] bytesNetEnd = Utils.intToBytes(intNetEnd);
            return bytesNetEnd;

        }
    }