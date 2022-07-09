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
package com.ngt.jopenmetaverse.shared.protocol;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;



    /// <summary>
    /// Static helper functions and global variables
    /// </summary>
    public class Helpers
    {
        /// <summary>This header flag signals that ACKs are appended to the packet</summary>
        public final static byte MSG_APPENDED_ACKS = (byte)0x10;
        /// <summary>This header flag signals that this packet has been sent before</summary>
        public final static byte MSG_RESENT = (byte)0x20;
        /// <summary>This header flags signals that an ACK is expected for this packet</summary>
        public final static byte MSG_RELIABLE = (byte)0x40;
        /// <summary>This header flag signals that the message is compressed using zerocoding</summary>
        public final static byte MSG_ZEROCODED = (byte)0x80;

        /// <summary>
        /// Passed to Logger.Log() to identify the severity of a log entry
        /// </summary>
        public enum LogLevel
        {
            /// <summary>No logging information will be output</summary>
            None,
            /// <summary>Non-noisy useful information, may be helpful in 
            /// debugging a problem</summary>
            Info,
            /// <summary>A non-critical error occurred. A warning will not 
            /// prevent the rest of the library from operating as usual, 
            /// although it may be indicative of an underlying issue</summary>
            Warning,
            /// <summary>A critical error has occurred. Generally this will 
            /// be followed by the network layer shutting down, although the 
            /// stability of the library after an error is uncertain</summary>
            Error,
            /// <summary>Used for private testing, this logging level can 
            /// generate very noisy (long and/or repetitive) messages. Don't
            /// pass this to the Log() function, use DebugLog() instead.
            /// </summary>
            Debug
        };

        /// <summary>
        /// 
        /// </summary>
        /// <param name="offset"></param>
        /// <returns></returns>
        public static short TEOffsetShort(float offset)
        {
            offset = Utils.clamp(offset, -1.0f, 1.0f);
            offset *= 32767.0f;
            return (short)Math.round(offset);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="bytes"></param>
        /// <param name="pos"></param>
        /// <returns></returns>
        public static float TEOffsetFloat(byte[] bytes, int pos)
        {
            float offset = (float)Utils.bytesToInt16(bytes, pos);
            return offset / 32767.0f;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="rotation"></param>
        /// <returns></returns>
        public static short TERotationShort(float rotation)
        {
            final double TWO_PI = Math.PI * 2.0d;
            double remainder = Math.IEEEremainder(rotation, TWO_PI);
            return (short)Math.round((remainder / TWO_PI) * 32767.0d);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="bytes"></param>
        /// <param name="pos"></param>
        /// <returns></returns>
        public static float TERotationFloat(byte[] bytes, int pos)
        {
            final float TWO_PI = (float)(Math.PI * 2.0d);
            return (float)((bytes[pos] | (bytes[pos + 1] << 8)) / 32767.0f) * TWO_PI;
        }

        public static byte TEGlowByte(float glow)
        {
            return (byte)(glow * 255.0f);
        }

        public static float TEGlowFloat(byte[] bytes, int pos)
        {
            return (float)bytes[pos] / 255.0f;
        }

        /// <summary>
        /// Given an X/Y location in absolute (grid-relative) terms, a region
        /// handle is returned along with the local X/Y location in that region
        /// </summary>
        /// <param name="globalX">The absolute X location, a number such as 
        /// 255360.35</param>
        /// <param name="globalY">The absolute Y location, a number such as
        /// 255360.35</param>
        /// <param name="localX">The sim-local X position of the global X
        /// position, a value from 0.0 to 256.0</param>
        /// <param name="localY">The sim-local Y position of the global Y
        /// position, a value from 0.0 to 256.0</param>
        /// <returns>A 64-bit region handle that can be used to teleport to</returns>
        public static BigInteger GlobalPosToRegionHandle(float globalX, float globalY, float local[])
        {
            long x = ((long)globalX / 256) * 256;
            long y = ((long)globalY / 256) * 256;
            local[0] = globalX - (float)x;
            local[1] = globalY - (float)y;
            return new BigInteger(Utils.int64ToBytes((x << 32) | y));
        }

        /// <summary>
        /// Converts a floating point number to a terse String format used for
        /// transmitting numbers in wearable asset files
        /// </summary>
        /// <param name="val">Floating point number to convert to a String</param>
        /// <returns>A terse String representation of the input number</returns>
        public static String FloatToTerseString(float val)
        {
            StringBuilder s = new StringBuilder(String.format("%.2f", val));

            if (val == 0)
                return ".00";

            // Trim trailing zeroes
            while (s.charAt(s.length() - 1) == '0')
                s = s.delete(s.length() - 1, s.length());

            // Remove superfluous decimal places after the trim
            if (s.charAt((s.length() - 1)) == '.')
                s = s.delete(s.length() - 1, s.length());
            // Remove leading zeroes after a negative sign
            else if (s.charAt(0) == '-' && s.charAt(1) == '0')
                s = s.delete(1, 2);
            // Remove leading zeroes in positive numbers
            else if (s.charAt(0) == '0')
                s = s.delete(0, 1);

            return s.toString();
        }

        /// <summary>
        /// Convert a variable length field (byte array) to a String, with a
        /// field name prepended to each line of the output
        /// </summary>
        /// <remarks>If the byte array has unprintable characters in it, a 
        /// hex dump will be written instead</remarks>
        /// <param name="output">The StringBuilder object to write to</param>
        /// <param name="bytes">The byte array to convert to a String</param>
        /// <param name="fieldName">A field name to prepend to each line of output</param>
        private static void FieldToString(StringBuilder output, byte[] bytes, String fieldName)
        {
            // Check for a common case
            if (bytes.length == 0) return;

            boolean printable = true;

            for (int i = 0; i < bytes.length; ++i)
            {
                // Check if there are any unprintable characters in the array
                if ((bytes[i] < 0x20 || bytes[i] > 0x7E) && bytes[i] != 0x09
                    && bytes[i] != 0x0D && bytes[i] != 0x0A && bytes[i] != 0x00)
                {
                    printable = false;
                    break;
                }
            }

            if (printable)
            {
                if (fieldName.length() > 0)
                {
                    output.append(fieldName);
                    output.append(": ");
                }

                if (bytes[bytes.length - 1] == 0x00)
                    output.append(Utils.bytesToString(bytes, 0, bytes.length - 1));
                else
                    output.append(Utils.bytesToString(bytes, 0, bytes.length));
            }
            else
            {
                for (int i = 0; i < bytes.length; i += 16)
                {
                    if (i != 0)
                        output.append('\n');
                    if (fieldName.length() > 0)
                    {
                        output.append(fieldName);
                        output.append(": ");
                    }

                    for (int j = 0; j < 16; j++)
                    {
                        if ((i + j) < bytes.length)
                            output.append(String.format("%2X ", bytes[i + j]));
                        else
                            output.append("   ");
                    }
                }
            }
        }

        /// <summary>
        /// Decode a zerocoded byte array, used to decompress packets marked
        /// with the zerocoded flag
        /// </summary>
        /// <remarks>Any time a zero is encountered, the next byte is a count 
        /// of how many zeroes to expand. One zero is encoded with 0x00 0x01, 
        /// two zeroes is 0x00 0x02, three zeroes is 0x00 0x03, etc. The 
        /// first four bytes are copied directly to the output buffer.
        /// </remarks>
        /// <param name="src">The byte array to decode</param>
        /// <param name="srclen">The length of the byte array to decode. This 
        /// would be the length of the packet up to (but not including) any
        /// appended ACKs</param>
        /// <param name="dest">The output byte array to decode to</param>
        /// <returns>The length of the output buffer</returns>
        public static int ZeroDecode(byte[] src, int srclen, byte[] dest)
        {
            if (srclen > src.length)
                throw new IllegalArgumentException("srclen cannot be greater than src.length");

            int zerolen = 0;
            int bodylen = 0;
            int i = 0;

            try
            {
        		System.arraycopy(src, 0, dest, 0, 6);
                zerolen = 6;
                bodylen = srclen;

                for (i = zerolen; i < bodylen; i++)
                {
                    if (src[i] == 0x00)
                    {
                        for (byte j = 0; j < src[i + 1]; j++)
                        {
                            dest[zerolen++] = 0x00;
                        }

                        i++;
                    }
                    else
                    {
                        dest[zerolen++] = src[i];
                    }
                }

                // Copy appended ACKs
                for (; i < srclen; i++)
                {
                    dest[zerolen++] = src[i];
                }

                return (int)zerolen;
            }
            catch (Exception ex)
            {
                //Logger.Log(String.format("Zerodecoding error: i={0}, srclen={1}, bodylen={2}, zerolen={3}\n{4}\n{5}",
                  //  i, srclen, bodylen, zerolen, Utils.bytesToHexString(src, srclen, null), ex), LogLevel.Error);

                throw new IndexOutOfBoundsException(String.format("Zerodecoding error: i=%d, srclen=%d, bodylen=%d, zerolen=%d\n%s\n%s",
                    i, srclen, bodylen, zerolen, 
                    Utils.bytesToHexDebugString(src, srclen, null), 
                    Utils.getExceptionStackTraceAsString(ex)));
            }
        }

        /// <summary>
        /// Encode a byte array with zerocoding. Used to compress packets marked
        /// with the zerocoded flag. Any zeroes in the array are compressed down
        /// to a single zero byte followed by a count of how many zeroes to expand
        /// out. A single zero becomes 0x00 0x01, two zeroes becomes 0x00 0x02,
        /// three zeroes becomes 0x00 0x03, etc. The first four bytes are copied
        /// directly to the output buffer.
        /// </summary>
        /// <param name="src">The byte array to encode</param>
        /// <param name="srclen">The length of the byte array to encode</param>
        /// <param name="dest">The output byte array to encode to</param>
        /// <returns>The length of the output buffer</returns>
        public static int ZeroEncode(byte[] src, int srclen, byte[] dest)
        {
            int zerolen = 0;
            byte zerocount = 0;

            
            System.arraycopy(src, 0, dest, 0, 6);
            zerolen += 6;

            int bodylen;
            if ((src[0] & MSG_APPENDED_ACKS) == 0)
            {
                bodylen = srclen;
            }
            else
            {
                bodylen = srclen - src[srclen - 1] * 4 - 1;
            }

            int i;
            for (i = zerolen; i < bodylen; i++)
            {
                if (src[i] == 0x00)
                {
                    zerocount++;

                    if (zerocount == 0)
                    {
                        dest[zerolen++] = 0x00;
                        dest[zerolen++] = (byte) 0xff;
                        zerocount++;
                    }
                }
                else
                {
                    if (zerocount != 0)
                    {
                        dest[zerolen++] = 0x00;
                        dest[zerolen++] = (byte)zerocount;
                        zerocount = 0;
                    }

                    dest[zerolen++] = src[i];
                }
            }

            if (zerocount != 0)
            {
                dest[zerolen++] = 0x00;
                dest[zerolen++] = (byte)zerocount;
            }

            // copy appended ACKs
            for (; i < srclen; i++)
            {
                dest[zerolen++] = src[i];
            }

            return (int)zerolen;
        }

        /// <summary>
        /// Calculates the CRC (cyclic redundancy check) needed to upload inventory.
        /// </summary>
        /// <param name="creationDate">Creation date</param>
        /// <param name="saleType">Sale type</param>
        /// <param name="invType">Inventory type</param>
        /// <param name="type">Type</param>
        /// <param name="assetID">Asset ID</param>
        /// <param name="groupID">Group ID</param>
        /// <param name="salePrice">Sale price</param>
        /// <param name="ownerID">Owner ID</param>
        /// <param name="creatorID">Creator ID</param>
        /// <param name="itemID">Item ID</param>
        /// <param name="folderID">Folder ID</param>
        /// <param name="everyoneMask">Everyone mask (permissions)</param>
        /// <param name="flags">Flags</param>
        /// <param name="nextOwnerMask">Next owner mask (permissions)</param>
        /// <param name="groupMask">Group mask (permissions)</param>
        /// <param name="ownerMask">Owner mask (permissions)</param>
        /// <returns>The calculated CRC</returns>
        public static long InventoryCRC(int creationDate, byte saleType, byte invType, byte type,
            UUID assetID, UUID groupID, int salePrice, UUID ownerID, UUID creatorID,
            UUID itemID, UUID folderID, long everyoneMask, long flags, long nextOwnerMask,
            long groupMask, long ownerMask)
        {
            long CRC = 0;

            // IDs
            CRC += assetID.CRC(); // AssetID
            CRC += folderID.CRC(); // FolderID
            CRC += itemID.CRC(); // ItemID

            // Permission stuff
            CRC += creatorID.CRC(); // CreatorID
            CRC += ownerID.CRC(); // OwnerID
            CRC += groupID.CRC(); // GroupID

            // CRC += another 4 words which always seem to be zero -- unclear if this is a UUID or what
            CRC += ownerMask;
            CRC += nextOwnerMask;
            CRC += everyoneMask;
            CRC += groupMask;

            // The rest of the CRC fields
            CRC += flags; // Flags
            CRC += (long)invType; // InvType
            CRC += (long)type; // Type 
            CRC += (long)creationDate; // CreationDate
            CRC += (long)salePrice;    // SalePrice
            CRC += (long)((long)saleType * 0x07073096); // SaleType

            return CRC;
        }

//        /// <summary>
//        /// Attempts to load a file embedded in the assembly
//        /// </summary>
//        /// <param name="resourceName">The filename of the resource to load</param>
//        /// <returns>A Stream for the requested file, or null if the resource
//        /// was not successfully loaded</returns>
//        public static System.IO.Stream GetResourceStream(String resourceName)
//        {
//            return GetResourceStream(resourceName, "openmetaverse_data");
//        }

        /// <summary>
        /// Attempts to load a file either embedded in the assembly or found in
        /// a given search path
        /// </summary>
        /// <param name="resourceName">The filename of the resource to load</param>
        /// <param name="searchPath">An optional path that will be searched if
        /// the asset is not found embedded in the assembly</param>
        /// <returns>A Stream for the requested file, or null if the resource
        /// was not successfully loaded</returns>
        public static InputStream GetResourceStream(String resourceName, String searchPath)
        {
            if (searchPath != null)
            {
        		URL fileLocation =  Helpers.class.getClassLoader().getResource(searchPath);
            	String fullPath = fileLocation.getPath() + "/" + resourceName;
        		
//                String filename = System.IO.Path.Combine(System.IO.Path.Combine(System.IO.Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), searchPath), resourceName);
                try
                {
                	return new FileInputStream(new File(fullPath));
//                    return new System.IO.FileStream(
//                        filename,
//                        System.IO.FileMode.Open, System.IO.FileAccess.Read, System.IO.FileShare.Read);
                }
                catch (Exception ex)
                {
                    JLogger.error(String.format("Failed opening resource from file %s: %s \n %s", fullPath, ex.getMessage(), 
                    		Utils.getExceptionStackTraceAsString(ex)));
                }
            }
            //TODO need to handle following
//            else
//            {
//                try
//                {
//                    System.Reflection.Assembly a = System.Reflection.Assembly.GetExecutingAssembly();
//                    System.IO.Stream s = a.GetManifestResourceStream("OpenMetaverse.Resources." + resourceName);
//                    if (s != null) return s;
//                }
//                catch (Exception ex)
//                {
//                	//TODO Need to put the logger
//                    Logger.getLogger(Helpers.class.getName()).log(Level.SEVERE, String.format("Failed opening resource stream: {0}", ex.getMessage()));
//                }
//            }

            return null;
        }
        
        public static String GetResourcePath(String resourceName, String searchPath)
        {
//        	URL fileLocation =  Helpers.class.getClassLoader().getResource(searchPath);
        	return  searchPath + "/" + resourceName;
        }
        
        
        /// <summary>
        /// Converts a list of primitives to an object that can be serialized
        /// with the LLSD system
        /// </summary>
        /// <param name="prims">Primitives to convert to a serializable object</param>
        /// <returns>An object that can be serialized with LLSD</returns>
        public static OSD PrimListToOSD(List<Primitive> prims)
        {
            OSDMap map = new OSDMap(prims.size());

            for (int i = 0; i < prims.size(); i++)
                map.put(Long.toString(prims.get(i).LocalID), prims.get(i).GetOSD());

            return map;
        }

        /// <summary>
        /// Deserializes OSD in to a list of primitives
        /// </summary>
        /// <param name="osd">Structure holding the serialized primitive list,
        /// must be of the SDMap type</param>
        /// <returns>A list of deserialized primitives</returns>
        public static List<Primitive> OSDToPrimList(OSD osd)
        {
            if (osd.getType() != OSDType.Map)
                throw new IllegalArgumentException("LLSD must be in the Map structure");

            OSDMap map = (OSDMap)osd;
            List<Primitive> prims = new ArrayList<Primitive>(map.count());

            for(Map.Entry<String, OSD> kvp : map.entrySet())
            {
                Primitive prim = Primitive.FromOSD(kvp.getValue());
                prim.LocalID = Long.parseLong(kvp.getKey());
                prims.add(prim);
            }

            return prims;
        }

        /// <summary>
        /// Converts a struct or class object containing fields only into a key value separated String
        /// </summary>
        /// <param name="t">The struct object</param>
        /// <returns>A String containing the struct fields as the keys, and the field value as the value separated</returns>
        /// <example>
        /// <code>
        /// // Add the following code to any struct or class containing only fields to override the ToString() 
        /// // method to display the values of the passed object
        /// 
        /// /// <summary>Print the struct data as a String</summary>
        /// ///<returns>A String containing the field name, and field value</returns>
        ///public override String ToString()
        ///{
        ///    return Helpers.StructToString(this);
        ///}
        /// </code>
        /// </example>
        public static String StructToString(Object t) throws IllegalArgumentException, IllegalAccessException
        {
            StringBuilder result = new StringBuilder();            
            Field[] fields = t.getClass().getFields();
            for (int i = 0; i < fields.length; i++)
            {
                Field field = fields[i];
                result.append(field.getName() + ": " + field.get(t) + " ");                
            }
            
            result.append("\n");
            return result.toString().trim();
        }
        
        public static String StructToStringWithOutException(Object t)
        {
        	try {
				return StructToString(t);
			} catch (Exception e) {
	            	JLogger.warn(Utils.getExceptionStackTraceAsString(e));
	            	return 	"Error" + e.getMessage();		
	        } 
        }
        
    }
