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

import java.io.UnsupportedEncodingException;

import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;


public class BitPack
    {
        /// <summary></summary>
        public byte[] Data;

        /// <summary></summary>
        public int getBytePos()
        {
                if (bytePos != 0 && bitPos == 0)
                    return bytePos - 1;
                else
                    return bytePos;
        }

        /// <summary></summary>
        public int getBitPos() { return bitPos; } 


        private final int MAX_BITS = 8;
        private static final byte[] ON = new byte[] { 1 };
        private static final byte[] OFF = new byte[] { 0 };

        private int bytePos=0;
        private int bitPos=0;
//        private boolean weAreBigEndian = !BitConverter.IsLittleEndian;


        /// <summary>
        /// Default finalructor, initialize the bit packer / bit unpacker
        /// with a byte array and starting position
        /// </summary>
        /// <param name="data">Byte array to pack bits in to or unpack from</param>
        /// <param name="pos">Starting position in the byte array</param>
        public BitPack(byte[] data, int pos)
        {
            Data = data;
            bytePos = pos;
            bitPos=0;
        }

        /// <summary>
        /// Pack a floating point value in to the data
        /// </summary>
        /// <param name="data">Floating point value to pack</param>
        public void PackFloat(float data)
        {
            byte[] input = Utils.floatToBytes(data);
            Utils.reverse(input);
            PackBitArray(input, 32);
        }

        /// <summary>
        /// Pack part or all of an integer in to the data
        /// </summary>
        /// <param name="data">Integer containing the data to pack</param>
        /// <param name="totalCount">Number of bits of the integer to pack</param>
        public void PackBits(int data, int totalCount)
        {
            byte[] input = Utils.intToBytes(data);
//            if (weAreBigEndian) Array.Reverse(input);
            Utils.reverse(input);
            PackBitArray(input, totalCount);
        }

        /// <summary>
        /// Pack part or all of an unsigned integer in to the data
        /// </summary>
        /// <param name="data">Unsigned integer containing the data to pack</param>
        /// <param name="totalCount">Number of bits of the integer to pack</param>
        public void PackBits(long data, int totalCount)
        {
            byte[] input = Utils.intToBytes((int)data & 0xffffffff);
            //if (weAreBigEndian) Array.Reverse(input);
            Utils.reverse(input);
            PackBitArray(input, totalCount);
        }

        /// <summary>
        /// Pack a single bit in to the data
        /// </summary>
        /// <param name="bit">Bit to pack</param>
        public void PackBit(boolean bit)
        {
            if (bit)
                PackBitArray(ON, 1);
            else
                PackBitArray(OFF, 1);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="data"></param>
        /// <param name="isSigned"></param>
        /// <param name="intBits"></param>
        /// <param name="fracBits"></param>
        public void PackFixed(float data, boolean isSigned, int intBits, int fracBits) throws Exception
        {
            int unsignedBits = intBits + fracBits;
            int totalBits = unsignedBits;
            int min, max;

            if (isSigned)
            {
                totalBits++;
                min = 1 << intBits;
                min *= -1;
            }
            else
            {
                min = 0;
            }

            max = 1 << intBits;

            float fixedVal = Utils.clamp(data, (float)min, (float)max);
            if (isSigned) fixedVal += max;
            fixedVal *= 1 << fracBits;

            if (totalBits <= 8)
                PackBits((long)fixedVal, 8);
            else if (totalBits <= 16)
                PackBits((long)fixedVal, 16);
            else if (totalBits <= 31)
                PackBits((long)fixedVal, 32);
            else
                throw new Exception("Can't use fixed point packing for " + totalBits);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="data"></param>
        public void PackUUID(UUID data)
        {
            byte[] bytes = data.GetBytes();

            // Not sure if our PackBitArray function can handle 128-bit byte
            //arrays, so using this for now
            for (int i = 0; i < 16; i++)
                PackBits(bytes[i], 8);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="data"></param>
        public void PackColor(Color4 data)
        {
            byte[] bytes = data.getBytes();
            PackBitArray(bytes, 32);
        }

        /// <summary>
        /// Unpacking a floating point value from the data
        /// </summary>
        /// <returns>Unpacked floating point value</returns>
        public float UnpackFloat()
        {
            byte[] output = UnpackBitsArray(32);
            Utils.reverse(output);
            return Utils.bytesToFloat(output, 0);
        }

        /// <summary>
        /// Unpack a variable number of bits from the data in to integer format
        /// </summary>
        /// <param name="totalCount">Number of bits to unpack</param>
        /// <returns>An integer containing the unpacked bits</returns>
        /// <remarks>This function is only useful up to 32 bits</remarks>
        public int UnpackBits(int totalCount)
        {
            byte[] output = UnpackBitsArray(totalCount);
            Utils.reverse(output);
            return Utils.bytesToInt(output, 0);
        }

        /// <summary>
        /// Unpack a variable number of bits from the data in to unsigned 
        /// integer format
        /// </summary>
        /// <param name="totalCount">Number of bits to unpack</param>
        /// <returns>An unsigned integer containing the unpacked bits</returns>
        /// <remarks>This function is only useful up to 32 bits</remarks>
        public long UnpackLBits(int totalCount)
        {
            byte[] output = UnpackBitsArray(totalCount);
            Utils.reverse(output);            
            return Utils.bytesToInt64(output, 0);
        }

        /// <summary>
        /// Unpack a 16-bit signed integer
        /// </summary>
        /// <returns>16-bit signed integer</returns>
        public short UnpackShort()
        {
            return (short)UnpackBits(16);
        }

        /// <summary>
        /// Unpack a 32-bit signed integer
        /// </summary>
        /// <returns>32-bit signed integer</returns>
        public int UnpackInt()
        {
            return UnpackBits(32);
        }

        /// <summary>
        /// Unpack a 32-bit unsigned integer
        /// </summary>
        /// <returns>32-bit unsigned integer</returns>
        public long UnpackLInt()
        {
            return UnpackLBits(32);
        }

        public byte UnpackByte()
        {
            byte[] output = UnpackBitsArray(8);
            return output[0];
        }

        public float UnpackFixed(boolean signed, int intBits, int fracBits)
        {
            int minVal;
            int maxVal;
            int unsignedBits = intBits + fracBits;
            int totalBits = unsignedBits;
            float fixedVal;

            if (signed)
            {
                totalBits++;

                minVal = 1 << intBits;
                minVal *= -1;
            }
            maxVal = 1 << intBits;

            if (totalBits <= 8)
                fixedVal = (float)UnpackByte();
            else if (totalBits <= 16)
                fixedVal = (float)UnpackLBits(16);
            else if (totalBits <= 31)
                fixedVal = (float)UnpackLBits(32);
            else
                return 0.0f;

            fixedVal /= (float)(1 << fracBits);

            if (signed) fixedVal -= (float)maxVal;

            return fixedVal;
        }

        public String UnpackString(int size) throws UnsupportedEncodingException
        {
            if (bitPos != 0 || bytePos + size > Data.length) throw new IndexOutOfBoundsException();

            String str = Utils.bytesToString(Data, bytePos, size, "UTF8");
//            String str = System.Text.UTF8Encoding.UTF8.GetString(Data, bytePos, size);
            bytePos += size;
            return str;
        }

        public UUID UnpackUUID()
        {
            if (bitPos != 0) throw new IndexOutOfBoundsException();

            UUID val = new UUID(Data, bytePos);
            bytePos += 16;
            return val;
        }

        private void PackBitArray(byte[] data, int totalCount)
        {
            int count = 0;
            int curBytePos = 0;
            int curBitPos = 0;

            while (totalCount > 0)
            {
                if (totalCount > MAX_BITS)
                {
                    count = MAX_BITS;
                    totalCount -= MAX_BITS;
                }
                else
                {
                    count = totalCount;
                    totalCount = 0;
                }

                while (count > 0)
                {
                    byte curBit = (byte)(0x80 >> bitPos);

                    if ((data[curBytePos] & (0x01 << (count - 1))) != 0)
                        Data[bytePos] |= curBit;
                    else
                        Data[bytePos] &= (byte)~curBit;

                    --count;
                    ++bitPos;
                    ++curBitPos;

                    if (bitPos >= MAX_BITS)
                    {
                        bitPos = 0;
                        ++bytePos;
                    }
                    if (curBitPos >= MAX_BITS)
                    {
                        curBitPos = 0;
                        ++curBytePos;
                    }
                }
            }
        }

        private byte[] UnpackBitsArray(int totalCount)
        {
            int count = 0;
            byte[] output = new byte[] {0x00, 0x00, 0x00, 0x00};
            int curBytePos = 0;
            int curBitPos = 0;

            while (totalCount > 0)
            {
                if (totalCount > MAX_BITS)
                {
                    count = MAX_BITS;
                    totalCount -= MAX_BITS;
                }
                else
                {
                    count = totalCount;
                    totalCount = 0;
                }

                while (count > 0)
                {
                    // Shift the previous bits
                    output[curBytePos] <<= 1;
                    // Grab one bit
                    if ((Data[bytePos] & (0x80 >> bitPos++)) != 0)
                        ++output[curBytePos];

                    --count;
                    ++curBitPos;

                    if (bitPos >= MAX_BITS)
                    {
                        bitPos = 0;
                        ++bytePos;
                    }
                    if (curBitPos >= MAX_BITS)
                    {
                        curBitPos = 0;
                        ++curBytePos;
                    }
                }
            }
            
//            System.out.println(Utils.bytesToHexString(Data, " UnpackBitsArray Data"));
//            System.out.println(Utils.bytesToHexString(output, " UnpackBitsArray Output"));
            
            return output;
        }
    }