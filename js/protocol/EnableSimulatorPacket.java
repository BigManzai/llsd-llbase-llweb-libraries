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

import java.math.BigInteger;

import com.ngt.jopenmetaverse.shared.util.Utils;


    public final class EnableSimulatorPacket extends Packet
    {
        /// <exclude/>
        public static final class SimulatorInfoBlock extends PacketBlock
        {
            public BigInteger Handle;
            public long IP;
            /**
             * Unsigned short
             * Only two of its least significant bytes should be used
             */
            public int Port;

            @Override
			public int getLength()
            {
                                {
                    return 14;
                }
            }

            public SimulatorInfoBlock() { }
            public SimulatorInfoBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    Handle = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    IP = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    Port = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.ulongToBytesLit(Handle, bytes, i[0]); i[0] += 8;
                Utils.uintToBytesLit(IP, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((Port >> 8) % 256);
                bytes[i[0]++] = (byte)(Port % 256);
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += SimulatorInfo.getLength();
                return length;
            }
        }
        public SimulatorInfoBlock SimulatorInfo;

        public EnableSimulatorPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.EnableSimulator;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 151;
            header.Reliable = true;
            SimulatorInfo = new SimulatorInfoBlock();
        }

        public EnableSimulatorPacket(byte[] bytes, int[] i) throws MalformedDataException 
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(bytes, i, packetEnd, null);
        }

        @Override
		public void FromBytes(byte[] bytes, int[] i, int[] packetEnd, byte[] zeroBuffer) throws MalformedDataException
        {
            header.FromBytes(bytes, i, packetEnd);
            if (header.Zerocoded && zeroBuffer != null)
            {
                packetEnd[0] = Helpers.ZeroDecode(bytes, packetEnd[0] + 1, zeroBuffer) - 1;
                bytes = zeroBuffer;
            }
            SimulatorInfo.FromBytes(bytes, i);
        }

        public EnableSimulatorPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            SimulatorInfo.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += SimulatorInfo.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            SimulatorInfo.ToBytes(bytes, i);
            if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
            return bytes;
        }

        @Override
			public byte[][] ToBytesMultiple()
        {
            return new byte[][] { ToBytes() };
        }
    }

    /// <exclude/>
