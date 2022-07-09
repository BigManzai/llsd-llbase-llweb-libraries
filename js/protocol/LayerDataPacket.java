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

import com.ngt.jopenmetaverse.shared.util.Utils;

	public final class LayerDataPacket extends Packet
    {
        /// <exclude/>
        public static final class LayerIDBlock extends PacketBlock
        {
            public byte Type;

            @Override
			public int getLength()
            {
                                {
                    return 1;
                }
            }

            public LayerIDBlock() { }
            public LayerIDBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    Type = (byte)bytes[i[0]++];
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = Type;
            }

        }

        /// <exclude/>
        public static final class LayerDataBlock extends PacketBlock
        {
            public byte[] Data;

            @Override
			public int getLength()
            {
                                {
                    int length = 2;
                    if (Data != null) { length += Data.length; }
                    return length;
                }
            }

            public LayerDataBlock() { }
            public LayerDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Data = new byte[length];
                    Utils.arraycopy(bytes, i[0], Data, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)(Data.length % 256);
                bytes[i[0]++] = (byte)((Data.length >> 8) % 256);
                Utils.arraycopy(Data, 0, bytes, i[0], Data.length); i[0] +=  Data.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 7;
                length += LayerID.getLength();
                length += LayerData.getLength();
                return length;
            }
        }
        public LayerIDBlock LayerID;
        public LayerDataBlock LayerData;

        public LayerDataPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.LayerData;
            this.header =  new Header();
            header.Frequency = PacketFrequency.High;
            header.ID = 11;
            header.Reliable = true;
            LayerID = new LayerIDBlock();
            LayerData = new LayerDataBlock();
        }

        public LayerDataPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            LayerID.FromBytes(bytes, i);
            LayerData.FromBytes(bytes, i);
        }

        public LayerDataPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            LayerID.FromBytes(bytes, i);
            LayerData.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 7;
            length += LayerID.getLength();
            length += LayerData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            LayerID.ToBytes(bytes, i);
            LayerData.ToBytes(bytes, i);
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
