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

import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;


    public final class ParcelMediaUpdatePacket extends Packet
    {
        public final class DataBlockBlock extends PacketBlock
        {
            public byte[] MediaURL;
            public UUID MediaID = new UUID();
		/** Unsigned Byte */ 
		public byte MediaAutoScale;

            @Override
			public int getLength()
            {
                    int length = 18;
                    if (MediaURL != null) { length += MediaURL.length; }
                    return length;
            }

            public DataBlockBlock() { }
            public DataBlockBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    MediaURL = new byte[length];
                    Utils.arraycopy(bytes, i[0], MediaURL, 0, length); i[0] += length;
                    MediaID.FromBytes(bytes, i[0]); i[0] += 16;
                    MediaAutoScale = (byte)bytes[i[0]++];
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)MediaURL.length;
                Utils.arraycopy(MediaURL, 0, bytes, i[0], MediaURL.length); i[0] += MediaURL.length;
                MediaID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = MediaAutoScale;
            }

        }

        /// <exclude/>
        public static final class DataBlockExtendedBlock extends PacketBlock
        {
            public byte[] MediaType;
		/** Unsigned Byte */ 
		public byte[] MediaDesc;
            public int MediaWidth;
            public int MediaHeight;
		/** Unsigned Byte */ 
		public byte MediaLoop;

            @Override
			public int getLength()
            {
                                {
                    int length = 11;
                    if (MediaType != null) { length += MediaType.length; }
                    if (MediaDesc != null) { length += MediaDesc.length; }
                    return length;
                }
            }

            public DataBlockExtendedBlock() { }
            public DataBlockExtendedBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    MediaType = new byte[length];
                    Utils.arraycopy(bytes, i[0], MediaType, 0, length); i[0] += length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    MediaDesc = new byte[length];
                    Utils.arraycopy(bytes, i[0], MediaDesc, 0, length); i[0] += length;
                    MediaWidth = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    MediaHeight = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    MediaLoop = (byte)bytes[i[0]++];
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)MediaType.length;
                Utils.arraycopy(MediaType, 0, bytes, i[0], MediaType.length); i[0] += MediaType.length;
                bytes[i[0]++] = (byte)MediaDesc.length;
                Utils.arraycopy(MediaDesc, 0, bytes, i[0], MediaDesc.length); i[0] += MediaDesc.length;
                Utils.intToBytesLit(MediaWidth, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(MediaHeight, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = MediaLoop;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += DataBlock.getLength();
                length += DataBlockExtended.getLength();
                return length;
            }
        }
        public DataBlockBlock DataBlock;
        public DataBlockExtendedBlock DataBlockExtended;

        public ParcelMediaUpdatePacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.ParcelMediaUpdate;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 420;
            header.Reliable = true;
            DataBlock = new DataBlockBlock();
            DataBlockExtended = new DataBlockExtendedBlock();
        }

        public ParcelMediaUpdatePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            DataBlock.FromBytes(bytes, i);
            DataBlockExtended.FromBytes(bytes, i);
        }

        public ParcelMediaUpdatePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            DataBlock.FromBytes(bytes, i);
            DataBlockExtended.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += DataBlock.getLength();
            length += DataBlockExtended.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[1];
            i[0] = 0;
            header.ToBytes(bytes, i);
            DataBlock.ToBytes(bytes, i);
            DataBlockExtended.ToBytes(bytes, i);
            if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
            return bytes;
        }

        @Override
			public byte[][] ToBytesMultiple()
        {
            return new byte[][] { ToBytes() };
        }
    }