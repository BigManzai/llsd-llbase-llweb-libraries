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


    public final class AssetUploadRequestPacket extends Packet
    {
        /// <exclude/>
        public static final class AssetBlockBlock extends PacketBlock
        {
            public UUID TransactionID = new UUID();
		/** Signed Byte */ 
		public byte Type;
            public boolean Tempfile;
            public boolean StoreLocal;
		/** Unsigned Byte */ 
		public byte[] AssetData;

            @Override
			public int getLength()
            {
                                {
                    int length = 21;
                    if (AssetData != null) { length += AssetData.length; }
                    return length;
                }
            }

            public AssetBlockBlock() { }
            public AssetBlockBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    TransactionID.FromBytes(bytes, i[0]); i[0] += 16;
                    Type = (byte)bytes[i[0]++];
                    Tempfile = (bytes[i[0]++] != 0) ? true : false;
                    StoreLocal = (bytes[i[0]++] != 0) ? true : false;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    AssetData = new byte[length];
                    Utils.arraycopy(bytes, i[0], AssetData, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                TransactionID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)Type;
                bytes[i[0]++] = (byte)((Tempfile) ? 1 : 0);
                bytes[i[0]++] = (byte)((StoreLocal) ? 1 : 0);
                bytes[i[0]++] = (byte)(AssetData.length % 256);
                bytes[i[0]++] = (byte)((AssetData.length >> 8) % 256);
                Utils.arraycopy(AssetData, 0, bytes, i[0], AssetData.length); i[0] +=  AssetData.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AssetBlock.getLength();
                return length;
            }
        }
        public AssetBlockBlock AssetBlock;

        public AssetUploadRequestPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.AssetUploadRequest;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 333;
            header.Reliable = true;
            AssetBlock = new AssetBlockBlock();
        }

        public AssetUploadRequestPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            AssetBlock.FromBytes(bytes, i);
        }

        public AssetUploadRequestPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            AssetBlock.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AssetBlock.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AssetBlock.ToBytes(bytes, i);
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
