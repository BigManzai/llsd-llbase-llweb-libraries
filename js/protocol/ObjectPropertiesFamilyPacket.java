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

import java.util.ArrayList;
import java.util.List;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;
	public final class ObjectPropertiesFamilyPacket extends Packet
    {
        /// <exclude/>
        public static final class ObjectDataBlock extends PacketBlock
        {
            public long RequestFlags;
            public UUID ObjectID = new UUID();
            public UUID OwnerID = new UUID();
            public UUID GroupID = new UUID();
            public long BaseMask;
            public long OwnerMask;
            public long GroupMask;
            public long EveryoneMask;
            public long NextOwnerMask;
            public int OwnershipCost;
		/** Unsigned Byte */ 
		public byte SaleType;
            public int SalePrice;
            public long Category;
            public UUID LastOwnerID = new UUID();
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Description;

            @Override
			public int getLength()
            {
                                {
                    int length = 103;
                    if (Name != null) { length += Name.length; }
                    if (Description != null) { length += Description.length; }
                    return length;
                }
            }

            public ObjectDataBlock() { }
            public ObjectDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    RequestFlags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    ObjectID.FromBytes(bytes, i[0]); i[0] += 16;
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    BaseMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    OwnerMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    GroupMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    EveryoneMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    NextOwnerMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    OwnershipCost = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SaleType = (byte)bytes[i[0]++];
                    SalePrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    Category = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    LastOwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Description = new byte[length];
                    Utils.arraycopy(bytes, i[0], Description, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(RequestFlags, bytes, i[0]); i[0] += 4;
                ObjectID.ToBytes(bytes, i[0]); i[0] += 16;
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.uintToBytesLit(BaseMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(OwnerMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(GroupMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(EveryoneMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(NextOwnerMask, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(OwnershipCost, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = SaleType;
                Utils.intToBytesLit(SalePrice, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(Category, bytes, i[0]); i[0] += 4;
                LastOwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)Description.length;
                Utils.arraycopy(Description, 0, bytes, i[0], Description.length); i[0] +=  Description.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 8;
                length += ObjectData.getLength();
                return length;
            }
        }
        public ObjectDataBlock ObjectData;

        public ObjectPropertiesFamilyPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.ObjectPropertiesFamily;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Medium;
            header.ID = 10;
            header.Reliable = true;
            header.Zerocoded = true;
            ObjectData = new ObjectDataBlock();
        }

        public ObjectPropertiesFamilyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            ObjectData.FromBytes(bytes, i);
        }

        public ObjectPropertiesFamilyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            ObjectData.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 8;
            length += ObjectData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            ObjectData.ToBytes(bytes, i);
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
