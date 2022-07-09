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
import java.util.ArrayList;
import java.util.List;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;
	public final class ObjectPropertiesPacket extends Packet
    {
        /// <exclude/>
        public static final class ObjectDataBlock extends PacketBlock
        {
            public UUID ObjectID = new UUID();
            public UUID CreatorID = new UUID();
            public UUID OwnerID = new UUID();
            public UUID GroupID = new UUID();
            public BigInteger CreationDate;
            public long BaseMask;
            public long OwnerMask;
            public long GroupMask;
            public long EveryoneMask;
            public long NextOwnerMask;
            public int OwnershipCost;
		/** Unsigned Byte */ 
		public byte SaleType;
            public int SalePrice;
		/** Unsigned Byte */ 
		public byte AggregatePerms;
		/** Unsigned Byte */ 
		public byte AggregatePermTextures;
		/** Unsigned Byte */ 
		public byte AggregatePermTexturesOwner;
            public long Category;
            public short InventorySerial;
            public UUID ItemID = new UUID();
            public UUID FolderID = new UUID();
            public UUID FromTaskID = new UUID();
            public UUID LastOwnerID = new UUID();
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Description;
		/** Unsigned Byte */ 
		public byte[] TouchName;
		/** Unsigned Byte */ 
		public byte[] SitName;
		/** Unsigned Byte */ 
		public byte[] TextureID;

            @Override
			public int getLength()
            {
                                {
                    int length = 179;
                    if (Name != null) { length += Name.length; }
                    if (Description != null) { length += Description.length; }
                    if (TouchName != null) { length += TouchName.length; }
                    if (SitName != null) { length += SitName.length; }
                    if (TextureID != null) { length += TextureID.length; }
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
                    ObjectID.FromBytes(bytes, i[0]); i[0] += 16;
                    CreatorID.FromBytes(bytes, i[0]); i[0] += 16;
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    CreationDate = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    BaseMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    OwnerMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    GroupMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    EveryoneMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    NextOwnerMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    OwnershipCost = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SaleType = (byte)bytes[i[0]++];
                    SalePrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    AggregatePerms = (byte)bytes[i[0]++];
                    AggregatePermTextures = (byte)bytes[i[0]++];
                    AggregatePermTexturesOwner = (byte)bytes[i[0]++];
                    Category = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    InventorySerial = (short)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    ItemID.FromBytes(bytes, i[0]); i[0] += 16;
                    FolderID.FromBytes(bytes, i[0]); i[0] += 16;
                    FromTaskID.FromBytes(bytes, i[0]); i[0] += 16;
                    LastOwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Description = new byte[length];
                    Utils.arraycopy(bytes, i[0], Description, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    TouchName = new byte[length];
                    Utils.arraycopy(bytes, i[0], TouchName, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    SitName = new byte[length];
                    Utils.arraycopy(bytes, i[0], SitName, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    TextureID = new byte[length];
                    Utils.arraycopy(bytes, i[0], TextureID, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                ObjectID.ToBytes(bytes, i[0]); i[0] += 16;
                CreatorID.ToBytes(bytes, i[0]); i[0] += 16;
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.ulongToBytesLit(CreationDate, bytes, i[0]); i[0] += 8;
                Utils.uintToBytesLit(BaseMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(OwnerMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(GroupMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(EveryoneMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(NextOwnerMask, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(OwnershipCost, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = SaleType;
                Utils.intToBytesLit(SalePrice, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = AggregatePerms;
                bytes[i[0]++] = AggregatePermTextures;
                bytes[i[0]++] = AggregatePermTexturesOwner;
                Utils.uintToBytesLit(Category, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)(InventorySerial % 256);
                bytes[i[0]++] = (byte)((InventorySerial >> 8) % 256);
                ItemID.ToBytes(bytes, i[0]); i[0] += 16;
                FolderID.ToBytes(bytes, i[0]); i[0] += 16;
                FromTaskID.ToBytes(bytes, i[0]); i[0] += 16;
                LastOwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)Description.length;
                Utils.arraycopy(Description, 0, bytes, i[0], Description.length); i[0] +=  Description.length;
                bytes[i[0]++] = (byte)TouchName.length;
                Utils.arraycopy(TouchName, 0, bytes, i[0], TouchName.length); i[0] +=  TouchName.length;
                bytes[i[0]++] = (byte)SitName.length;
                Utils.arraycopy(SitName, 0, bytes, i[0], SitName.length); i[0] +=  SitName.length;
                bytes[i[0]++] = (byte)TextureID.length;
                Utils.arraycopy(TextureID, 0, bytes, i[0], TextureID.length); i[0] +=  TextureID.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 9;
                for (int j = 0; j < ObjectData.length; j++)
                    length += ObjectData[j].getLength();
                return length;
            }
        }
        public ObjectDataBlock[] ObjectData;

        public ObjectPropertiesPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.ObjectProperties;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Medium;
            header.ID = 9;
            header.Reliable = true;
            header.Zerocoded = true;
            ObjectData = null;
        }

        public ObjectPropertiesPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(ObjectData == null || ObjectData.length != -1) {
                ObjectData = new ObjectDataBlock[count];
                for(int j = 0; j < count; j++)
                { ObjectData[j] = new ObjectDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { ObjectData[j].FromBytes(bytes, i); }
        }

        public ObjectPropertiesPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(ObjectData == null || ObjectData.length != count) {
                ObjectData = new ObjectDataBlock[count];
                for(int j = 0; j < count; j++)
                { ObjectData[j] = new ObjectDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { ObjectData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 8;
            length++;
            for (int j = 0; j < ObjectData.length; j++) { length += ObjectData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)ObjectData.length;
            for (int j = 0; j < ObjectData.length; j++) { ObjectData[j].ToBytes(bytes, i); }
            if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
            return bytes;
        }

        @Override
			public byte[][] ToBytesMultiple()
        {
            List<byte[]> packets = new ArrayList<byte[]>();
            int[] i = new int[]{0};
            int fixedLength = 8;

            byte[] ackBytes = null;
            int[] acksLength = new int[]{0};
            if (header.AckList != null && header.AckList.length > 0) {
                header.AppendedAcks = true;
                ackBytes = new byte[header.AckList.length * 4 + 1];
                header.AcksToBytes(ackBytes, acksLength);
            }

            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int ObjectDataStart = 0;
            do
            {
                int variableLength = 0;
                int ObjectDataCount = 0;

              i[0] =ObjectDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < ObjectData.length) {
                    int blockLength = ObjectData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++ObjectDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)ObjectDataCount;
                for (i[0] = ObjectDataStart; i[0] < ObjectDataStart + ObjectDataCount; i[0]++) { ObjectData[i[0]].ToBytes(packet, length); }
                ObjectDataStart += ObjectDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                ObjectDataStart < ObjectData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
