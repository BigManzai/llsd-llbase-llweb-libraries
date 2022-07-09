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


    public final class BulkUpdateInventoryPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID TransactionID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 32;
                }
            }

            public AgentDataBlock() { }
            public AgentDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    AgentID.FromBytes(bytes, i[0]); i[0] += 16;
                    TransactionID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                AgentID.ToBytes(bytes, i[0]); i[0] += 16;
                TransactionID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class FolderDataBlock extends PacketBlock
        {
            public UUID FolderID = new UUID();
            public UUID ParentID = new UUID();
		/** Signed Byte */ 
		public byte Type;
		/** Unsigned Byte */ 
		public byte[] Name;

            @Override
			public int getLength()
            {
                                {
                    int length = 34;
                    if (Name != null) { length += Name.length; }
                    return length;
                }
            }

            public FolderDataBlock() { }
            public FolderDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    FolderID.FromBytes(bytes, i[0]); i[0] += 16;
                    ParentID.FromBytes(bytes, i[0]); i[0] += 16;
                    Type = (byte)bytes[i[0]++];
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                FolderID.ToBytes(bytes, i[0]); i[0] += 16;
                ParentID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)Type;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
            }

        }

        /// <exclude/>
        public static final class ItemDataBlock extends PacketBlock
        {
            public UUID ItemID = new UUID();
            public long CallbackID;
            public UUID FolderID = new UUID();
            public UUID CreatorID = new UUID();
            public UUID OwnerID = new UUID();
            public UUID GroupID = new UUID();
            public long BaseMask;
            public long OwnerMask;
            public long GroupMask;
            public long EveryoneMask;
            public long NextOwnerMask;
            public boolean GroupOwned;
            public UUID AssetID = new UUID();
		/** Signed Byte */ 
		public byte Type;
		/** Signed Byte */ 
		public byte InvType;
            public long Flags;
		/** Unsigned Byte */ 
		public byte SaleType;
            public int SalePrice;
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Description;
            public int CreationDate;
            public long CRC;

            @Override
			public int getLength()
            {
                                {
                    int length = 142;
                    if (Name != null) { length += Name.length; }
                    if (Description != null) { length += Description.length; }
                    return length;
                }
            }

            public ItemDataBlock() { }
            public ItemDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    ItemID.FromBytes(bytes, i[0]); i[0] += 16;
                    CallbackID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    FolderID.FromBytes(bytes, i[0]); i[0] += 16;
                    CreatorID.FromBytes(bytes, i[0]); i[0] += 16;
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    BaseMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    OwnerMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    GroupMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    EveryoneMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    NextOwnerMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    GroupOwned = (bytes[i[0]++] != 0) ? true : false;
                    AssetID.FromBytes(bytes, i[0]); i[0] += 16;
                    Type = (byte)bytes[i[0]++];
                    InvType = (byte)bytes[i[0]++];
                    Flags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    SaleType = (byte)bytes[i[0]++];
                    SalePrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Description = new byte[length];
                    Utils.arraycopy(bytes, i[0], Description, 0, length); i[0] +=  length;
                    CreationDate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    CRC = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                ItemID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.uintToBytesLit(CallbackID, bytes, i[0]); i[0] += 4;
                FolderID.ToBytes(bytes, i[0]); i[0] += 16;
                CreatorID.ToBytes(bytes, i[0]); i[0] += 16;
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.uintToBytesLit(BaseMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(OwnerMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(GroupMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(EveryoneMask, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(NextOwnerMask, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((GroupOwned) ? 1 : 0);
                AssetID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)Type;
                bytes[i[0]++] = (byte)InvType;
                Utils.uintToBytesLit(Flags, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = SaleType;
                Utils.intToBytesLit(SalePrice, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)Description.length;
                Utils.arraycopy(Description, 0, bytes, i[0], Description.length); i[0] +=  Description.length;
                Utils.intToBytesLit(CreationDate, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(CRC, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 12;
                length += AgentData.getLength();
                for (int j = 0; j < FolderData.length; j++)
                    length += FolderData[j].getLength();
                for (int j = 0; j < ItemData.length; j++)
                    length += ItemData[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public FolderDataBlock[] FolderData;
        public ItemDataBlock[] ItemData;

        public BulkUpdateInventoryPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.BulkUpdateInventory;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 281;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            FolderData = null;
            ItemData = null;
        }

        public BulkUpdateInventoryPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            AgentData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(FolderData == null || FolderData.length != -1) {
                FolderData = new FolderDataBlock[count];
                for(int j = 0; j < count; j++)
                { FolderData[j] = new FolderDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { FolderData[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(ItemData == null || ItemData.length != -1) {
                ItemData = new ItemDataBlock[count];
                for(int j = 0; j < count; j++)
                { ItemData[j] = new ItemDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { ItemData[j].FromBytes(bytes, i); }
        }

        public BulkUpdateInventoryPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            AgentData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(FolderData == null || FolderData.length != count) {
                FolderData = new FolderDataBlock[count];
                for(int j = 0; j < count; j++)
                { FolderData[j] = new FolderDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { FolderData[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(ItemData == null || ItemData.length != count) {
                ItemData = new ItemDataBlock[count];
                for(int j = 0; j < count; j++)
                { ItemData[j] = new ItemDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { ItemData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length++;
            for (int j = 0; j < FolderData.length; j++) { length += FolderData[j].getLength(); }
            length++;
            for (int j = 0; j < ItemData.length; j++) { length += ItemData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)FolderData.length;
            for (int j = 0; j < FolderData.length; j++) { FolderData[j].ToBytes(bytes, i); }
            bytes[i[0]++] = (byte)ItemData.length;
            for (int j = 0; j < ItemData.length; j++) { ItemData[j].ToBytes(bytes, i); }
            if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
            return bytes;
        }

        @Override
			public byte[][] ToBytesMultiple()
        {
            List<byte[]> packets = new ArrayList<byte[]>();
            int[] i = new int[]{0};
            int fixedLength = 10;

            byte[] ackBytes = null;
            int[] acksLength = new int[]{0};
            if (header.AckList != null && header.AckList.length > 0) {
                header.AppendedAcks = true;
                ackBytes = new byte[header.AckList.length * 4 + 1];
                header.AcksToBytes(ackBytes, acksLength);
            }

            fixedLength += AgentData.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            AgentData.ToBytes(fixedBytes, i);
            fixedLength += 2;

            int FolderDataStart = 0;
            int ItemDataStart = 0;
            do
            {
                int variableLength = 0;
                int FolderDataCount = 0;
                int ItemDataCount = 0;

              i[0] =FolderDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < FolderData.length) {
                    int blockLength = FolderData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++FolderDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

              i[0] =ItemDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < ItemData.length) {
                    int blockLength = ItemData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++ItemDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)FolderDataCount;
                for (i[0] = FolderDataStart; i[0] < FolderDataStart + FolderDataCount; i[0]++) { FolderData[i[0]].ToBytes(packet, length); }
                FolderDataStart += FolderDataCount;

                packet[length[0]++] = (byte)ItemDataCount;
                for (i[0] = ItemDataStart; i[0] < ItemDataStart + ItemDataCount; i[0]++) { ItemData[i[0]].ToBytes(packet, length); }
                ItemDataStart += ItemDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                FolderDataStart < FolderData.length ||
                ItemDataStart < ItemData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
