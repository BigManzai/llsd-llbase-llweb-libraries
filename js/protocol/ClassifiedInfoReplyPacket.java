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
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.util.Utils;


    public final class ClassifiedInfoReplyPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
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
            }

        }

        /// <exclude/>
        public static final class DataBlock extends PacketBlock
        {
            public UUID ClassifiedID = new UUID();
            public UUID CreatorID = new UUID();
            public long CreationDate;
            public long ExpirationDate;
            public long Category;
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Desc;
            public UUID ParcelID = new UUID();
            public long ParentEstate;
            public UUID SnapshotID = new UUID();
		/** Unsigned Byte */ 
		public byte[] SimName;
            public Vector3d PosGlobal = new Vector3d();
		/** Unsigned Byte */ 
		public byte[] ParcelName;
		/** Unsigned Byte */ 
		public byte ClassifiedFlags;
            public int PriceForListing;

            @Override
			public int getLength()
            {
                                {
                    int length = 114;
                    if (Name != null) { length += Name.length; }
                    if (Desc != null) { length += Desc.length; }
                    if (SimName != null) { length += SimName.length; }
                    if (ParcelName != null) { length += ParcelName.length; }
                    return length;
                }
            }

            public DataBlock() { }
            public DataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    ClassifiedID.FromBytes(bytes, i[0]); i[0] += 16;
                    CreatorID.FromBytes(bytes, i[0]); i[0] += 16;
                    CreationDate = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    ExpirationDate = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    Category = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Desc = new byte[length];
                    Utils.arraycopy(bytes, i[0], Desc, 0, length); i[0] +=  length;
                    ParcelID.FromBytes(bytes, i[0]); i[0] += 16;
                    ParentEstate = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    SnapshotID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    SimName = new byte[length];
                    Utils.arraycopy(bytes, i[0], SimName, 0, length); i[0] +=  length;
                    PosGlobal.fromBytesLit(bytes, i[0]); i[0] += 24;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    ParcelName = new byte[length];
                    Utils.arraycopy(bytes, i[0], ParcelName, 0, length); i[0] +=  length;
                    ClassifiedFlags = (byte)bytes[i[0]++];
                    PriceForListing = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                ClassifiedID.ToBytes(bytes, i[0]); i[0] += 16;
                CreatorID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.uintToBytesLit(CreationDate, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(ExpirationDate, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(Category, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)(Desc.length % 256);
                bytes[i[0]++] = (byte)((Desc.length >> 8) % 256);
                Utils.arraycopy(Desc, 0, bytes, i[0], Desc.length); i[0] +=  Desc.length;
                ParcelID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.uintToBytesLit(ParentEstate, bytes, i[0]); i[0] += 4;
                SnapshotID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)SimName.length;
                Utils.arraycopy(SimName, 0, bytes, i[0], SimName.length); i[0] +=  SimName.length;
                PosGlobal.toBytesLit(bytes, i[0]); i[0] += 24;
                bytes[i[0]++] = (byte)ParcelName.length;
                Utils.arraycopy(ParcelName, 0, bytes, i[0], ParcelName.length); i[0] +=  ParcelName.length;
                bytes[i[0]++] = ClassifiedFlags;
                Utils.intToBytesLit(PriceForListing, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                length += Data.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public DataBlock Data;

        public ClassifiedInfoReplyPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.ClassifiedInfoReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 44;
            header.Reliable = true;
            AgentData = new AgentDataBlock();
            Data = new DataBlock();
        }

        public ClassifiedInfoReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            Data.FromBytes(bytes, i);
        }

        public ClassifiedInfoReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            Data.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += Data.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            Data.ToBytes(bytes, i);
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
