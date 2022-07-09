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

    public final class GodUpdateRegionInfoPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID SessionID = new UUID();

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
                    SessionID.FromBytes(bytes, i[0]); i[0] += 16;
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
                SessionID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class RegionInfoBlock extends PacketBlock
        {
            public byte[] SimName;
            public long EstateID;
            public long ParentEstateID;
            public long RegionFlags;
            public float BillableFactor;
            public int PricePerMeter;
            public int RedirectGridX;
            public int RedirectGridY;

            @Override
			public int getLength()
            {
                                {
                    int length = 29;
                    if (SimName != null) { length += SimName.length; }
                    return length;
                }
            }

            public RegionInfoBlock() { }
            public RegionInfoBlock(byte[] bytes, int[] i) throws MalformedDataException
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
                    SimName = new byte[length];
                    Utils.arraycopy(bytes, i[0], SimName, 0, length); i[0] +=  length;
                    EstateID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    ParentEstateID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    RegionFlags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    BillableFactor = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    PricePerMeter = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    RedirectGridX = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    RedirectGridY = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)SimName.length;
                Utils.arraycopy(SimName, 0, bytes, i[0], SimName.length); i[0] +=  SimName.length;
                Utils.uintToBytesLit(EstateID, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(ParentEstateID, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(RegionFlags, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(BillableFactor, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PricePerMeter, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(RedirectGridX, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(RedirectGridY, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                length += RegionInfo.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public RegionInfoBlock RegionInfo;

        public GodUpdateRegionInfoPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.GodUpdateRegionInfo;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 143;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            RegionInfo = new RegionInfoBlock();
        }

        public GodUpdateRegionInfoPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            RegionInfo.FromBytes(bytes, i);
        }

        public GodUpdateRegionInfoPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            RegionInfo.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += RegionInfo.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            RegionInfo.ToBytes(bytes, i);
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
