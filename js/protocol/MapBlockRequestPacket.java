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
	public final class MapBlockRequestPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID SessionID = new UUID();
            public long Flags;
            public long EstateID;
            public boolean Godlike;

            @Override
			public int getLength()
            {
                                {
                    return 41;
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
                    Flags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    EstateID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    Godlike = (bytes[i[0]++] != 0) ? true : false;
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
                Utils.uintToBytesLit(Flags, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(EstateID, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((Godlike) ? 1 : 0);
            }

        }

        /// <exclude/>
        public static final class PositionDataBlock extends PacketBlock
        {
        	/**
        	 * Unsigned Short
        	 * 
        	 */
            public int MinX;
        	/**
        	 * Unsigned Short
        	 * 
        	 */            
            public int MaxX;
        	/**
        	 * Unsigned Short
        	 * 
        	 */
            public int MinY;
        	/**
        	 * Unsigned Short
        	 * 
        	 */
            public int MaxY;

            @Override
			public int getLength()
            {
                                {
                    return 8;
                }
            }

            public PositionDataBlock() { }
            public PositionDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    MinX = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    MaxX = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    MinY = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    MaxY = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)(MinX % 256);
                bytes[i[0]++] = (byte)((MinX >> 8) % 256);
                bytes[i[0]++] = (byte)(MaxX % 256);
                bytes[i[0]++] = (byte)((MaxX >> 8) % 256);
                bytes[i[0]++] = (byte)(MinY % 256);
                bytes[i[0]++] = (byte)((MinY >> 8) % 256);
                bytes[i[0]++] = (byte)(MaxY % 256);
                bytes[i[0]++] = (byte)((MaxY >> 8) % 256);
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                length += PositionData.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public PositionDataBlock PositionData;

        public MapBlockRequestPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.MapBlockRequest;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 407;
            header.Reliable = true;
            AgentData = new AgentDataBlock();
            PositionData = new PositionDataBlock();
        }

        public MapBlockRequestPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            PositionData.FromBytes(bytes, i);
        }

        public MapBlockRequestPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            PositionData.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += PositionData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            PositionData.ToBytes(bytes, i);
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
