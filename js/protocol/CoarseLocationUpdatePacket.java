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


    public final class CoarseLocationUpdatePacket extends Packet
    {
        /// <exclude/>
        public static final class LocationBlock extends PacketBlock
        {
            public byte X;
		/** Unsigned Byte */ 
		public byte Y;
		/** Unsigned Byte */ 
		public byte Z;

            @Override
			public int getLength()
            {
                {
                    return 3;
                }
            }

            public LocationBlock() { }
            public LocationBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    X = (byte)bytes[i[0]++];
                    Y = (byte)bytes[i[0]++];
                    Z = (byte)bytes[i[0]++];
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = X;
                bytes[i[0]++] = Y;
                bytes[i[0]++] = Z;
            }

        }

        /// <exclude/>
        public static final class IndexBlock extends PacketBlock
        {
            public short You;
            public short Prey;

            @Override
			public int getLength()
            {
                                {
                    return 4;
                }
            }

            public IndexBlock() { }
            public IndexBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    You = (short) Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Prey = (short)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)(You % 256);
                bytes[i[0]++] = (byte)((You >> 8) % 256);
                bytes[i[0]++] = (byte)(Prey % 256);
                bytes[i[0]++] = (byte)((Prey >> 8) % 256);
            }

        }

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

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                for (int j = 0; j < Location.length; j++)
                    length += Location[j].getLength();
                length += Index.getLength();
                for (int j = 0; j < AgentData.length; j++)
                    length += AgentData[j].getLength();
                return length;
            }
        }
        public LocationBlock[] Location;
        public IndexBlock Index;
        public AgentDataBlock[] AgentData;

        public CoarseLocationUpdatePacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.CoarseLocationUpdate;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Medium;
            header.ID = 6;
            header.Reliable = true;
            Location = null;
            Index = new IndexBlock();
            AgentData = null;
        }

        public CoarseLocationUpdatePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            if(Location == null || Location.length != -1) {
                Location = new LocationBlock[count];
                for(int j = 0; j < count; j++)
                { Location[j] = new LocationBlock(); }
            }
            for (int j = 0; j < count; j++)
            { Location[j].FromBytes(bytes, i); }
            Index.FromBytes(bytes, i);
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(AgentData == null || AgentData.length != -1) {
                AgentData = new AgentDataBlock[count];
                for(int j = 0; j < count; j++)
                { AgentData[j] = new AgentDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { AgentData[j].FromBytes(bytes, i); }
        }

        public CoarseLocationUpdatePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            if(Location == null || Location.length != count) {
                Location = new LocationBlock[count];
                for(int j = 0; j < count; j++)
                { Location[j] = new LocationBlock(); }
            }
            for (int j = 0; j < count; j++)
            { Location[j].FromBytes(bytes, i); }
            Index.FromBytes(bytes, i);
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(AgentData == null || AgentData.length != count) {
                AgentData = new AgentDataBlock[count];
                for(int j = 0; j < count; j++)
                { AgentData[j] = new AgentDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { AgentData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 8;
            length += Index.getLength();
            length++;
            for (int j = 0; j < Location.length; j++) { length += Location[j].getLength(); }
            length++;
            for (int j = 0; j < AgentData.length; j++) { length += AgentData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)Location.length;
            for (int j = 0; j < Location.length; j++) { Location[j].ToBytes(bytes, i); }
            Index.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)AgentData.length;
            for (int j = 0; j < AgentData.length; j++) { AgentData[j].ToBytes(bytes, i); }
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
