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
	public final class GroupDataUpdatePacket extends Packet
    {
        /// <exclude/>
        public static final class AgentGroupDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID GroupID = new UUID();
            public BigInteger AgentPowers;
		/** Unsigned Byte */ 
		public byte[] GroupTitle;

            @Override
			public int getLength()
            {
                                {
                    int length = 41;
                    if (GroupTitle != null) { length += GroupTitle.length; }
                    return length;
                }
            }

            public AgentGroupDataBlock() { }
            public AgentGroupDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    AgentID.FromBytes(bytes, i[0]); i[0] += 16;
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    AgentPowers = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    GroupTitle = new byte[length];
                    Utils.arraycopy(bytes, i[0], GroupTitle, 0, length); i[0] +=  length;
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
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.ulongToBytesLit(AgentPowers, bytes, i[0]); i[0] += 8;
                bytes[i[0]++] = (byte)GroupTitle.length;
                Utils.arraycopy(GroupTitle, 0, bytes, i[0], GroupTitle.length); i[0] +=  GroupTitle.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                for (int j = 0; j < AgentGroupData.length; j++)
                    length += AgentGroupData[j].getLength();
                return length;
            }
        }
        public AgentGroupDataBlock[] AgentGroupData;

        public GroupDataUpdatePacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.GroupDataUpdate;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 388;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentGroupData = null;
        }

        public GroupDataUpdatePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            if(AgentGroupData == null || AgentGroupData.length != -1) {
                AgentGroupData = new AgentGroupDataBlock[count];
                for(int j = 0; j < count; j++)
                { AgentGroupData[j] = new AgentGroupDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { AgentGroupData[j].FromBytes(bytes, i); }
        }

        public GroupDataUpdatePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            if(AgentGroupData == null || AgentGroupData.length != count) {
                AgentGroupData = new AgentGroupDataBlock[count];
                for(int j = 0; j < count; j++)
                { AgentGroupData[j] = new AgentGroupDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { AgentGroupData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length++;
            for (int j = 0; j < AgentGroupData.length; j++) { length += AgentGroupData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)AgentGroupData.length;
            for (int j = 0; j < AgentGroupData.length; j++) { AgentGroupData[j].ToBytes(bytes, i); }
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

            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int AgentGroupDataStart = 0;
            do
            {
                int variableLength = 0;
                int AgentGroupDataCount = 0;

              i[0] =AgentGroupDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < AgentGroupData.length) {
                    int blockLength = AgentGroupData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++AgentGroupDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)AgentGroupDataCount;
                for (i[0] = AgentGroupDataStart; i[0] < AgentGroupDataStart + AgentGroupDataCount; i[0]++) { AgentGroupData[i[0]].ToBytes(packet, length); }
                AgentGroupDataStart += AgentGroupDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                AgentGroupDataStart < AgentGroupData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
