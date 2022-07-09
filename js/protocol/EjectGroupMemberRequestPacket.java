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


    public final class EjectGroupMemberRequestPacket extends Packet
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
        public static final class GroupDataBlock extends PacketBlock
        {
            public UUID GroupID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public GroupDataBlock() { }
            public GroupDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class EjectDataBlock extends PacketBlock
        {
            public UUID EjecteeID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public EjectDataBlock() { }
            public EjectDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    EjecteeID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                EjecteeID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += AgentData.getLength();
                length += GroupData.getLength();
                for (int j = 0; j < EjectData.length; j++)
                    length += EjectData[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public GroupDataBlock GroupData;
        public EjectDataBlock[] EjectData;

        public EjectGroupMemberRequestPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.EjectGroupMemberRequest;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 345;
            header.Reliable = true;
            AgentData = new AgentDataBlock();
            GroupData = new GroupDataBlock();
            EjectData = null;
        }

        public EjectGroupMemberRequestPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            GroupData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(EjectData == null || EjectData.length != -1) {
                EjectData = new EjectDataBlock[count];
                for(int j = 0; j < count; j++)
                { EjectData[j] = new EjectDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { EjectData[j].FromBytes(bytes, i); }
        }

        public EjectGroupMemberRequestPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            GroupData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(EjectData == null || EjectData.length != count) {
                EjectData = new EjectDataBlock[count];
                for(int j = 0; j < count; j++)
                { EjectData[j] = new EjectDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { EjectData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += GroupData.getLength();
            length++;
            for (int j = 0; j < EjectData.length; j++) { length += EjectData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            GroupData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)EjectData.length;
            for (int j = 0; j < EjectData.length; j++) { EjectData[j].ToBytes(bytes, i); }
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
            fixedLength += GroupData.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            AgentData.ToBytes(fixedBytes, i);
            GroupData.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int EjectDataStart = 0;
            do
            {
                int variableLength = 0;
                int EjectDataCount = 0;

              i[0] =EjectDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < EjectData.length) {
                    int blockLength = EjectData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++EjectDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)EjectDataCount;
                for (i[0] = EjectDataStart; i[0] < EjectDataStart + EjectDataCount; i[0]++) { EjectData[i[0]].ToBytes(packet, length); }
                EjectDataStart += EjectDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                EjectDataStart < EjectData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
