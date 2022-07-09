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
	public final class GroupMembersReplyPacket extends Packet
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
        public static final class GroupDataBlock extends PacketBlock
        {
            public UUID GroupID = new UUID();
            public UUID RequestID = new UUID();
            public int MemberCount;

            @Override
			public int getLength()
            {
                                {
                    return 36;
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
                    RequestID.FromBytes(bytes, i[0]); i[0] += 16;
                    MemberCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
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
                RequestID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.intToBytesLit(MemberCount, bytes, i[0]); i[0] += 4;
            }

        }

        /// <exclude/>
        public static final class MemberDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public int Contribution;
		/** Unsigned Byte */ 
		public byte[] OnlineStatus;
            public BigInteger AgentPowers;
		/** Unsigned Byte */ 
		public byte[] Title;
            public boolean IsOwner;

            @Override
			public int getLength()
            {
                                {
                    int length = 31;
                    if (OnlineStatus != null) { length += OnlineStatus.length; }
                    if (Title != null) { length += Title.length; }
                    return length;
                }
            }

            public MemberDataBlock() { }
            public MemberDataBlock(byte[] bytes, int[] i) throws MalformedDataException
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
                    Contribution = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    OnlineStatus = new byte[length];
                    Utils.arraycopy(bytes, i[0], OnlineStatus, 0, length); i[0] +=  length;
                    AgentPowers = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Title = new byte[length];
                    Utils.arraycopy(bytes, i[0], Title, 0, length); i[0] +=  length;
                    IsOwner = (bytes[i[0]++] != 0) ? true : false;
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
                Utils.intToBytesLit(Contribution, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)OnlineStatus.length;
                Utils.arraycopy(OnlineStatus, 0, bytes, i[0], OnlineStatus.length); i[0] +=  OnlineStatus.length;
                Utils.ulongToBytesLit(AgentPowers, bytes, i[0]); i[0] += 8;
                bytes[i[0]++] = (byte)Title.length;
                Utils.arraycopy(Title, 0, bytes, i[0], Title.length); i[0] +=  Title.length;
                bytes[i[0]++] = (byte)((IsOwner) ? 1 : 0);
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += AgentData.getLength();
                length += GroupData.getLength();
                for (int j = 0; j < MemberData.length; j++)
                    length += MemberData[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public GroupDataBlock GroupData;
        public MemberDataBlock[] MemberData;

        public GroupMembersReplyPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.GroupMembersReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 367;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            GroupData = new GroupDataBlock();
            MemberData = null;
        }

        public GroupMembersReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            if(MemberData == null || MemberData.length != -1) {
                MemberData = new MemberDataBlock[count];
                for(int j = 0; j < count; j++)
                { MemberData[j] = new MemberDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { MemberData[j].FromBytes(bytes, i); }
        }

        public GroupMembersReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            if(MemberData == null || MemberData.length != count) {
                MemberData = new MemberDataBlock[count];
                for(int j = 0; j < count; j++)
                { MemberData[j] = new MemberDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { MemberData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += GroupData.getLength();
            length++;
            for (int j = 0; j < MemberData.length; j++) { length += MemberData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            GroupData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)MemberData.length;
            for (int j = 0; j < MemberData.length; j++) { MemberData[j].ToBytes(bytes, i); }
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

            int MemberDataStart = 0;
            do
            {
                int variableLength = 0;
                int MemberDataCount = 0;

              i[0] =MemberDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < MemberData.length) {
                    int blockLength = MemberData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++MemberDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)MemberDataCount;
                for (i[0] = MemberDataStart; i[0] < MemberDataStart + MemberDataCount; i[0]++) { MemberData[i[0]].ToBytes(packet, length); }
                MemberDataStart += MemberDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                MemberDataStart < MemberData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
