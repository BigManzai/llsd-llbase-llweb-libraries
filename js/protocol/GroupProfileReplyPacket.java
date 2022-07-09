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
	public final class GroupProfileReplyPacket extends Packet
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
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Charter;
            public boolean ShowInList;
		/** Unsigned Byte */ 
		public byte[] MemberTitle;
            public BigInteger PowersMask;
            public UUID InsigniaID = new UUID();
            public UUID FounderID = new UUID();
            public int MembershipFee;
            public boolean OpenEnrollment;
            public int Money;
            public int GroupMembershipCount;
            public int GroupRolesCount;
            public boolean AllowPublish;
            public boolean MaturePublish;
            public UUID OwnerRole = new UUID();

            @Override
			public int getLength()
            {
                                {
                    int length = 96;
                    if (Name != null) { length += Name.length; }
                    if (Charter != null) { length += Charter.length; }
                    if (MemberTitle != null) { length += MemberTitle.length; }
                    return length;
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
                int length;
                try
                {
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Charter = new byte[length];
                    Utils.arraycopy(bytes, i[0], Charter, 0, length); i[0] +=  length;
                    ShowInList = (bytes[i[0]++] != 0) ? true : false;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    MemberTitle = new byte[length];
                    Utils.arraycopy(bytes, i[0], MemberTitle, 0, length); i[0] +=  length;
                    PowersMask = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    InsigniaID.FromBytes(bytes, i[0]); i[0] += 16;
                    FounderID.FromBytes(bytes, i[0]); i[0] += 16;
                    MembershipFee = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    OpenEnrollment = (bytes[i[0]++] != 0) ? true : false;
                    Money = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    GroupMembershipCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    GroupRolesCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    AllowPublish = (bytes[i[0]++] != 0) ? true : false;
                    MaturePublish = (bytes[i[0]++] != 0) ? true : false;
                    OwnerRole.FromBytes(bytes, i[0]); i[0] += 16;
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
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)(Charter.length % 256);
                bytes[i[0]++] = (byte)((Charter.length >> 8) % 256);
                Utils.arraycopy(Charter, 0, bytes, i[0], Charter.length); i[0] +=  Charter.length;
                bytes[i[0]++] = (byte)((ShowInList) ? 1 : 0);
                bytes[i[0]++] = (byte)MemberTitle.length;
                Utils.arraycopy(MemberTitle, 0, bytes, i[0], MemberTitle.length); i[0] +=  MemberTitle.length;
                Utils.ulongToBytesLit(PowersMask, bytes, i[0]); i[0] += 8;
                InsigniaID.ToBytes(bytes, i[0]); i[0] += 16;
                FounderID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.intToBytesLit(MembershipFee, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((OpenEnrollment) ? 1 : 0);
                Utils.intToBytesLit(Money, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(GroupMembershipCount, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(GroupRolesCount, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((AllowPublish) ? 1 : 0);
                bytes[i[0]++] = (byte)((MaturePublish) ? 1 : 0);
                OwnerRole.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                length += GroupData.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public GroupDataBlock GroupData;

        public GroupProfileReplyPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.GroupProfileReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 352;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            GroupData = new GroupDataBlock();
        }

        public GroupProfileReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
        }

        public GroupProfileReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += GroupData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            GroupData.ToBytes(bytes, i);
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
