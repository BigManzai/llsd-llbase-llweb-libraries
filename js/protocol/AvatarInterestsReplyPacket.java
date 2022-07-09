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


    public final class AvatarInterestsReplyPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID AvatarID = new UUID();

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
                    AvatarID.FromBytes(bytes, i[0]); i[0] += 16;
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
                AvatarID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class PropertiesDataBlock extends PacketBlock
        {
            public long WantToMask;
		/** Unsigned Byte */ 
		public byte[] WantToText;
            public long SkillsMask;
		/** Unsigned Byte */ 
		public byte[] SkillsText;
		/** Unsigned Byte */ 
		public byte[] LanguagesText;

            @Override
			public int getLength()
            {
                                {
                    int length = 11;
                    if (WantToText != null) { length += WantToText.length; }
                    if (SkillsText != null) { length += SkillsText.length; }
                    if (LanguagesText != null) { length += LanguagesText.length; }
                    return length;
                }
            }

            public PropertiesDataBlock() { }
            public PropertiesDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    WantToMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    WantToText = new byte[length];
                    Utils.arraycopy(bytes, i[0], WantToText, 0, length); i[0] +=  length;
                    SkillsMask = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    SkillsText = new byte[length];
                    Utils.arraycopy(bytes, i[0], SkillsText, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    LanguagesText = new byte[length];
                    Utils.arraycopy(bytes, i[0], LanguagesText, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(WantToMask, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)WantToText.length;
                Utils.arraycopy(WantToText, 0, bytes, i[0], WantToText.length); i[0] +=  WantToText.length;
                Utils.uintToBytesLit(SkillsMask, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)SkillsText.length;
                Utils.arraycopy(SkillsText, 0, bytes, i[0], SkillsText.length); i[0] +=  SkillsText.length;
                bytes[i[0]++] = (byte)LanguagesText.length;
                Utils.arraycopy(LanguagesText, 0, bytes, i[0], LanguagesText.length); i[0] +=  LanguagesText.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                length += PropertiesData.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public PropertiesDataBlock PropertiesData;

        public AvatarInterestsReplyPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.AvatarInterestsReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 172;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            PropertiesData = new PropertiesDataBlock();
        }

        public AvatarInterestsReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            PropertiesData.FromBytes(bytes, i);
        }

        public AvatarInterestsReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            PropertiesData.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += PropertiesData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            PropertiesData.ToBytes(bytes, i);
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
