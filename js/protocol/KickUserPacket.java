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
	public final class KickUserPacket extends Packet
    {
        /// <exclude/>
        public static final class TargetBlockBlock extends PacketBlock
        {
            public long TargetIP;
            /**
             * Unsigned Short
             * 
             */
            public int TargetPort;

            @Override
			public int getLength()
            {
                                {
                    return 6;
                }
            }

            public TargetBlockBlock() { }
            public TargetBlockBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    TargetIP = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    TargetPort = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(TargetIP, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((TargetPort >> 8) % 256);
                bytes[i[0]++] = (byte)(TargetPort % 256);
            }

        }

        /// <exclude/>
        public static final class UserInfoBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID SessionID = new UUID();
		/** Unsigned Byte */ 
		public byte[] Reason;

            @Override
			public int getLength()
            {
                                {
                    int length = 34;
                    if (Reason != null) { length += Reason.length; }
                    return length;
                }
            }

            public UserInfoBlock() { }
            public UserInfoBlock(byte[] bytes, int[] i) throws MalformedDataException
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
                    SessionID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Reason = new byte[length];
                    Utils.arraycopy(bytes, i[0], Reason, 0, length); i[0] +=  length;
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
                bytes[i[0]++] = (byte)(Reason.length % 256);
                bytes[i[0]++] = (byte)((Reason.length >> 8) % 256);
                Utils.arraycopy(Reason, 0, bytes, i[0], Reason.length); i[0] +=  Reason.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += TargetBlock.getLength();
                length += UserInfo.getLength();
                return length;
            }
        }
        public TargetBlockBlock TargetBlock;
        public UserInfoBlock UserInfo;

        public KickUserPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.KickUser;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 163;
            header.Reliable = true;
            TargetBlock = new TargetBlockBlock();
            UserInfo = new UserInfoBlock();
        }

        public KickUserPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            TargetBlock.FromBytes(bytes, i);
            UserInfo.FromBytes(bytes, i);
        }

        public KickUserPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            TargetBlock.FromBytes(bytes, i);
            UserInfo.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += TargetBlock.getLength();
            length += UserInfo.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            TargetBlock.ToBytes(bytes, i);
            UserInfo.ToBytes(bytes, i);
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
