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
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;


    public final class ChatFromSimulatorPacket extends Packet
    {
        /// <exclude/>
        public static final class ChatDataBlock extends PacketBlock
        {
            public byte[] FromName;
            public UUID SourceID = new UUID();
            public UUID OwnerID = new UUID();
		/** Unsigned Byte */ 
		public byte SourceType;
		/** Unsigned Byte */ 
		public byte ChatType;
		/** Unsigned Byte */ 
		public byte Audible;
            public Vector3 Position = new Vector3();
		/** Unsigned Byte */ 
		public byte[] Message;

            @Override
			public int getLength()
            {
                                {
                    int length = 50;
                    if (FromName != null) { length += FromName.length; }
                    if (Message != null) { length += Message.length; }
                    return length;
                }
            }

            public ChatDataBlock() { }
            public ChatDataBlock(byte[] bytes, int[] i) throws MalformedDataException
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
                    FromName = new byte[length];
                    Utils.arraycopy(bytes, i[0], FromName, 0, length); i[0] +=  length;
                    SourceID.FromBytes(bytes, i[0]); i[0] += 16;
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    SourceType = (byte)bytes[i[0]++];
                    ChatType = (byte)bytes[i[0]++];
                    Audible = (byte)bytes[i[0]++];
                    Position.fromBytesLit(bytes, i[0]); i[0] += 12;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Message = new byte[length];
                    Utils.arraycopy(bytes, i[0], Message, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)FromName.length;
                Utils.arraycopy(FromName, 0, bytes, i[0], FromName.length); i[0] +=  FromName.length;
                SourceID.ToBytes(bytes, i[0]); i[0] += 16;
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = SourceType;
                bytes[i[0]++] = ChatType;
                bytes[i[0]++] = Audible;
                Position.toBytesLit(bytes, i[0]); i[0] += 12;
                bytes[i[0]++] = (byte)(Message.length % 256);
                bytes[i[0]++] = (byte)((Message.length >> 8) % 256);
                Utils.arraycopy(Message, 0, bytes, i[0], Message.length); i[0] +=  Message.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += ChatData.getLength();
                return length;
            }
        }
        public ChatDataBlock ChatData;

        public ChatFromSimulatorPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.ChatFromSimulator;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 139;
            header.Reliable = true;
            ChatData = new ChatDataBlock();
        }

        public ChatFromSimulatorPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            ChatData.FromBytes(bytes, i);
        }

        public ChatFromSimulatorPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            ChatData.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += ChatData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            ChatData.ToBytes(bytes, i);
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
