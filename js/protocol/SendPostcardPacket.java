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
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.util.Utils;
	public final class SendPostcardPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID SessionID = new UUID();
            public UUID AssetID = new UUID();
            public Vector3d PosGlobal = new Vector3d();
		/** Unsigned Byte */ 
		public byte[] To;
		/** Unsigned Byte */ 
		public byte[] From;
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Subject;
		/** Unsigned Byte */ 
		public byte[] Msg;
            public boolean AllowPublish;
            public boolean MaturePublish;

            @Override
			public int getLength()
            {
                                {
                    int length = 80;
                    if (To != null) { length += To.length; }
                    if (From != null) { length += From.length; }
                    if (Name != null) { length += Name.length; }
                    if (Subject != null) { length += Subject.length; }
                    if (Msg != null) { length += Msg.length; }
                    return length;
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
                int length;
                try
                {
                    AgentID.FromBytes(bytes, i[0]); i[0] += 16;
                    SessionID.FromBytes(bytes, i[0]); i[0] += 16;
                    AssetID.FromBytes(bytes, i[0]); i[0] += 16;
                    PosGlobal.fromBytesLit(bytes, i[0]); i[0] += 24;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    To = new byte[length];
                    Utils.arraycopy(bytes, i[0], To, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    From = new byte[length];
                    Utils.arraycopy(bytes, i[0], From, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Subject = new byte[length];
                    Utils.arraycopy(bytes, i[0], Subject, 0, length); i[0] +=  length;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Msg = new byte[length];
                    Utils.arraycopy(bytes, i[0], Msg, 0, length); i[0] +=  length;
                    AllowPublish = (bytes[i[0]++] != 0) ? true : false;
                    MaturePublish = (bytes[i[0]++] != 0) ? true : false;
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
                AssetID.ToBytes(bytes, i[0]); i[0] += 16;
                PosGlobal.toBytesLit(bytes, i[0]); i[0] += 24;
                bytes[i[0]++] = (byte)To.length;
                Utils.arraycopy(To, 0, bytes, i[0], To.length); i[0] +=  To.length;
                bytes[i[0]++] = (byte)From.length;
                Utils.arraycopy(From, 0, bytes, i[0], From.length); i[0] +=  From.length;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)Subject.length;
                Utils.arraycopy(Subject, 0, bytes, i[0], Subject.length); i[0] +=  Subject.length;
                bytes[i[0]++] = (byte)(Msg.length % 256);
                bytes[i[0]++] = (byte)((Msg.length >> 8) % 256);
                Utils.arraycopy(Msg, 0, bytes, i[0], Msg.length); i[0] +=  Msg.length;
                bytes[i[0]++] = (byte)((AllowPublish) ? 1 : 0);
                bytes[i[0]++] = (byte)((MaturePublish) ? 1 : 0);
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;

        public SendPostcardPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.SendPostcard;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 412;
            header.Reliable = true;
            AgentData = new AgentDataBlock();
        }

        public SendPostcardPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
        }

        public SendPostcardPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
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
