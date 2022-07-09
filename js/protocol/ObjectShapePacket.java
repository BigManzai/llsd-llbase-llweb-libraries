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
	public final class ObjectShapePacket extends Packet
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
        public static final class ObjectDataBlock extends PacketBlock
        {
            public long ObjectLocalID;
		/** Unsigned Byte */ 
		public byte PathCurve;
		/** Unsigned Byte */ 
		public byte ProfileCurve;
            /**Unsigned Short**/
            public int PathBegin;
            /**Unsigned Short**/
            public int PathEnd;
		/** Unsigned Byte */ 
		public byte PathScaleX;
		/** Unsigned Byte */ 
		public byte PathScaleY;
		/** Unsigned Byte */ 
		public byte PathShearX;
		/** Unsigned Byte */ 
		public byte PathShearY;
		/** Signed Byte */ 
		public byte PathTwist;
		/** Signed Byte */ 
		public byte PathTwistBegin;
		/** Signed Byte */ 
		public byte PathRadiusOffset;
		/** Signed Byte */ 
		public byte PathTaperX;
		/** Signed Byte */ 
		public byte PathTaperY;
		/** Unsigned Byte */ 
		public byte PathRevolutions;
		/** Signed Byte */ 
		public byte PathSkew;
            /**Unsigned Short**/
            public int ProfileBegin;
            /**Unsigned Short**/
            public int ProfileEnd;
            /**Unsigned Short**/
            public int ProfileHollow;

            @Override
			public int getLength()
            {
                                {
                    return 27;
                }
            }

            public ObjectDataBlock() { }
            public ObjectDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    ObjectLocalID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    PathCurve = (byte)bytes[i[0]++];
                    ProfileCurve = (byte)bytes[i[0]++];
                    PathBegin = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    PathEnd = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    PathScaleX = (byte)bytes[i[0]++];
                    PathScaleY = (byte)bytes[i[0]++];
                    PathShearX = (byte)bytes[i[0]++];
                    PathShearY = (byte)bytes[i[0]++];
                    PathTwist = (byte)bytes[i[0]++];
                    PathTwistBegin = (byte)bytes[i[0]++];
                    PathRadiusOffset = (byte)bytes[i[0]++];
                    PathTaperX = (byte)bytes[i[0]++];
                    PathTaperY = (byte)bytes[i[0]++];
                    PathRevolutions = (byte)bytes[i[0]++];
                    PathSkew = (byte)bytes[i[0]++];
                    ProfileBegin = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    ProfileEnd = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    ProfileHollow = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(ObjectLocalID, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = PathCurve;
                bytes[i[0]++] = ProfileCurve;
                bytes[i[0]++] = (byte)(PathBegin % 256);
                bytes[i[0]++] = (byte)((PathBegin >> 8) % 256);
                bytes[i[0]++] = (byte)(PathEnd % 256);
                bytes[i[0]++] = (byte)((PathEnd >> 8) % 256);
                bytes[i[0]++] = PathScaleX;
                bytes[i[0]++] = PathScaleY;
                bytes[i[0]++] = PathShearX;
                bytes[i[0]++] = PathShearY;
                bytes[i[0]++] = (byte)PathTwist;
                bytes[i[0]++] = (byte)PathTwistBegin;
                bytes[i[0]++] = (byte)PathRadiusOffset;
                bytes[i[0]++] = (byte)PathTaperX;
                bytes[i[0]++] = (byte)PathTaperY;
                bytes[i[0]++] = PathRevolutions;
                bytes[i[0]++] = (byte)PathSkew;
                bytes[i[0]++] = (byte)(ProfileBegin % 256);
                bytes[i[0]++] = (byte)((ProfileBegin >> 8) % 256);
                bytes[i[0]++] = (byte)(ProfileEnd % 256);
                bytes[i[0]++] = (byte)((ProfileEnd >> 8) % 256);
                bytes[i[0]++] = (byte)(ProfileHollow % 256);
                bytes[i[0]++] = (byte)((ProfileHollow >> 8) % 256);
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += AgentData.getLength();
                for (int j = 0; j < ObjectData.length; j++)
                    length += ObjectData[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public ObjectDataBlock[] ObjectData;

        public ObjectShapePacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.ObjectShape;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 98;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            ObjectData = null;
        }

        public ObjectShapePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(ObjectData == null || ObjectData.length != -1) {
                ObjectData = new ObjectDataBlock[count];
                for(int j = 0; j < count; j++)
                { ObjectData[j] = new ObjectDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { ObjectData[j].FromBytes(bytes, i); }
        }

        public ObjectShapePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(ObjectData == null || ObjectData.length != count) {
                ObjectData = new ObjectDataBlock[count];
                for(int j = 0; j < count; j++)
                { ObjectData[j] = new ObjectDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { ObjectData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length++;
            for (int j = 0; j < ObjectData.length; j++) { length += ObjectData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)ObjectData.length;
            for (int j = 0; j < ObjectData.length; j++) { ObjectData[j].ToBytes(bytes, i); }
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
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            AgentData.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int ObjectDataStart = 0;
            do
            {
                int variableLength = 0;
                int ObjectDataCount = 0;

              i[0] =ObjectDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < ObjectData.length) {
                    int blockLength = ObjectData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++ObjectDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)ObjectDataCount;
                for (i[0] = ObjectDataStart; i[0] < ObjectDataStart + ObjectDataCount; i[0]++) { ObjectData[i[0]].ToBytes(packet, length); }
                ObjectDataStart += ObjectDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                ObjectDataStart < ObjectData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
