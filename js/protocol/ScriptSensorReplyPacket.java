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

import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;
	public final class ScriptSensorReplyPacket extends Packet
    {
        /// <exclude/>
        public static final class RequesterBlock extends PacketBlock
        {
            public UUID SourceID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public RequesterBlock() { }
            public RequesterBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    SourceID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                SourceID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class SensedDataBlock extends PacketBlock
        {
            public UUID ObjectID = new UUID();
            public UUID OwnerID = new UUID();
            public UUID GroupID = new UUID();
            public Vector3 Position = new Vector3();
            public Vector3 Velocity = new Vector3();
            public Quaternion Rotation = new Quaternion();
		/** Unsigned Byte */ 
		public byte[] Name;
            public int Type;
            public float Range;

            @Override
			public int getLength()
            {
                                {
                    int length = 93;
                    if (Name != null) { length += Name.length; }
                    return length;
                }
            }

            public SensedDataBlock() { }
            public SensedDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    ObjectID.FromBytes(bytes, i[0]); i[0] += 16;
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    Position.fromBytesLit(bytes, i[0]); i[0] += 12;
                    Velocity.fromBytesLit(bytes, i[0]); i[0] += 12;
                    Rotation.fromBytesLit(bytes, i[0], true); i[0] += 12;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    Type = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    Range = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                ObjectID.ToBytes(bytes, i[0]); i[0] += 16;
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
                Position.toBytesLit(bytes, i[0]); i[0] += 12;
                Velocity.toBytesLit(bytes, i[0]); i[0] += 12;
                Rotation.toBytesLit(bytes, i[0]); i[0] += 12;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                Utils.intToBytesLit(Type, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(Range, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += Requester.getLength();
                for (int j = 0; j < SensedData.length; j++)
                    length += SensedData[j].getLength();
                return length;
            }
        }
        public RequesterBlock Requester;
        public SensedDataBlock[] SensedData;

        public ScriptSensorReplyPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.ScriptSensorReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 248;
            header.Reliable = true;
            header.Zerocoded = true;
            Requester = new RequesterBlock();
            SensedData = null;
        }

        public ScriptSensorReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            Requester.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(SensedData == null || SensedData.length != -1) {
                SensedData = new SensedDataBlock[count];
                for(int j = 0; j < count; j++)
                { SensedData[j] = new SensedDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { SensedData[j].FromBytes(bytes, i); }
        }

        public ScriptSensorReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            Requester.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(SensedData == null || SensedData.length != count) {
                SensedData = new SensedDataBlock[count];
                for(int j = 0; j < count; j++)
                { SensedData[j] = new SensedDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { SensedData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += Requester.getLength();
            length++;
            for (int j = 0; j < SensedData.length; j++) { length += SensedData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            Requester.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)SensedData.length;
            for (int j = 0; j < SensedData.length; j++) { SensedData[j].ToBytes(bytes, i); }
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

            fixedLength += Requester.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            Requester.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int SensedDataStart = 0;
            do
            {
                int variableLength = 0;
                int SensedDataCount = 0;

              i[0] =SensedDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < SensedData.length) {
                    int blockLength = SensedData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++SensedDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)SensedDataCount;
                for (i[0] = SensedDataStart; i[0] < SensedDataStart + SensedDataCount; i[0]++) { SensedData[i[0]].ToBytes(packet, length); }
                SensedDataStart += SensedDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                SensedDataStart < SensedData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
