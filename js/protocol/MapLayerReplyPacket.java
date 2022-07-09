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
	public final class MapLayerReplyPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public long Flags;

            @Override
			public int getLength()
            {
                                {
                    return 20;
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
                    Flags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
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
                Utils.uintToBytesLit(Flags, bytes, i[0]); i[0] += 4;
            }

        }

        /// <exclude/>
        public static final class LayerDataBlock extends PacketBlock
        {
            public long Left;
            public long Right;
            public long Top;
            public long Bottom;
            public UUID ImageID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 32;
                }
            }

            public LayerDataBlock() { }
            public LayerDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    Left = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    Right = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    Top = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    Bottom = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    ImageID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(Left, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(Right, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(Top, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(Bottom, bytes, i[0]); i[0] += 4;
                ImageID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += AgentData.getLength();
                for (int j = 0; j < LayerData.length; j++)
                    length += LayerData[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public LayerDataBlock[] LayerData;

        public MapLayerReplyPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.MapLayerReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 406;
            header.Reliable = true;
            AgentData = new AgentDataBlock();
            LayerData = null;
        }

        public MapLayerReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            if(LayerData == null || LayerData.length != -1) {
                LayerData = new LayerDataBlock[count];
                for(int j = 0; j < count; j++)
                { LayerData[j] = new LayerDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { LayerData[j].FromBytes(bytes, i); }
        }

        public MapLayerReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            if(LayerData == null || LayerData.length != count) {
                LayerData = new LayerDataBlock[count];
                for(int j = 0; j < count; j++)
                { LayerData[j] = new LayerDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { LayerData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length++;
            for (int j = 0; j < LayerData.length; j++) { length += LayerData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)LayerData.length;
            for (int j = 0; j < LayerData.length; j++) { LayerData[j].ToBytes(bytes, i); }
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

            int LayerDataStart = 0;
            do
            {
                int variableLength = 0;
                int LayerDataCount = 0;

              i[0] =LayerDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < LayerData.length) {
                    int blockLength = LayerData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++LayerDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)LayerDataCount;
                for (i[0] = LayerDataStart; i[0] < LayerDataStart + LayerDataCount; i[0]++) { LayerData[i[0]].ToBytes(packet, length); }
                LayerDataStart += LayerDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                LayerDataStart < LayerData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
