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


    public final class AvatarAppearancePacket extends Packet
    {
        /// <exclude/>
        public static final class SenderBlock extends PacketBlock
        {
            public UUID ID = new UUID();
            public boolean IsTrial;

            @Override
			public int getLength()
            {
                                {
                    return 17;
                }
            }

            public SenderBlock() { }
            public SenderBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    ID.FromBytes(bytes, i[0]); i[0] += 16;
                    IsTrial = (bytes[i[0]++] != 0) ? true : false;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                ID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)((IsTrial) ? 1 : 0);
            }

        }

        /// <exclude/>
        public static final class ObjectDataBlock extends PacketBlock
        {
            public byte[] TextureEntry;

            @Override
			public int getLength()
            {
                                {
                    int length = 2;
                    if (TextureEntry != null) { length += TextureEntry.length; }
                    return length;
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
                int length;
                try
                {
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    TextureEntry = new byte[length];
                    Utils.arraycopy(bytes, i[0], TextureEntry, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)(TextureEntry.length % 256);
                bytes[i[0]++] = (byte)((TextureEntry.length >> 8) % 256);
                Utils.arraycopy(TextureEntry, 0, bytes, i[0], TextureEntry.length); i[0] +=  TextureEntry.length;
            }

        }

        /// <exclude/>
        public static final class VisualParamBlock extends PacketBlock
        {
            public byte ParamValue;

            @Override
			public int getLength()
            {
                                {
                    return 1;
                }
            }

            public VisualParamBlock() { }
            public VisualParamBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    ParamValue = (byte)bytes[i[0]++];
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = ParamValue;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += Sender.getLength();
                length += ObjectData.getLength();
                for (int j = 0; j < VisualParam.length; j++)
                    length += VisualParam[j].getLength();
                return length;
            }
        }
        public SenderBlock Sender;
        public ObjectDataBlock ObjectData;
        public VisualParamBlock[] VisualParam;

        public AvatarAppearancePacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.AvatarAppearance;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 158;
            header.Reliable = true;
            header.Zerocoded = true;
            Sender = new SenderBlock();
            ObjectData = new ObjectDataBlock();
            VisualParam = null;
        }

        public AvatarAppearancePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            Sender.FromBytes(bytes, i);
            ObjectData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(VisualParam == null || VisualParam.length != -1) {
                VisualParam = new VisualParamBlock[count];
                for(int j = 0; j < count; j++)
                { VisualParam[j] = new VisualParamBlock(); }
            }
            for (int j = 0; j < count; j++)
            { VisualParam[j].FromBytes(bytes, i); }
        }

        public AvatarAppearancePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            Sender.FromBytes(bytes, i);
            ObjectData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(VisualParam == null || VisualParam.length != count) {
                VisualParam = new VisualParamBlock[count];
                for(int j = 0; j < count; j++)
                { VisualParam[j] = new VisualParamBlock(); }
            }
            for (int j = 0; j < count; j++)
            { VisualParam[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += Sender.getLength();
            length += ObjectData.getLength();
            length++;
            for (int j = 0; j < VisualParam.length; j++) { length += VisualParam[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            Sender.ToBytes(bytes, i);
            ObjectData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)VisualParam.length;
            for (int j = 0; j < VisualParam.length; j++) { VisualParam[j].ToBytes(bytes, i); }
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

            fixedLength += Sender.getLength();
            fixedLength += ObjectData.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            Sender.ToBytes(fixedBytes, i);
            ObjectData.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int VisualParamStart = 0;
            do
            {
                int variableLength = 0;
                int VisualParamCount = 0;

              i[0] =VisualParamStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < VisualParam.length) {
                    int blockLength = VisualParam[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++VisualParamCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)VisualParamCount;
                for (i[0] = VisualParamStart; i[0] < VisualParamStart + VisualParamCount; i[0]++) { VisualParam[i[0]].ToBytes(packet, length); }
                VisualParamStart += VisualParamCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                VisualParamStart < VisualParam.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
