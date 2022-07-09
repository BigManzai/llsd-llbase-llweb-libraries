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


    public final class ParcelReturnObjectsPacket extends Packet
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
        public static final class ParcelDataBlock extends PacketBlock
        {
            public int LocalID;
            public long ReturnType;

            @Override
			public int getLength()
            {
                                {
                    return 8;
                }
            }

            public ParcelDataBlock() { }
            public ParcelDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    LocalID = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ReturnType = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.intToBytesLit(LocalID, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(ReturnType, bytes, i[0]); i[0] += 4;
            }

        }

        /// <exclude/>
        public static final class TaskIDsBlock extends PacketBlock
        {
            public UUID TaskID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public TaskIDsBlock() { }
            public TaskIDsBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    TaskID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                TaskID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class OwnerIDsBlock extends PacketBlock
        {
            public UUID OwnerID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public OwnerIDsBlock() { }
            public OwnerIDsBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 12;
                length += AgentData.getLength();
                length += ParcelData.getLength();
                for (int j = 0; j < TaskIDs.length; j++)
                    length += TaskIDs[j].getLength();
                for (int j = 0; j < OwnerIDs.length; j++)
                    length += OwnerIDs[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public ParcelDataBlock ParcelData;
        public TaskIDsBlock[] TaskIDs;
        public OwnerIDsBlock[] OwnerIDs;

        public ParcelReturnObjectsPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.ParcelReturnObjects;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 199;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            ParcelData = new ParcelDataBlock();
            TaskIDs = null;
            OwnerIDs = null;
        }

        public ParcelReturnObjectsPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            ParcelData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(TaskIDs == null || TaskIDs.length != -1) {
                TaskIDs = new TaskIDsBlock[count];
                for(int j = 0; j < count; j++)
                { TaskIDs[j] = new TaskIDsBlock(); }
            }
            for (int j = 0; j < count; j++)
            { TaskIDs[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(OwnerIDs == null || OwnerIDs.length != -1) {
                OwnerIDs = new OwnerIDsBlock[count];
                for(int j = 0; j < count; j++)
                { OwnerIDs[j] = new OwnerIDsBlock(); }
            }
            for (int j = 0; j < count; j++)
            { OwnerIDs[j].FromBytes(bytes, i); }
        }

        public ParcelReturnObjectsPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            ParcelData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(TaskIDs == null || TaskIDs.length != count) {
                TaskIDs = new TaskIDsBlock[count];
                for(int j = 0; j < count; j++)
                { TaskIDs[j] = new TaskIDsBlock(); }
            }
            for (int j = 0; j < count; j++)
            { TaskIDs[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(OwnerIDs == null || OwnerIDs.length != count) {
                OwnerIDs = new OwnerIDsBlock[count];
                for(int j = 0; j < count; j++)
                { OwnerIDs[j] = new OwnerIDsBlock(); }
            }
            for (int j = 0; j < count; j++)
            { OwnerIDs[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += ParcelData.getLength();
            length++;
            for (int j = 0; j < TaskIDs.length; j++) { length += TaskIDs[j].getLength(); }
            length++;
            for (int j = 0; j < OwnerIDs.length; j++) { length += OwnerIDs[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            ParcelData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)TaskIDs.length;
            for (int j = 0; j < TaskIDs.length; j++) { TaskIDs[j].ToBytes(bytes, i); }
            bytes[i[0]++] = (byte)OwnerIDs.length;
            for (int j = 0; j < OwnerIDs.length; j++) { OwnerIDs[j].ToBytes(bytes, i); }
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
            fixedLength += ParcelData.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            AgentData.ToBytes(fixedBytes, i);
            ParcelData.ToBytes(fixedBytes, i);
            fixedLength += 2;

            int TaskIDsStart = 0;
            int OwnerIDsStart = 0;
            do
            {
                int variableLength = 0;
                int TaskIDsCount = 0;
                int OwnerIDsCount = 0;

              i[0] =TaskIDsStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < TaskIDs.length) {
                    int blockLength = TaskIDs[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++TaskIDsCount;
                    }
                    else { break; }
                    i[0]++;
                }

              i[0] =OwnerIDsStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < OwnerIDs.length) {
                    int blockLength = OwnerIDs[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++OwnerIDsCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)TaskIDsCount;
                for (i[0] = TaskIDsStart; i[0] < TaskIDsStart + TaskIDsCount; i[0]++) { TaskIDs[i[0]].ToBytes(packet, length); }
                TaskIDsStart += TaskIDsCount;

                packet[length[0]++] = (byte)OwnerIDsCount;
                for (i[0] = OwnerIDsStart; i[0] < OwnerIDsStart + OwnerIDsCount; i[0]++) { OwnerIDs[i[0]].ToBytes(packet, length); }
                OwnerIDsStart += OwnerIDsCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                TaskIDsStart < TaskIDs.length ||
                OwnerIDsStart < OwnerIDs.length);

            return packets.toArray(new byte[0][0]);
        }
    }