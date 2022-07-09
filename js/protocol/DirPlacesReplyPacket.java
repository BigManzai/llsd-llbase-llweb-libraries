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


    public final class DirPlacesReplyPacket extends Packet
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
                	e.printStackTrace();
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
        public static final class QueryDataBlock extends PacketBlock
        {
            public UUID QueryID  = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public QueryDataBlock() { }
            public QueryDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    QueryID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                QueryID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class QueryRepliesBlock extends PacketBlock
        {
            public UUID ParcelID = new UUID();
		/** Unsigned Byte */ 
		public byte[] Name;
            public boolean ForSale;
            public boolean Auction;
            public float Dwell;

            @Override
			public int getLength()
            {
                                {
                    int length = 23;
                    if (Name != null) { length += Name.length; }
                    return length;
                }
            }

            public QueryRepliesBlock() { }
            public QueryRepliesBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    ParcelID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    ForSale = (bytes[i[0]++] != 0) ? true : false;
                    Auction = (bytes[i[0]++] != 0) ? true : false;
                    Dwell = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                ParcelID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)((ForSale) ? 1 : 0);
                bytes[i[0]++] = (byte)((Auction) ? 1 : 0);
                Utils.floatToBytesLit(Dwell, bytes, i[0]); i[0] += 4;
            }

        }

        /// <exclude/>
        public static final class StatusDataBlock extends PacketBlock
        {
            public long Status;

            @Override
			public int getLength()
            {
                                {
                    return 4;
                }
            }

            public StatusDataBlock() { }
            public StatusDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    Status = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
            	//System.out.println(bytes.length + " To Bytes:  " + i[0]);
                Utils.uintToBytesLit(Status, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 13;
                length += AgentData.getLength();
                for (int j = 0; j < QueryData.length; j++)
                    length += QueryData[j].getLength();
                for (int j = 0; j < QueryReplies.length; j++)
                    length += QueryReplies[j].getLength();
                for (int j = 0; j < StatusData.length; j++)
                    length += StatusData[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public QueryDataBlock[] QueryData;
        public QueryRepliesBlock[] QueryReplies;
        public StatusDataBlock[] StatusData;

        public DirPlacesReplyPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.DirPlacesReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 35;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            QueryData = null;
            QueryReplies = null;
            StatusData = null;
        }

        public DirPlacesReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            if(QueryData == null || QueryData.length != -1) {
                QueryData = new QueryDataBlock[count];
                for(int j = 0; j < count; j++)
                { QueryData[j] = new QueryDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { QueryData[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(QueryReplies == null || QueryReplies.length != -1) {
                QueryReplies = new QueryRepliesBlock[count];
                for(int j = 0; j < count; j++)
                { QueryReplies[j] = new QueryRepliesBlock(); }
            }
            for (int j = 0; j < count; j++)
            { QueryReplies[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(StatusData == null || StatusData.length != -1) {
                StatusData = new StatusDataBlock[count];
                for(int j = 0; j < count; j++)
                { StatusData[j] = new StatusDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { StatusData[j].FromBytes(bytes, i); }
        }

        public DirPlacesReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            if(QueryData == null || QueryData.length != count) {
                QueryData = new QueryDataBlock[count];
                for(int j = 0; j < count; j++)
                { QueryData[j] = new QueryDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { QueryData[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(QueryReplies == null || QueryReplies.length != count) {
                QueryReplies = new QueryRepliesBlock[count];
                for(int j = 0; j < count; j++)
                { QueryReplies[j] = new QueryRepliesBlock(); }
            }
            for (int j = 0; j < count; j++)
            { QueryReplies[j].FromBytes(bytes, i); }
            count = Utils.ubyteToInt(bytes[i[0]++]);
            if(StatusData == null || StatusData.length != count) {
                StatusData = new StatusDataBlock[count];
                for(int j = 0; j < count; j++)
                { StatusData[j] = new StatusDataBlock(); }
            }
            for (int j = 0; j < count; j++)
            { StatusData[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length++;
            for (int j = 0; j < QueryData.length; j++) { length += QueryData[j].getLength(); }
            length++;
            for (int j = 0; j < QueryReplies.length; j++) { length += QueryReplies[j].getLength(); }
            length++;
            for (int j = 0; j < StatusData.length; j++) { length += StatusData[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)QueryData.length;
            for (int j = 0; j < QueryData.length; j++) { QueryData[j].ToBytes(bytes, i); }
            bytes[i[0]++] = (byte)QueryReplies.length;
            for (int j = 0; j < QueryReplies.length; j++) { QueryReplies[j].ToBytes(bytes, i); }
            bytes[i[0]++] = (byte)StatusData.length;
            for (int j = 0; j < StatusData.length; j++) { StatusData[j].ToBytes(bytes, i); }
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
            fixedLength += 3;

            int QueryDataStart = 0;
            int QueryRepliesStart = 0;
            int StatusDataStart = 0;
            do
            {
                int variableLength = 0;
                int QueryDataCount = 0;
                int QueryRepliesCount = 0;
                int StatusDataCount = 0;

              i[0] =QueryDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < QueryData.length) {
                    int blockLength = QueryData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++QueryDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

              i[0] =QueryRepliesStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < QueryReplies.length) {
                    int blockLength = QueryReplies[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++QueryRepliesCount;
                    }
                    else { break; }
                    i[0]++;
                }

              i[0] =StatusDataStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < StatusData.length) {
                    int blockLength = StatusData[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++StatusDataCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)QueryDataCount;
                for (i[0] = QueryDataStart; i[0] < QueryDataStart + QueryDataCount; i[0]++) { QueryData[i[0]].ToBytes(packet, length); }
                QueryDataStart += QueryDataCount;

                packet[length[0]++] = (byte)QueryRepliesCount;
                for (i[0] = QueryRepliesStart; i[0] < QueryRepliesStart + QueryRepliesCount; i[0]++) { QueryReplies[i[0]].ToBytes(packet, length); }
                QueryRepliesStart += QueryRepliesCount;

                packet[length[0]++] = (byte)StatusDataCount;
                for (i[0] = StatusDataStart; i[0] < StatusDataStart + StatusDataCount; i[0]++) { StatusData[i[0]].ToBytes(packet, length); }
                StatusDataStart += StatusDataCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                QueryDataStart < QueryData.length ||
                QueryRepliesStart < QueryReplies.length ||
                StatusDataStart < StatusData.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
