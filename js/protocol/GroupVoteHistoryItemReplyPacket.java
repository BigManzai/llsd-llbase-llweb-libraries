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
	public final class GroupVoteHistoryItemReplyPacket extends Packet
    {
        /// <exclude/>
        public static final class AgentDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID GroupID = new UUID();

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
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
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
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class TransactionDataBlock extends PacketBlock
        {
            public UUID TransactionID = new UUID();
            public long TotalNumItems;

            @Override
			public int getLength()
            {
                                {
                    return 20;
                }
            }

            public TransactionDataBlock() { }
            public TransactionDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    TransactionID.FromBytes(bytes, i[0]); i[0] += 16;
                    TotalNumItems = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                TransactionID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.uintToBytesLit(TotalNumItems, bytes, i[0]); i[0] += 4;
            }

        }

        /// <exclude/>
        public static final class HistoryItemDataBlock extends PacketBlock
        {
            public UUID VoteID = new UUID();
		/** Unsigned Byte */ 
		public byte[] TerseDateID;
		/** Unsigned Byte */ 
		public byte[] StartDateTime;
		/** Unsigned Byte */ 
		public byte[] EndDateTime;
            public UUID VoteInitiator = new UUID();
		/** Unsigned Byte */ 
		public byte[] VoteType;
		/** Unsigned Byte */ 
		public byte[] VoteResult;
            public float Majority;
            public int Quorum;
		/** Unsigned Byte */ 
		public byte[] ProposalText;

            @Override
			public int getLength()
            {
                                {
                    int length = 47;
                    if (TerseDateID != null) { length += TerseDateID.length; }
                    if (StartDateTime != null) { length += StartDateTime.length; }
                    if (EndDateTime != null) { length += EndDateTime.length; }
                    if (VoteType != null) { length += VoteType.length; }
                    if (VoteResult != null) { length += VoteResult.length; }
                    if (ProposalText != null) { length += ProposalText.length; }
                    return length;
                }
            }

            public HistoryItemDataBlock() { }
            public HistoryItemDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    VoteID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    TerseDateID = new byte[length];
                    Utils.arraycopy(bytes, i[0], TerseDateID, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    StartDateTime = new byte[length];
                    Utils.arraycopy(bytes, i[0], StartDateTime, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    EndDateTime = new byte[length];
                    Utils.arraycopy(bytes, i[0], EndDateTime, 0, length); i[0] +=  length;
                    VoteInitiator.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    VoteType = new byte[length];
                    Utils.arraycopy(bytes, i[0], VoteType, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    VoteResult = new byte[length];
                    Utils.arraycopy(bytes, i[0], VoteResult, 0, length); i[0] +=  length;
                    Majority = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    Quorum = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    ProposalText = new byte[length];
                    Utils.arraycopy(bytes, i[0], ProposalText, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                VoteID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)TerseDateID.length;
                Utils.arraycopy(TerseDateID, 0, bytes, i[0], TerseDateID.length); i[0] +=  TerseDateID.length;
                bytes[i[0]++] = (byte)StartDateTime.length;
                Utils.arraycopy(StartDateTime, 0, bytes, i[0], StartDateTime.length); i[0] +=  StartDateTime.length;
                bytes[i[0]++] = (byte)EndDateTime.length;
                Utils.arraycopy(EndDateTime, 0, bytes, i[0], EndDateTime.length); i[0] +=  EndDateTime.length;
                VoteInitiator.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)VoteType.length;
                Utils.arraycopy(VoteType, 0, bytes, i[0], VoteType.length); i[0] +=  VoteType.length;
                bytes[i[0]++] = (byte)VoteResult.length;
                Utils.arraycopy(VoteResult, 0, bytes, i[0], VoteResult.length); i[0] +=  VoteResult.length;
                Utils.floatToBytesLit(Majority, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(Quorum, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)(ProposalText.length % 256);
                bytes[i[0]++] = (byte)((ProposalText.length >> 8) % 256);
                Utils.arraycopy(ProposalText, 0, bytes, i[0], ProposalText.length); i[0] +=  ProposalText.length;
            }

        }

        /// <exclude/>
        public static final class VoteItemBlock extends PacketBlock
        {
            public UUID CandidateID = new UUID();
		/** Unsigned Byte */ 
		public byte[] VoteCast;
            public int NumVotes;

            @Override
			public int getLength()
            {
                                {
                    int length = 21;
                    if (VoteCast != null) { length += VoteCast.length; }
                    return length;
                }
            }

            public VoteItemBlock() { }
            public VoteItemBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    CandidateID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    VoteCast = new byte[length];
                    Utils.arraycopy(bytes, i[0], VoteCast, 0, length); i[0] +=  length;
                    NumVotes = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                CandidateID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)VoteCast.length;
                Utils.arraycopy(VoteCast, 0, bytes, i[0], VoteCast.length); i[0] +=  VoteCast.length;
                Utils.intToBytesLit(NumVotes, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 11;
                length += AgentData.getLength();
                length += TransactionData.getLength();
                length += HistoryItemData.getLength();
                for (int j = 0; j < VoteItem.length; j++)
                    length += VoteItem[j].getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public TransactionDataBlock TransactionData;
        public HistoryItemDataBlock HistoryItemData;
        public VoteItemBlock[] VoteItem;

        public GroupVoteHistoryItemReplyPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.GroupVoteHistoryItemReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 362;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            TransactionData = new TransactionDataBlock();
            HistoryItemData = new HistoryItemDataBlock();
            VoteItem = null;
        }

        public GroupVoteHistoryItemReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            TransactionData.FromBytes(bytes, i);
            HistoryItemData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(VoteItem == null || VoteItem.length != -1) {
                VoteItem = new VoteItemBlock[count];
                for(int j = 0; j < count; j++)
                { VoteItem[j] = new VoteItemBlock(); }
            }
            for (int j = 0; j < count; j++)
            { VoteItem[j].FromBytes(bytes, i); }
        }

        public GroupVoteHistoryItemReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            TransactionData.FromBytes(bytes, i);
            HistoryItemData.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(VoteItem == null || VoteItem.length != count) {
                VoteItem = new VoteItemBlock[count];
                for(int j = 0; j < count; j++)
                { VoteItem[j] = new VoteItemBlock(); }
            }
            for (int j = 0; j < count; j++)
            { VoteItem[j].FromBytes(bytes, i); }
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += TransactionData.getLength();
            length += HistoryItemData.getLength();
            length++;
            for (int j = 0; j < VoteItem.length; j++) { length += VoteItem[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            TransactionData.ToBytes(bytes, i);
            HistoryItemData.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)VoteItem.length;
            for (int j = 0; j < VoteItem.length; j++) { VoteItem[j].ToBytes(bytes, i); }
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
            fixedLength += TransactionData.getLength();
            fixedLength += HistoryItemData.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes, i);
            AgentData.ToBytes(fixedBytes, i);
            TransactionData.ToBytes(fixedBytes, i);
            HistoryItemData.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int VoteItemStart = 0;
            do
            {
                int variableLength = 0;
                int VoteItemCount = 0;

              i[0] =VoteItemStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < VoteItem.length) {
                    int blockLength = VoteItem[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++VoteItemCount;
                    }
                    else { break; }
                    i[0]++;
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[] {fixedBytes.length};
                Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)VoteItemCount;
                for (i[0] = VoteItemStart; i[0] < VoteItemStart + VoteItemCount; i[0]++) { VoteItem[i[0]].ToBytes(packet, length); }
                VoteItemStart += VoteItemCount;

                if (acksLength[0] > 0) {
                    Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                VoteItemStart < VoteItem.length);

            return packets.toArray(new byte[0][0]);
        }
    }

    /// <exclude/>
