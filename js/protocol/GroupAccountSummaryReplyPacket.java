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
	public final class GroupAccountSummaryReplyPacket extends Packet
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
        public static final class MoneyDataBlock extends PacketBlock
        {
            public UUID RequestID = new UUID();
            public int IntervalDays;
            public int CurrentInterval;
		/** Unsigned Byte */ 
		public byte[] StartDate;
            public int Balance;
            public int TotalCredits;
            public int TotalDebits;
            public int ObjectTaxCurrent;
            public int LightTaxCurrent;
            public int LandTaxCurrent;
            public int GroupTaxCurrent;
            public int ParcelDirFeeCurrent;
            public int ObjectTaxEstimate;
            public int LightTaxEstimate;
            public int LandTaxEstimate;
            public int GroupTaxEstimate;
            public int ParcelDirFeeEstimate;
            public int NonExemptMembers;
		/** Unsigned Byte */ 
		public byte[] LastTaxDate;
		/** Unsigned Byte */ 
		public byte[] TaxDate;

            @Override
			public int getLength()
            {
                                {
                    int length = 83;
                    if (StartDate != null) { length += StartDate.length; }
                    if (LastTaxDate != null) { length += LastTaxDate.length; }
                    if (TaxDate != null) { length += TaxDate.length; }
                    return length;
                }
            }

            public MoneyDataBlock() { }
            public MoneyDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    RequestID.FromBytes(bytes, i[0]); i[0] += 16;
                    IntervalDays = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    CurrentInterval = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    StartDate = new byte[length];
                    Utils.arraycopy(bytes, i[0], StartDate, 0, length); i[0] +=  length;
                    Balance = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    TotalCredits = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    TotalDebits = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ObjectTaxCurrent = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    LightTaxCurrent = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    LandTaxCurrent = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    GroupTaxCurrent = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ParcelDirFeeCurrent = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ObjectTaxEstimate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    LightTaxEstimate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    LandTaxEstimate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    GroupTaxEstimate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ParcelDirFeeEstimate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    NonExemptMembers = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    LastTaxDate = new byte[length];
                    Utils.arraycopy(bytes, i[0], LastTaxDate, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    TaxDate = new byte[length];
                    Utils.arraycopy(bytes, i[0], TaxDate, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                RequestID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.intToBytesLit(IntervalDays, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(CurrentInterval, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)StartDate.length;
                Utils.arraycopy(StartDate, 0, bytes, i[0], StartDate.length); i[0] +=  StartDate.length;
                Utils.intToBytesLit(Balance, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(TotalCredits, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(TotalDebits, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ObjectTaxCurrent, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(LightTaxCurrent, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(LandTaxCurrent, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(GroupTaxCurrent, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ParcelDirFeeCurrent, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ObjectTaxEstimate, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(LightTaxEstimate, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(LandTaxEstimate, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(GroupTaxEstimate, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ParcelDirFeeEstimate, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(NonExemptMembers, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)LastTaxDate.length;
                Utils.arraycopy(LastTaxDate, 0, bytes, i[0], LastTaxDate.length); i[0] +=  LastTaxDate.length;
                bytes[i[0]++] = (byte)TaxDate.length;
                Utils.arraycopy(TaxDate, 0, bytes, i[0], TaxDate.length); i[0] +=  TaxDate.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += AgentData.getLength();
                length += MoneyData.getLength();
                return length;
            }
        }
        public AgentDataBlock AgentData;
        public MoneyDataBlock MoneyData;

        public GroupAccountSummaryReplyPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.GroupAccountSummaryReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 354;
            header.Reliable = true;
            header.Zerocoded = true;
            AgentData = new AgentDataBlock();
            MoneyData = new MoneyDataBlock();
        }

        public GroupAccountSummaryReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            MoneyData.FromBytes(bytes, i);
        }

        public GroupAccountSummaryReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
            MoneyData.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += AgentData.getLength();
            length += MoneyData.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            AgentData.ToBytes(bytes, i);
            MoneyData.ToBytes(bytes, i);
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
