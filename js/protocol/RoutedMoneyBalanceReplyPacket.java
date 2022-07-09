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
	public final class RoutedMoneyBalanceReplyPacket extends Packet
    {
        /// <exclude/>
        public static final class TargetBlockBlock extends PacketBlock
        {
            public long TargetIP;
            /** Unsigned Short **/
            public int TargetPort;

            @Override
			public int getLength()
            {
                                {
                    return 6;
                }
            }

            public TargetBlockBlock() { }
            public TargetBlockBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    TargetIP = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    TargetPort = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(TargetIP, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((TargetPort >> 8) % 256);
                bytes[i[0]++] = (byte)(TargetPort % 256);
            }

        }

        /// <exclude/>
        public static final class MoneyDataBlock extends PacketBlock
        {
            public UUID AgentID = new UUID();
            public UUID TransactionID = new UUID();
            public boolean TransactionSuccess;
            public int MoneyBalance;
            public int SquareMetersCredit;
            public int SquareMetersCommitted;
		/** Unsigned Byte */ 
		public byte[] Description;

            @Override
			public int getLength()
            {
                                {
                    int length = 46;
                    if (Description != null) { length += Description.length; }
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
                    AgentID.FromBytes(bytes, i[0]); i[0] += 16;
                    TransactionID.FromBytes(bytes, i[0]); i[0] += 16;
                    TransactionSuccess = (bytes[i[0]++] != 0) ? true : false;
                    MoneyBalance = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SquareMetersCredit = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SquareMetersCommitted = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Description = new byte[length];
                    Utils.arraycopy(bytes, i[0], Description, 0, length); i[0] +=  length;
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
                TransactionID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)((TransactionSuccess) ? 1 : 0);
                Utils.intToBytesLit(MoneyBalance, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(SquareMetersCredit, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(SquareMetersCommitted, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)Description.length;
                Utils.arraycopy(Description, 0, bytes, i[0], Description.length); i[0] +=  Description.length;
            }

        }

        /// <exclude/>
        public static final class TransactionInfoBlock extends PacketBlock
        {
            public int TransactionType;
            public UUID SourceID = new UUID();
            public boolean IsSourceGroup;
            public UUID DestID = new UUID();
            public boolean IsDestGroup;
            public int Amount;
		/** Unsigned Byte */ 
		public byte[] ItemDescription;

            @Override
			public int getLength()
            {
                                {
                    int length = 43;
                    if (ItemDescription != null) { length += ItemDescription.length; }
                    return length;
                }
            }

            public TransactionInfoBlock() { }
            public TransactionInfoBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    TransactionType = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SourceID.FromBytes(bytes, i[0]); i[0] += 16;
                    IsSourceGroup = (bytes[i[0]++] != 0) ? true : false;
                    DestID.FromBytes(bytes, i[0]); i[0] += 16;
                    IsDestGroup = (bytes[i[0]++] != 0) ? true : false;
                    Amount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    ItemDescription = new byte[length];
                    Utils.arraycopy(bytes, i[0], ItemDescription, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.intToBytesLit(TransactionType, bytes, i[0]); i[0] += 4;
                SourceID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)((IsSourceGroup) ? 1 : 0);
                DestID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)((IsDestGroup) ? 1 : 0);
                Utils.intToBytesLit(Amount, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)ItemDescription.length;
                Utils.arraycopy(ItemDescription, 0, bytes, i[0], ItemDescription.length); i[0] +=  ItemDescription.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += TargetBlock.getLength();
                length += MoneyData.getLength();
                length += TransactionInfo.getLength();
                return length;
            }
        }
        public TargetBlockBlock TargetBlock;
        public MoneyDataBlock MoneyData;
        public TransactionInfoBlock TransactionInfo;

        public RoutedMoneyBalanceReplyPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.RoutedMoneyBalanceReply;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 315;
            header.Reliable = true;
            header.Zerocoded = true;
            TargetBlock = new TargetBlockBlock();
            MoneyData = new MoneyDataBlock();
            TransactionInfo = new TransactionInfoBlock();
        }

        public RoutedMoneyBalanceReplyPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            TargetBlock.FromBytes(bytes, i);
            MoneyData.FromBytes(bytes, i);
            TransactionInfo.FromBytes(bytes, i);
        }

        public RoutedMoneyBalanceReplyPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            TargetBlock.FromBytes(bytes, i);
            MoneyData.FromBytes(bytes, i);
            TransactionInfo.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += TargetBlock.getLength();
            length += MoneyData.getLength();
            length += TransactionInfo.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            TargetBlock.ToBytes(bytes, i);
            MoneyData.ToBytes(bytes, i);
            TransactionInfo.ToBytes(bytes, i);
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
