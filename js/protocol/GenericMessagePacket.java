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


public final class GenericMessagePacket extends Packet
{
	/// <exclude/>
	public static final class AgentDataBlock extends PacketBlock
	{
		public UUID AgentID = new UUID();
		public UUID SessionID = new UUID();
		public UUID TransactionID = new UUID();

		@Override
		public int getLength()
		{
			{
				return 48;
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
				TransactionID.FromBytes(bytes, i[0]); i[0] += 16;
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
			TransactionID.ToBytes(bytes, i[0]); i[0] += 16;
		}

	}

	/// <exclude/>
	public static final class MethodDataBlock extends PacketBlock
	{
		public byte[] Method;
		public UUID Invoice = new UUID();

		@Override
		public int getLength()
		{
			{
				int length = 17;
				if (Method != null) { length += Method.length; }
				return length;
			}
		}

		public MethodDataBlock() { }
		public MethodDataBlock(byte[] bytes, int[] i) throws MalformedDataException
		{
			FromBytes(bytes, i);
		}

		@Override
		public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
		{
			int length;
			try
			{
				length = Utils.ubyteToInt(bytes[i[0]++]);
				Method = new byte[length];
				Utils.arraycopy(bytes, i[0], Method, 0, length); i[0] +=  length;
				Invoice.FromBytes(bytes, i[0]); i[0] += 16;
			}
			catch (Exception e)
			{
				throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
			}
		}

		@Override
		public void ToBytes(byte[] bytes, int[] i)
		{
			bytes[i[0]++] = (byte)Method.length;
			Utils.arraycopy(Method, 0, bytes, i[0], Method.length); i[0] +=  Method.length;
			Invoice.ToBytes(bytes, i[0]); i[0] += 16;
		}

	}

	/// <exclude/>
	public static final class ParamListBlock extends PacketBlock
	{
		public byte[] Parameter;

		@Override
		public int getLength()
		{
			{
				int length = 1;
				if (Parameter != null) { length += Parameter.length; }
				return length;
			}
		}

		public ParamListBlock() { }
		public ParamListBlock(byte[] bytes, int[] i) throws MalformedDataException
		{
			FromBytes(bytes, i);
		}

		@Override
		public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
		{
			int length;
			try
			{
				length = Utils.ubyteToInt(bytes[i[0]++]);
				Parameter = new byte[length];
				Utils.arraycopy(bytes, i[0], Parameter, 0, length); i[0] +=  length;
			}
			catch (Exception e)
			{
				throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
			}
		}

		@Override
		public void ToBytes(byte[] bytes, int[] i)
		{
			bytes[i[0]++] = (byte)Parameter.length;
			Utils.arraycopy(Parameter, 0, bytes, i[0], Parameter.length); i[0] +=  Parameter.length;
		}

	}

	@Override
	public int getLength()
	{
		{
			int length = 11;
			length += AgentData.getLength();
			length += MethodData.getLength();
			for (int j = 0; j < ParamList.length; j++)
				length += ParamList[j].getLength();
			return length;
		}
	}
	public AgentDataBlock AgentData;
	public MethodDataBlock MethodData;
	public ParamListBlock[] ParamList;

	public GenericMessagePacket()
	{
		HasVariableBlocks = true;
		Type = PacketType.GenericMessage;
		this.header =  new Header();
		header.Frequency = PacketFrequency.Low;
		header.ID = 261;
		header.Reliable = true;
		header.Zerocoded = true;
		AgentData = new AgentDataBlock();
		MethodData = new MethodDataBlock();
		ParamList = null;
	}

	public GenericMessagePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
		MethodData.FromBytes(bytes, i);
		int count = Utils.ubyteToInt(bytes[i[0]++]);
		if(ParamList == null || ParamList.length != -1) {
			ParamList = new ParamListBlock[count];
			for(int j = 0; j < count; j++)
			{ ParamList[j] = new ParamListBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ ParamList[j].FromBytes(bytes, i); }
	}

	public GenericMessagePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
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
		MethodData.FromBytes(bytes, i);
		int count = Utils.ubyteToInt(bytes[i[0]++]);
		if(ParamList == null || ParamList.length != count) {
			ParamList = new ParamListBlock[count];
			for(int j = 0; j < count; j++)
			{ ParamList[j] = new ParamListBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ ParamList[j].FromBytes(bytes, i); }
	}

	@Override
	public byte[] ToBytes()
	{
		int length = 10;
		length += AgentData.getLength();
		length += MethodData.getLength();
		length++;
		for (int j = 0; j < ParamList.length; j++) { length += ParamList[j].getLength(); }
		if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
		byte[] bytes = new byte[length];
		int[] i = new int[]{0};
		header.ToBytes(bytes, i);
		AgentData.ToBytes(bytes, i);
		MethodData.ToBytes(bytes, i);
		bytes[i[0]++] = (byte)ParamList.length;
		for (int j = 0; j < ParamList.length; j++) { ParamList[j].ToBytes(bytes, i); }
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
		fixedLength += MethodData.getLength();
		byte[] fixedBytes = new byte[fixedLength];
		header.ToBytes(fixedBytes, i);
		AgentData.ToBytes(fixedBytes, i);
		MethodData.ToBytes(fixedBytes, i);
		fixedLength += 1;

		int ParamListStart = 0;
		do
		{
			int variableLength = 0;
			int ParamListCount = 0;

			i[0] =ParamListStart;
			while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < ParamList.length) {
				int blockLength = ParamList[i[0]].getLength();
				if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
					variableLength += blockLength;
					++ParamListCount;
				}
				else { break; }
				i[0]++;
			}

			byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
			int[] length = new int[] {fixedBytes.length};
			Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
			if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

			packet[length[0]++] = (byte)ParamListCount;
			for (i[0] = ParamListStart; i[0] < ParamListStart + ParamListCount; i[0]++) { ParamList[i[0]].ToBytes(packet, length); }
			ParamListStart += ParamListCount;

			if (acksLength[0] > 0) {
				Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
				acksLength[0] = 0;
			}

			packets.add(packet);
		} while (
				ParamListStart < ParamList.length);

		return packets.toArray(new byte[0][0]);
	}
}

/// <exclude/>
