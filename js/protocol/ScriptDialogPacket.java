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
public final class ScriptDialogPacket extends Packet
{
	/// <exclude/>
	public static final class DataBlock extends PacketBlock
	{
		public UUID ObjectID = new UUID();
		/** Unsigned Byte */ 
		public byte[] FirstName;
		/** Unsigned Byte */ 
		public byte[] LastName;
		/** Unsigned Byte */ 
		public byte[] ObjectName;
		/** Unsigned Byte */ 
		public byte[] Message;
		public int ChatChannel;
		public UUID ImageID = new UUID();

		@Override
		public int getLength()
		{
			{
				int length = 41;
				if (FirstName != null) { length += FirstName.length; }
				if (LastName != null) { length += LastName.length; }
				if (ObjectName != null) { length += ObjectName.length; }
				if (Message != null) { length += Message.length; }
				return length;
			}
		}

		public DataBlock() { }
		public DataBlock(byte[] bytes, int[] i) throws MalformedDataException
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
				length = Utils.ubyteToInt(bytes[i[0]++]);
				FirstName = new byte[length];
				Utils.arraycopy(bytes, i[0], FirstName, 0, length); i[0] +=  length;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				LastName = new byte[length];
				Utils.arraycopy(bytes, i[0], LastName, 0, length); i[0] +=  length;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				ObjectName = new byte[length];
				Utils.arraycopy(bytes, i[0], ObjectName, 0, length); i[0] +=  length;
				length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				Message = new byte[length];
				Utils.arraycopy(bytes, i[0], Message, 0, length); i[0] +=  length;
				ChatChannel = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
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
			ObjectID.ToBytes(bytes, i[0]); i[0] += 16;
			bytes[i[0]++] = (byte)FirstName.length;
			Utils.arraycopy(FirstName, 0, bytes, i[0], FirstName.length); i[0] +=  FirstName.length;
			bytes[i[0]++] = (byte)LastName.length;
			Utils.arraycopy(LastName, 0, bytes, i[0], LastName.length); i[0] +=  LastName.length;
			bytes[i[0]++] = (byte)ObjectName.length;
			Utils.arraycopy(ObjectName, 0, bytes, i[0], ObjectName.length); i[0] +=  ObjectName.length;
			bytes[i[0]++] = (byte)(Message.length % 256);
			bytes[i[0]++] = (byte)((Message.length >> 8) % 256);
			Utils.arraycopy(Message, 0, bytes, i[0], Message.length); i[0] +=  Message.length;
			Utils.intToBytesLit(ChatChannel, bytes, i[0]); i[0] += 4;
			ImageID.ToBytes(bytes, i[0]); i[0] += 16;
		}

	}

	/// <exclude/>
	public static final class ButtonsBlock extends PacketBlock
	{
		public byte[] ButtonLabel;

		@Override
		public int getLength()
		{
			{
				int length = 1;
				if (ButtonLabel != null) { length += ButtonLabel.length; }
				return length;
			}
		}

		public ButtonsBlock() { }
		public ButtonsBlock(byte[] bytes, int[] i) throws MalformedDataException
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
				ButtonLabel = new byte[length];
				Utils.arraycopy(bytes, i[0], ButtonLabel, 0, length); i[0] +=  length;
			}
			catch (Exception e)
			{
				throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
			}
		}

		@Override
		public void ToBytes(byte[] bytes, int[] i)
		{
			bytes[i[0]++] = (byte)ButtonLabel.length;
			Utils.arraycopy(ButtonLabel, 0, bytes, i[0], ButtonLabel.length); i[0] +=  ButtonLabel.length;
		}

	}

	/// <exclude/>
	public static final class OwnerDataBlock extends PacketBlock
	{
		public UUID OwnerID = new UUID();

		@Override
		public int getLength()
		{
			{
				return 16;
			}
		}

		public OwnerDataBlock() { }
		public OwnerDataBlock(byte[] bytes, int[] i) throws MalformedDataException
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
			length += Data.getLength();
			for (int j = 0; j < Buttons.length; j++)
				length += Buttons[j].getLength();
			for (int j = 0; j < OwnerData.length; j++)
				length += OwnerData[j].getLength();
			return length;
		}
	}
	public DataBlock Data;
	public ButtonsBlock[] Buttons;
	public OwnerDataBlock[] OwnerData;

	public ScriptDialogPacket()
	{
		HasVariableBlocks = true;
		Type = PacketType.ScriptDialog;
		this.header =  new Header();
		header.Frequency = PacketFrequency.Low;
		header.ID = 190;
		header.Reliable = true;
		header.Zerocoded = true;
		Data = new DataBlock();
		Buttons = null;
		OwnerData = null;
	}

	public ScriptDialogPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
		Data.FromBytes(bytes, i);
		int count = Utils.ubyteToInt(bytes[i[0]++]);
		if(Buttons == null || Buttons.length != -1) {
			Buttons = new ButtonsBlock[count];
			for(int j = 0; j < count; j++)
			{ Buttons[j] = new ButtonsBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ Buttons[j].FromBytes(bytes, i); }
		count = Utils.ubyteToInt(bytes[i[0]++]);
		if(OwnerData == null || OwnerData.length != -1) {
			OwnerData = new OwnerDataBlock[count];
			for(int j = 0; j < count; j++)
			{ OwnerData[j] = new OwnerDataBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ OwnerData[j].FromBytes(bytes, i); }
	}

	public ScriptDialogPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
	{
		this();
		int[] packetEnd = new int[] {bytes.length - 1};
		FromBytes(head, bytes, i, packetEnd);
	}

	@Override
	public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
	{
		this.header =  header;
		Data.FromBytes(bytes, i);
		int count = Utils.ubyteToInt(bytes[i[0]++]);
		if(Buttons == null || Buttons.length != count) {
			Buttons = new ButtonsBlock[count];
			for(int j = 0; j < count; j++)
			{ Buttons[j] = new ButtonsBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ Buttons[j].FromBytes(bytes, i); }
		count = Utils.ubyteToInt(bytes[i[0]++]);
		if(OwnerData == null || OwnerData.length != count) {
			OwnerData = new OwnerDataBlock[count];
			for(int j = 0; j < count; j++)
			{ OwnerData[j] = new OwnerDataBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ OwnerData[j].FromBytes(bytes, i); }
	}

	@Override
	public byte[] ToBytes()
	{
		int length = 10;
		length += Data.getLength();
		length++;
		for (int j = 0; j < Buttons.length; j++) { length += Buttons[j].getLength(); }
		length++;
		for (int j = 0; j < OwnerData.length; j++) { length += OwnerData[j].getLength(); }
		if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
		byte[] bytes = new byte[length];
		int[] i = new int[]{0};
		header.ToBytes(bytes, i);
		Data.ToBytes(bytes, i);
		bytes[i[0]++] = (byte)Buttons.length;
		for (int j = 0; j < Buttons.length; j++) { Buttons[j].ToBytes(bytes, i); }
		bytes[i[0]++] = (byte)OwnerData.length;
		for (int j = 0; j < OwnerData.length; j++) { OwnerData[j].ToBytes(bytes, i); }
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

		fixedLength += Data.getLength();
		byte[] fixedBytes = new byte[fixedLength];
		header.ToBytes(fixedBytes, i);
		Data.ToBytes(fixedBytes, i);
		fixedLength += 2;

		int ButtonsStart = 0;
		int OwnerDataStart = 0;
		do
		{
			int variableLength = 0;
			int ButtonsCount = 0;
			int OwnerDataCount = 0;

			i[0] =ButtonsStart;
			while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < Buttons.length) {
				int blockLength = Buttons[i[0]].getLength();
				if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
					variableLength += blockLength;
					++ButtonsCount;
				}
				else { break; }
				i[0]++;
			}

			i[0] =OwnerDataStart;
			while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < OwnerData.length) {
				int blockLength = OwnerData[i[0]].getLength();
				if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
					variableLength += blockLength;
					++OwnerDataCount;
				}
				else { break; }
				i[0]++;
			}

			byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
			int[] length = new int[] {fixedBytes.length};
			Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
			if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

			packet[length[0]++] = (byte)ButtonsCount;
			for (i[0] = ButtonsStart; i[0] < ButtonsStart + ButtonsCount; i[0]++) { Buttons[i[0]].ToBytes(packet, length); }
			ButtonsStart += ButtonsCount;

			packet[length[0]++] = (byte)OwnerDataCount;
			for (i[0] = OwnerDataStart; i[0] < OwnerDataStart + OwnerDataCount; i[0]++) { OwnerData[i[0]].ToBytes(packet, length); }
			OwnerDataStart += OwnerDataCount;

			if (acksLength[0] > 0) {
				Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
				acksLength[0] = 0;
			}

			packets.add(packet);
		} while (
				ButtonsStart < Buttons.length ||
				OwnerDataStart < OwnerData.length);

		return packets.toArray(new byte[0][0]);
	}
}

/// <exclude/>
