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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;
public final class ObjectUpdatePacket extends Packet
{
	/// <exclude/>
	public static final class RegionDataBlock extends PacketBlock
	{
		public BigInteger RegionHandle;
		/** Unsigned Short **/
		public int TimeDilation;

		@Override
		public int getLength()
		{
			{
				return 10;
			}
		}

		public RegionDataBlock() { }
		public RegionDataBlock(byte[] bytes, int[] i) throws MalformedDataException
		{
			FromBytes(bytes, i);
		}

		@Override
		public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
		{
			try
			{
				RegionHandle = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
				TimeDilation = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
			}
			catch (Exception e)
			{
				throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
			}
		}

		@Override
		public void ToBytes(byte[] bytes, int[] i)
		{
			Utils.ulongToBytesLit(RegionHandle, bytes, i[0]); i[0] += 8;
			bytes[i[0]++] = (byte)(TimeDilation % 256);
			bytes[i[0]++] = (byte)((TimeDilation >> 8) % 256);
		}

	}

	/// <exclude/>
	public static final class ObjectDataBlock extends PacketBlock
	{
		public long ID;
		/** Unsigned Byte */ 
		public byte State;
		public UUID FullID = new UUID();
		public long CRC;
		/** Unsigned Byte */ 
		public byte PCode;
		/** Unsigned Byte */ 
		public byte Material;
		/** Unsigned Byte */ 
		public byte ClickAction;
		public Vector3 Scale = new Vector3();
		/** Unsigned Byte */ 
		public byte[] ObjectData;
		public long ParentID;
		public long UpdateFlags;
		/** Unsigned Byte */ 
		public byte PathCurve;
		/** Unsigned Byte */ 
		public byte ProfileCurve;
		/** Unsigned Short **/
		public int PathBegin;
		/** Unsigned Short **/
		public int PathEnd;
		/** Unsigned Byte */ 
		public byte PathScaleX;
		/** Unsigned Byte */ 
		public byte PathScaleY;
		/** Unsigned Byte */ 
		public byte PathShearX;
		/** Unsigned Byte */ 
		public byte PathShearY;
		/** Signed Byte */ 
		public byte PathTwist;
		/** Signed Byte */ 
		public byte PathTwistBegin;
		/** Signed Byte */ 
		public byte PathRadiusOffset;
		/** Signed Byte */ 
		public byte PathTaperX;
		/** Signed Byte */ 
		public byte PathTaperY;
		/** Unsigned Byte */ 
		public byte PathRevolutions;
		/** Signed Byte */ 
		public byte PathSkew;
		/** Unsigned Short **/
		public int ProfileBegin;
		/** Unsigned Short **/
		public int ProfileEnd;
		/** Unsigned Short **/
		public int ProfileHollow;
		/** Unsigned Byte */ 
		public byte[] TextureEntry;
		/** Unsigned Byte */ 
		public byte[] TextureAnim;
		/** Unsigned Byte */ 
		public byte[] NameValue;
		/** Unsigned Byte */ 
		public byte[] Data;
		/** Unsigned Byte */ 
		public byte[] Text;
		/** Unsigned Byte */ 
		public byte[] TextColor;
		/** Unsigned Byte */ 
		public byte[] MediaURL;
		/** Unsigned Byte */ 
		public byte[] PSBlock;
		/** Unsigned Byte */ 
		public byte[] ExtraParams;
		public UUID Sound = new UUID();
		public UUID OwnerID = new UUID();
		public float Gain;
		/** Unsigned Byte */ 
		public byte Flags;
		public float Radius;
		/** Unsigned Byte */ 
		public byte JointType;
		public Vector3 JointPivot = new Vector3();
		public Vector3 JointAxisOrAnchor = new Vector3();

		@Override
		public int getLength()
		{
			{
				int length = 153;
				if (ObjectData != null) { length += ObjectData.length; }
				if (TextureEntry != null) { length += TextureEntry.length; }
				if (TextureAnim != null) { length += TextureAnim.length; }
				if (NameValue != null) { length += NameValue.length; }
				if (Data != null) { length += Data.length; }
				if (Text != null) { length += Text.length; }
				if (MediaURL != null) { length += MediaURL.length; }
				if (PSBlock != null) { length += PSBlock.length; }
				if (ExtraParams != null) { length += ExtraParams.length; }
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
				ID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
				State = (byte)bytes[i[0]++];
				FullID.FromBytes(bytes, i[0]); i[0] += 16;
				CRC = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
				PCode = (byte)bytes[i[0]++];
				Material = (byte)bytes[i[0]++];
				ClickAction = (byte)bytes[i[0]++];
				Scale.fromBytesLit(bytes, i[0]); i[0] += 12;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				ObjectData = new byte[length];
				Utils.arraycopy(bytes, i[0], ObjectData, 0, length); i[0] +=  length;
				ParentID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
				UpdateFlags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
				PathCurve = (byte)bytes[i[0]++];
				ProfileCurve = (byte)bytes[i[0]++];
				PathBegin = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				PathEnd = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				PathScaleX = (byte)bytes[i[0]++];
				PathScaleY = (byte)bytes[i[0]++];
				PathShearX = (byte)bytes[i[0]++];
				PathShearY = (byte)bytes[i[0]++];
				PathTwist = (byte)bytes[i[0]++];
				PathTwistBegin = (byte)bytes[i[0]++];
				PathRadiusOffset = (byte)bytes[i[0]++];
				PathTaperX = (byte)bytes[i[0]++];
				PathTaperY = (byte)bytes[i[0]++];
				PathRevolutions = (byte)bytes[i[0]++];
				PathSkew = (byte)bytes[i[0]++];
				ProfileBegin = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				ProfileEnd = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				ProfileHollow = (int)Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				TextureEntry = new byte[length];
				Utils.arraycopy(bytes, i[0], TextureEntry, 0, length); i[0] +=  length;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				TextureAnim = new byte[length];
				Utils.arraycopy(bytes, i[0], TextureAnim, 0, length); i[0] +=  length;
				length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				NameValue = new byte[length];
				Utils.arraycopy(bytes, i[0], NameValue, 0, length); i[0] +=  length;
				length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
				Data = new byte[length];
				Utils.arraycopy(bytes, i[0], Data, 0, length); i[0] +=  length;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				Text = new byte[length];
				Utils.arraycopy(bytes, i[0], Text, 0, length); i[0] +=  length;
				TextColor = new byte[4];
				Utils.arraycopy(bytes, i[0], TextColor, 0, 4); i[0] += 4;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				MediaURL = new byte[length];
				Utils.arraycopy(bytes, i[0], MediaURL, 0, length); i[0] +=  length;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				PSBlock = new byte[length];
				Utils.arraycopy(bytes, i[0], PSBlock, 0, length); i[0] +=  length;
				length = Utils.ubyteToInt(bytes[i[0]++]);
				ExtraParams = new byte[length];
				Utils.arraycopy(bytes, i[0], ExtraParams, 0, length); i[0] +=  length;
				Sound.FromBytes(bytes, i[0]); i[0] += 16;
				OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
				Gain = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
				Flags = (byte)bytes[i[0]++];
				Radius = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
				JointType = (byte)bytes[i[0]++];
				JointPivot.fromBytesLit(bytes, i[0]); i[0] += 12;
				JointAxisOrAnchor.fromBytesLit(bytes, i[0]); i[0] += 12;
			}
			catch (Exception e)
			{
				throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
			}
		}

		@Override
		public void ToBytes(byte[] bytes, int[] i)
		{
			Utils.uintToBytesLit(ID, bytes, i[0]); i[0] += 4;
			bytes[i[0]++] = State;
			FullID.ToBytes(bytes, i[0]); i[0] += 16;
			Utils.uintToBytesLit(CRC, bytes, i[0]); i[0] += 4;
			bytes[i[0]++] = PCode;
			bytes[i[0]++] = Material;
			bytes[i[0]++] = ClickAction;
			Scale.toBytesLit(bytes, i[0]); i[0] += 12;
			bytes[i[0]++] = (byte)ObjectData.length;
			Utils.arraycopy(ObjectData, 0, bytes, i[0], ObjectData.length); i[0] +=  ObjectData.length;
			Utils.uintToBytesLit(ParentID, bytes, i[0]); i[0] += 4;
			Utils.uintToBytesLit(UpdateFlags, bytes, i[0]); i[0] += 4;
			bytes[i[0]++] = PathCurve;
			bytes[i[0]++] = ProfileCurve;
			bytes[i[0]++] = (byte)(PathBegin % 256);
			bytes[i[0]++] = (byte)((PathBegin >> 8) % 256);
			bytes[i[0]++] = (byte)(PathEnd % 256);
			bytes[i[0]++] = (byte)((PathEnd >> 8) % 256);
			bytes[i[0]++] = PathScaleX;
			bytes[i[0]++] = PathScaleY;
			bytes[i[0]++] = PathShearX;
			bytes[i[0]++] = PathShearY;
			bytes[i[0]++] = (byte)PathTwist;
			bytes[i[0]++] = (byte)PathTwistBegin;
			bytes[i[0]++] = (byte)PathRadiusOffset;
			bytes[i[0]++] = (byte)PathTaperX;
			bytes[i[0]++] = (byte)PathTaperY;
			bytes[i[0]++] = PathRevolutions;
			bytes[i[0]++] = (byte)PathSkew;
			bytes[i[0]++] = (byte)(ProfileBegin % 256);
			bytes[i[0]++] = (byte)((ProfileBegin >> 8) % 256);
			bytes[i[0]++] = (byte)(ProfileEnd % 256);
			bytes[i[0]++] = (byte)((ProfileEnd >> 8) % 256);
			bytes[i[0]++] = (byte)(ProfileHollow % 256);
			bytes[i[0]++] = (byte)((ProfileHollow >> 8) % 256);
			bytes[i[0]++] = (byte)(TextureEntry.length % 256);
			bytes[i[0]++] = (byte)((TextureEntry.length >> 8) % 256);
			Utils.arraycopy(TextureEntry, 0, bytes, i[0], TextureEntry.length); i[0] +=  TextureEntry.length;
			bytes[i[0]++] = (byte)TextureAnim.length;
			Utils.arraycopy(TextureAnim, 0, bytes, i[0], TextureAnim.length); i[0] +=  TextureAnim.length;
			bytes[i[0]++] = (byte)(NameValue.length % 256);
			bytes[i[0]++] = (byte)((NameValue.length >> 8) % 256);
			Utils.arraycopy(NameValue, 0, bytes, i[0], NameValue.length); i[0] +=  NameValue.length;
			bytes[i[0]++] = (byte)(Data.length % 256);
			bytes[i[0]++] = (byte)((Data.length >> 8) % 256);
			Utils.arraycopy(Data, 0, bytes, i[0], Data.length); i[0] +=  Data.length;
			bytes[i[0]++] = (byte)Text.length;
			Utils.arraycopy(Text, 0, bytes, i[0], Text.length); i[0] +=  Text.length;
			Utils.arraycopy(TextColor, 0, bytes, i[0], 4);i[0] += 4;
			bytes[i[0]++] = (byte)MediaURL.length;
			Utils.arraycopy(MediaURL, 0, bytes, i[0], MediaURL.length); i[0] +=  MediaURL.length;
			bytes[i[0]++] = (byte)PSBlock.length;
			Utils.arraycopy(PSBlock, 0, bytes, i[0], PSBlock.length); i[0] +=  PSBlock.length;
			bytes[i[0]++] = (byte)ExtraParams.length;
			Utils.arraycopy(ExtraParams, 0, bytes, i[0], ExtraParams.length); i[0] +=  ExtraParams.length;
			Sound.ToBytes(bytes, i[0]); i[0] += 16;
			OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
			Utils.floatToBytesLit(Gain, bytes, i[0]); i[0] += 4;
			bytes[i[0]++] = Flags;
			Utils.floatToBytesLit(Radius, bytes, i[0]); i[0] += 4;
			bytes[i[0]++] = JointType;
			JointPivot.toBytesLit(bytes, i[0]); i[0] += 12;
			JointAxisOrAnchor.toBytesLit(bytes, i[0]); i[0] += 12;
		}

	}

	@Override
	public int getLength()
	{
		{
			int length = 8;
			length += RegionData.getLength();
			for (int j = 0; j < ObjectData.length; j++)
				length += ObjectData[j].getLength();
			return length;
		}
	}
	public RegionDataBlock RegionData;
	public ObjectDataBlock[] ObjectData;

	public ObjectUpdatePacket()
	{
		HasVariableBlocks = true;
		Type = PacketType.ObjectUpdate;
		this.header =  new Header();
		header.Frequency = PacketFrequency.High;
		header.ID = 12;
		header.Reliable = true;
		header.Zerocoded = true;
		RegionData = new RegionDataBlock();
		ObjectData = null;
	}

	public ObjectUpdatePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
		RegionData.FromBytes(bytes, i);
		int count = Utils.ubyteToInt(bytes[i[0]++]);
		if(ObjectData == null || ObjectData.length != -1) {
			ObjectData = new ObjectDataBlock[count];
			for(int j = 0; j < count; j++)
			{ ObjectData[j] = new ObjectDataBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ ObjectData[j].FromBytes(bytes, i); }
	}

	public ObjectUpdatePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
	{
		this();
		int[] packetEnd = new int[] {bytes.length - 1};
		FromBytes(head, bytes, i, packetEnd);
	}

	@Override
	public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
	{
		this.header =  header;
		RegionData.FromBytes(bytes, i);
		int count = Utils.ubyteToInt(bytes[i[0]++]);
		if(ObjectData == null || ObjectData.length != count) {
			ObjectData = new ObjectDataBlock[count];
			for(int j = 0; j < count; j++)
			{ ObjectData[j] = new ObjectDataBlock(); }
		}
		for (int j = 0; j < count; j++)
		{ ObjectData[j].FromBytes(bytes, i); }
	}

	@Override
	public byte[] ToBytes()
	{
		int length = 7;
		length += RegionData.getLength();
		length++;
		for (int j = 0; j < ObjectData.length; j++) { length += ObjectData[j].getLength(); }
		if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
		byte[] bytes = new byte[length];
		int[] i = new int[]{0};
		header.ToBytes(bytes, i);
		RegionData.ToBytes(bytes, i);
		bytes[i[0]++] = (byte)ObjectData.length;
		for (int j = 0; j < ObjectData.length; j++) { ObjectData[j].ToBytes(bytes, i); }
		if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
		return bytes;
	}

	@Override
	public byte[][] ToBytesMultiple()
	{
		List<byte[]> packets = new ArrayList<byte[]>();
		int[] i = new int[]{0};
		int fixedLength = 7;

		byte[] ackBytes = null;
		int[] acksLength = new int[]{0};
		if (header.AckList != null && header.AckList.length > 0) {
			header.AppendedAcks = true;
			ackBytes = new byte[header.AckList.length * 4 + 1];
			header.AcksToBytes(ackBytes, acksLength);
		}

		fixedLength += RegionData.getLength();
		byte[] fixedBytes = new byte[fixedLength];
		header.ToBytes(fixedBytes, i);
		RegionData.ToBytes(fixedBytes, i);
		fixedLength += 1;

		int ObjectDataStart = 0;
		do
		{
			int variableLength = 0;
			int ObjectDataCount = 0;

			i[0] =ObjectDataStart;
			while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < ObjectData.length) {
				int blockLength = ObjectData[i[0]].getLength();
				if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
					variableLength += blockLength;
					++ObjectDataCount;
				}
				else { break; }
				i[0]++;
			}

			byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
			int[] length = new int[] {fixedBytes.length};
			Utils.arraycopy(fixedBytes, 0, packet, 0, length[0]);
			if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

			packet[length[0]++] = (byte)ObjectDataCount;
			for (i[0] = ObjectDataStart; i[0] < ObjectDataStart + ObjectDataCount; i[0]++) { ObjectData[i[0]].ToBytes(packet, length); }
			ObjectDataStart += ObjectDataCount;

			if (acksLength[0] > 0) {
				Utils.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
				acksLength[0] = 0;
			}

			packets.add(packet);
		} while (
				ObjectDataStart < ObjectData.length);

		return packets.toArray(new byte[0][0]);
	}
}

/// <exclude/>
