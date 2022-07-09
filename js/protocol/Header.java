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

import java.io.Serializable;
import java.util.logging.Logger;
import com.ngt.jopenmetaverse.shared.util.Utils;


/// <summary>
/// The header of a message template packet. Holds packet flags, sequence
/// number, packet ID, and any ACKs that will be appended at the end of
/// the packet
/// </summary>
public class Header implements Serializable
{
	protected  static Logger logger = Logger.getLogger("Header"); 

	public boolean Reliable;
	public boolean Resent;
	public boolean Zerocoded;
	public boolean AppendedAcks;
	public long Sequence; //Actually a uint, need only 4 unsigned bytes
	public int ID; //Unsigned Short
	public PacketFrequency Frequency;
	public long[] AckList; //array of uint

	public void ToBytes(byte[] bytes, int[] i)
	{
		byte flags = 0;
		if (Reliable) flags |= Helpers.MSG_RELIABLE;
		if (Resent) flags |= Helpers.MSG_RESENT;
		if (Zerocoded) flags |= Helpers.MSG_ZEROCODED;
		if (AppendedAcks) flags |= Helpers.MSG_APPENDED_ACKS;

		// Flags
		bytes[i[0]++] = flags;

		// Sequence number

		//need to take only 4 bytes out of 8 bytes of long
		Utils.uintToBytes(Sequence, bytes, i[0]);
		i[0] += 4;

		// Extra byte
		bytes[i[0]++] = 0;

		// Packet ID
		switch (Frequency)
		{
		case High:
			// 1 byte ID
			bytes[i[0]++] = (byte)ID;
			break;
		case Medium:
			// 2 byte ID
			bytes[i[0]++] = (byte)0xFF;
			bytes[i[0]++] = (byte)ID;
			break;
		case Low:
			// 4 byte ID
			bytes[i[0]++] = (byte)0xFF;
			bytes[i[0]++] = (byte)0xFF;
			Utils.int16ToBytes((short)ID, bytes, i[0]);
			i[0] += 2;
			break;
		}
	}

	public void FromBytes(byte[] bytes, int pos[], int packetEnd[])
	{
		Header header = BuildHeader(bytes, pos, packetEnd);
		this.AppendedAcks = header.AppendedAcks;
		this.Reliable = header.Reliable;
		this.Resent = header.Resent;
		this.Zerocoded = header.Zerocoded;
		this.Sequence = header.Sequence;
	}

	/// <summary>
	/// Convert the AckList to a byte array, used for packet serializing
	/// </summary>
	/// <param name="bytes">Reference to the target byte array</param>
	/// <param name="i">Beginning position to start writing to in the byte
	/// array, will be updated with the ending position of the ACK list</param>
	public void AcksToBytes(byte[] bytes, int i[])
	{
		for(int j=0; j < AckList.length; j++ )
		{
			Utils.uintToBytes(AckList[j], bytes, i[0]);
			i[0] += 4;
		}
		if (AckList.length > 0) { bytes[i[0]++] = (byte)AckList.length; }
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="bytes"></param>
	/// <param name="pos"></param>
	/// <param name="packetEnd"></param>
	/// <returns></returns>
	public static Header BuildHeader(byte[] bytes, int[] pos, int[] packetEnd)
	{
		//        	int[] packetEnd = new int[] {packetEnd2};
		Header header = new Header();
		byte flags = bytes[pos[0]];

		header.AppendedAcks = (flags & Helpers.MSG_APPENDED_ACKS) != 0;
		header.Reliable = (flags & Helpers.MSG_RELIABLE) != 0;
		header.Resent = (flags & Helpers.MSG_RESENT) != 0;
		header.Zerocoded = (flags & Helpers.MSG_ZEROCODED) != 0;
		//            header.Sequence = (long)((bytes[pos[0] + 1] << 24) + (bytes[pos[0] + 2] << 16) + (bytes[pos[0] + 3] << 8) + bytes[pos[0] + 4]);
		header.Sequence = (long)Utils.bytesToUInt(bytes, pos[0]+1);

		// Set the frequency and packet ID number
		if (bytes[pos[0] + 6] == (byte)0xFF)
		{
			if (bytes[pos[0] + 7] == (byte)0xFF)
			{
				header.Frequency = PacketFrequency.Low;
				if (header.Zerocoded && bytes[pos[0] + 8] == 0)
					header.ID = Utils.ubyteToInt(bytes[pos[0] + 10]);
				else
					//                        header.ID = (int)((bytes[pos[0] + 8] << 8) + bytes[pos[0] + 9]);
					header.ID = (int) Utils.bytesToUInt16(bytes, pos[0] + 8);

				pos[0] += 10;
			}
			else
			{
				header.Frequency = PacketFrequency.Medium;
				header.ID = Utils.ubyteToInt(bytes[pos[0] + 7]);

				pos[0] += 8;
			}
		}
		else
		{
			header.Frequency = PacketFrequency.High;
			header.ID = Utils.ubyteToInt(bytes[pos[0] + 6]);

			pos[0] += 7;
		}

		header.AckList = null;
		CreateAckList(header, bytes, packetEnd);

		return header;
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="header"></param>
	/// <param name="bytes"></param>
	/// <param name="packetEnd"></param>
	static void CreateAckList(Header header, byte[] bytes, int[] packetEnd)
	{
		if (header.AppendedAcks)
		{
			int count = Utils.ubyteToInt(bytes[packetEnd[0]--]);
			header.AckList = new long[count];

			for (int i = 0; i < count; i++)
			{
				//                    header.AckList[i] = (long)(
						//                        (bytes[(packetEnd[0] - i * 4) - 3] << 24) |
				//                        (bytes[(packetEnd[0] - i * 4) - 2] << 16) |
				//                        (bytes[(packetEnd[0] - i * 4) - 1] <<  8) |
				//                        (bytes[(packetEnd[0] - i * 4)    ]));
				header.AckList[i] = Utils.bytesToUInt(bytes, packetEnd[0] - i * 4 -3);
			}

			packetEnd[0] -= (count * 4);
		}
	}

//	public static OSD GetLLSD(Header packet) throws IllegalArgumentException, IllegalAccessException
//	{	
//		Field[] fields = packet.getClass().getFields();
//		OSDMap map = new OSDMap(fields.length);      
//		for (int i = 0; i < fields.length; i++)
//		{
//			Field field = fields[i];
//			logger.info(field.getName());
//			if (field.get(packet) instanceof Object[])
//			{
//				Object[] oarray = (Object[])field.get(packet);
//				OSDArray osdArray = new OSDArray(oarray.length);
//				for(int j=0; j< oarray.length; j++)
//				{
//					OSD osd = OSD.FromObject(oarray[j]); 
//					osdArray.add(osd);
//				}
//				map.put(field.getName(), osdArray);
//			}
//			else if(!field.get(packet).getClass().isEnum())
//			{
//				OSD osd = OSD.FromObject(field.get(packet)); 
//				map.put(field.getName(), osd);
//			}
//			OSD osd = OSD.FromObject(packet.Frequency.getIndex()); 
//			map.put("Frequency", osd);
//		}
//		return map;
//	}
}
