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

import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;
	public final class ScriptSensorRequestPacket extends Packet
    {
        /// <exclude/>
        public static final class RequesterBlock extends PacketBlock
        {
            public UUID SourceID = new UUID();
            public UUID RequestID = new UUID();
            public UUID SearchID = new UUID();
            public Vector3 SearchPos = new Vector3();
            public Quaternion SearchDir = new Quaternion();
		/** Unsigned Byte */ 
		public byte[] SearchName;
            public int Type;
            public float Range;
            public float Arc;
            public BigInteger RegionHandle;
		/** Unsigned Byte */ 
		public byte SearchRegions;

            @Override
			public int getLength()
            {
                                {
                    int length = 94;
                    if (SearchName != null) { length += SearchName.length; }
                    return length;
                }
            }

            public RequesterBlock() { }
            public RequesterBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    SourceID.FromBytes(bytes, i[0]); i[0] += 16;
                    RequestID.FromBytes(bytes, i[0]); i[0] += 16;
                    SearchID.FromBytes(bytes, i[0]); i[0] += 16;
                    SearchPos.fromBytesLit(bytes, i[0]); i[0] += 12;
                    SearchDir.fromBytesLit(bytes, i[0], true); i[0] += 12;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    SearchName = new byte[length];
                    Utils.arraycopy(bytes, i[0], SearchName, 0, length); i[0] +=  length;
                    Type = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    Range = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    Arc = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    RegionHandle = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    SearchRegions = (byte)bytes[i[0]++];
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                SourceID.ToBytes(bytes, i[0]); i[0] += 16;
                RequestID.ToBytes(bytes, i[0]); i[0] += 16;
                SearchID.ToBytes(bytes, i[0]); i[0] += 16;
                SearchPos.toBytesLit(bytes, i[0]); i[0] += 12;
                SearchDir.toBytesLit(bytes, i[0]); i[0] += 12;
                bytes[i[0]++] = (byte)SearchName.length;
                Utils.arraycopy(SearchName, 0, bytes, i[0], SearchName.length); i[0] +=  SearchName.length;
                Utils.intToBytesLit(Type, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(Range, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(Arc, bytes, i[0]); i[0] += 4;
                Utils.ulongToBytesLit(RegionHandle, bytes, i[0]); i[0] += 8;
                bytes[i[0]++] = SearchRegions;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += Requester.getLength();
                return length;
            }
        }
        public RequesterBlock Requester;

        public ScriptSensorRequestPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.ScriptSensorRequest;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 247;
            header.Reliable = true;
            header.Zerocoded = true;
            Requester = new RequesterBlock();
        }

        public ScriptSensorRequestPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            Requester.FromBytes(bytes, i);
        }

        public ScriptSensorRequestPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            Requester.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += Requester.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            Requester.ToBytes(bytes, i);
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
