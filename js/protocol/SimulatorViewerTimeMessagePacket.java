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
	public final class SimulatorViewerTimeMessagePacket extends Packet
    {
        /// <exclude/>
        public static final class TimeInfoBlock extends PacketBlock
        {
            public BigInteger UsecSinceStart;
            public long SecPerDay;
            public long SecPerYear;
            public Vector3 SunDirection = new Vector3();
            public float SunPhase;
            public Vector3 SunAngVelocity = new Vector3();

            @Override
			public int getLength()
            {
                                {
                    return 44;
                }
            }

            public TimeInfoBlock() { }
            public TimeInfoBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    UsecSinceStart = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                    SecPerDay = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    SecPerYear = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    SunDirection.fromBytesLit(bytes, i[0]); i[0] += 12;
                    SunPhase = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    SunAngVelocity.fromBytesLit(bytes, i[0]); i[0] += 12;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.ulongToBytesLit(UsecSinceStart, bytes, i[0]); i[0] += 8;
                Utils.uintToBytesLit(SecPerDay, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(SecPerYear, bytes, i[0]); i[0] += 4;
                SunDirection.toBytesLit(bytes, i[0]); i[0] += 12;
                Utils.floatToBytesLit(SunPhase, bytes, i[0]); i[0] += 4;
                SunAngVelocity.toBytesLit(bytes, i[0]); i[0] += 12;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += TimeInfo.getLength();
                return length;
            }
        }
        public TimeInfoBlock TimeInfo;

        public SimulatorViewerTimeMessagePacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.SimulatorViewerTimeMessage;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 150;
            header.Reliable = true;
            TimeInfo = new TimeInfoBlock();
        }

        public SimulatorViewerTimeMessagePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            TimeInfo.FromBytes(bytes, i);
        }

        public SimulatorViewerTimeMessagePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            TimeInfo.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += TimeInfo.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            TimeInfo.ToBytes(bytes, i);
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
