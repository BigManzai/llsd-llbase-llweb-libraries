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

import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;

public final class SoundTriggerPacket extends Packet
{
    /// <exclude/>
    public static final class SoundDataBlock extends PacketBlock
    {
        public UUID SoundID = new UUID();
        public UUID OwnerID = new UUID();
        public UUID ObjectID = new UUID();
        public UUID ParentID = new UUID();
        /** Unsigned Long */
        public BigInteger Handle;
        public Vector3 Position = new Vector3();
        public float Gain;

        @Override
		public  int getLength()
        {
            {
                return 88;
            }
        }

        public SoundDataBlock() { }
        public SoundDataBlock(byte[] bytes, int[] i) throws MalformedDataException
        {
            FromBytes(bytes,  i);
        }

        @Override
		public  void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
        {
            try
            {
                SoundID.FromBytes(bytes, i[0]); i[0] += 16;
                OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                ObjectID.FromBytes(bytes, i[0]); i[0] += 16;
                ParentID.FromBytes(bytes, i[0]); i[0] += 16;
                Handle = Utils.bytesToULongLit(bytes, i[0]); i[0] += 8;
                Position.fromBytesLit(bytes, i[0]); i[0] += 12;
                Gain = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
            }
            catch (Exception e)
            {
                throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
            }
        }

        @Override
		public  void ToBytes(byte[] bytes, int[] i)
        {
            SoundID.ToBytes(bytes, i[0]); i[0] += 16;
            OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
            ObjectID.ToBytes(bytes, i[0]); i[0] += 16;
            ParentID.ToBytes(bytes, i[0]); i[0] += 16;
            Utils.ulongToBytesLit(Handle, bytes, i[0]); i[0] += 8;
            Position.toBytesLit(bytes, i[0]); i[0] += 12;
            Utils.floatToBytesLit(Gain, bytes, i[0]); i[0] += 4;
        }

    }

    @Override
		public  int getLength()
    {
            int length = 7;
            length += SoundData.getLength();
            return length;
    }
    public SoundDataBlock SoundData;

    public SoundTriggerPacket()
    {
        HasVariableBlocks = false;
        Type = PacketType.SoundTrigger;
        header = new Header();
        header.Frequency = PacketFrequency.High;
        header.ID = 29;
        header.Reliable = true;
        SoundData = new SoundDataBlock();
    }

    public SoundTriggerPacket(byte[] bytes, int[] i) throws MalformedDataException 
    {
    	this();
        int[] packetEnd = new int[] {bytes.length - 1};
        FromBytes(bytes,  i,  packetEnd, null);
    }

    @Override
    public void FromBytes(byte[] bytes, int[] i, int[] packetEnd, byte[] zeroBuffer) throws MalformedDataException
    {
        header.FromBytes(bytes,  i,  packetEnd);
        if (header.Zerocoded && zeroBuffer != null)
        {
            packetEnd[0] = Helpers.ZeroDecode(bytes, packetEnd[0] + 1, zeroBuffer) - 1;
            bytes = zeroBuffer;
        }
        SoundData.FromBytes(bytes,  i);
    }

    public SoundTriggerPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
    {
    	this();
        int[] packetEnd = new int[] {bytes.length - 1};
        FromBytes(head, bytes,  i,  packetEnd);
    }

    @Override
    public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
    {
        this.header = header;
        SoundData.FromBytes(bytes,  i);
    }

    @Override
		public  byte[] ToBytes()
    {
        int length = 7;
        length += SoundData.getLength();
        if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
        byte[] bytes = new byte[length];
        int[] i = new int[]{0};
        header.ToBytes(bytes,  i);
        SoundData.ToBytes(bytes,  i);
        if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes,  i); }
        return bytes;
    }

    @Override
		public  byte[][] ToBytesMultiple()
    {
        return new byte[][] { ToBytes() };
    }
}

