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
	public final class RegionHandshakePacket extends Packet
    {
        /// <exclude/>
        public static final class RegionInfoBlock extends PacketBlock
        {
            public long RegionFlags;
		/** Unsigned Byte */ 
		public byte SimAccess;
		/** Unsigned Byte */ 
		public byte[] SimName;
            public UUID SimOwner = new UUID();
            public boolean IsEstateManager;
            public float WaterHeight;
            public float BillableFactor;
            public UUID CacheID = new UUID();
            public UUID TerrainBase0 = new UUID();
            public UUID TerrainBase1 = new UUID();
            public UUID TerrainBase2 = new UUID();
            public UUID TerrainBase3 = new UUID();
            public UUID TerrainDetail0 = new UUID();
            public UUID TerrainDetail1 = new UUID();
            public UUID TerrainDetail2 = new UUID();
            public UUID TerrainDetail3 = new UUID();
            public float TerrainStartHeight00;
            public float TerrainStartHeight01;
            public float TerrainStartHeight10;
            public float TerrainStartHeight11;
            public float TerrainHeightRange00;
            public float TerrainHeightRange01;
            public float TerrainHeightRange10;
            public float TerrainHeightRange11;

            @Override
			public int getLength()
            {
                                {
                    int length = 207;
                    if (SimName != null) { length += SimName.length; }
                    return length;
                }
            }

            public RegionInfoBlock() { }
            public RegionInfoBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    RegionFlags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    SimAccess = (byte)bytes[i[0]++];
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    SimName = new byte[length];
                    Utils.arraycopy(bytes, i[0], SimName, 0, length); i[0] +=  length;
                    SimOwner.FromBytes(bytes, i[0]); i[0] += 16;
                    IsEstateManager = (bytes[i[0]++] != 0) ? true : false;
                    WaterHeight = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    BillableFactor = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    CacheID.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainBase0.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainBase1.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainBase2.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainBase3.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainDetail0.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainDetail1.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainDetail2.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainDetail3.FromBytes(bytes, i[0]); i[0] += 16;
                    TerrainStartHeight00 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainStartHeight01 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainStartHeight10 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainStartHeight11 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainHeightRange00 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainHeightRange01 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainHeightRange10 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    TerrainHeightRange11 = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.uintToBytesLit(RegionFlags, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = SimAccess;
                bytes[i[0]++] = (byte)SimName.length;
                Utils.arraycopy(SimName, 0, bytes, i[0], SimName.length); i[0] +=  SimName.length;
                SimOwner.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)((IsEstateManager) ? 1 : 0);
                Utils.floatToBytesLit(WaterHeight, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(BillableFactor, bytes, i[0]); i[0] += 4;
                CacheID.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainBase0.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainBase1.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainBase2.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainBase3.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainDetail0.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainDetail1.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainDetail2.ToBytes(bytes, i[0]); i[0] += 16;
                TerrainDetail3.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.floatToBytesLit(TerrainStartHeight00, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainStartHeight01, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainStartHeight10, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainStartHeight11, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainHeightRange00, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainHeightRange01, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainHeightRange10, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TerrainHeightRange11, bytes, i[0]); i[0] += 4;
            }

        }

        /// <exclude/>
        public static final class RegionInfo2Block extends PacketBlock
        {
            public UUID RegionID = new UUID();

            @Override
			public int getLength()
            {
                                {
                    return 16;
                }
            }

            public RegionInfo2Block() { }
            public RegionInfo2Block(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    RegionID.FromBytes(bytes, i[0]); i[0] += 16;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                RegionID.ToBytes(bytes, i[0]); i[0] += 16;
            }

        }

        /// <exclude/>
        public static final class RegionInfo3Block extends PacketBlock
        {
            public int CPUClassID;
            public int CPURatio;
		/** Unsigned Byte */ 
		public byte[] ColoName;
		/** Unsigned Byte */ 
		public byte[] ProductSKU;
		/** Unsigned Byte */ 
		public byte[] ProductName;

            @Override
			public int getLength()
            {
                                {
                    int length = 11;
                    if (ColoName != null) { length += ColoName.length; }
                    if (ProductSKU != null) { length += ProductSKU.length; }
                    if (ProductName != null) { length += ProductName.length; }
                    return length;
                }
            }

            public RegionInfo3Block() { }
            public RegionInfo3Block(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    CPUClassID = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    CPURatio = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    ColoName = new byte[length];
                    Utils.arraycopy(bytes, i[0], ColoName, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    ProductSKU = new byte[length];
                    Utils.arraycopy(bytes, i[0], ProductSKU, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    ProductName = new byte[length];
                    Utils.arraycopy(bytes, i[0], ProductName, 0, length); i[0] +=  length;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.intToBytesLit(CPUClassID, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(CPURatio, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)ColoName.length;
                Utils.arraycopy(ColoName, 0, bytes, i[0], ColoName.length); i[0] +=  ColoName.length;
                bytes[i[0]++] = (byte)ProductSKU.length;
                Utils.arraycopy(ProductSKU, 0, bytes, i[0], ProductSKU.length); i[0] +=  ProductSKU.length;
                bytes[i[0]++] = (byte)ProductName.length;
                Utils.arraycopy(ProductName, 0, bytes, i[0], ProductName.length); i[0] +=  ProductName.length;
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 10;
                length += RegionInfo.getLength();
                length += RegionInfo2.getLength();
                length += RegionInfo3.getLength();
                return length;
            }
        }
        public RegionInfoBlock RegionInfo;
        public RegionInfo2Block RegionInfo2;
        public RegionInfo3Block RegionInfo3;

        public RegionHandshakePacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.RegionHandshake;
            this.header =  new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 148;
            header.Reliable = true;
            header.Zerocoded = true;
            RegionInfo = new RegionInfoBlock();
            RegionInfo2 = new RegionInfo2Block();
            RegionInfo3 = new RegionInfo3Block();
        }

        public RegionHandshakePacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            RegionInfo.FromBytes(bytes, i);
            RegionInfo2.FromBytes(bytes, i);
            RegionInfo3.FromBytes(bytes, i);
        }

        public RegionHandshakePacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            RegionInfo.FromBytes(bytes, i);
            RegionInfo2.FromBytes(bytes, i);
            RegionInfo3.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 10;
            length += RegionInfo.getLength();
            length += RegionInfo2.getLength();
            length += RegionInfo3.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            RegionInfo.ToBytes(bytes, i);
            RegionInfo2.ToBytes(bytes, i);
            RegionInfo3.ToBytes(bytes, i);
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
