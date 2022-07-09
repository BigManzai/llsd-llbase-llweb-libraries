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

import com.ngt.jopenmetaverse.shared.util.*;
import com.ngt.jopenmetaverse.shared.protocol.MalformedDataException;

public final class EconomyDataPacket extends Packet
    {
        public final class InfoBlock extends PacketBlock
        {
            public int ObjectCapacity;
            public int ObjectCount;
            public int PriceEnergyUnit;
            public int PriceObjectClaim;
            public int PricePublicObjectDecay;
            public int PricePublicObjectDelete;
            public int PriceParcelClaim;
            public float PriceParcelClaimFactor;
            public int PriceUpload;
            public int PriceRentLight;
            public int TeleportMinPrice;
            public float TeleportPriceExponent;
            public float EnergyEfficiency;
            public float PriceObjectRent;
            public float PriceObjectScaleFactor;
            public int PriceParcelRent;
            public int PriceGroupCreate;

            @Override
            public int getLength()
            {
                    return 68;
            }

            public InfoBlock() { }
            
            public InfoBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
            public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    ObjectCapacity = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ObjectCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PriceEnergyUnit = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PriceObjectClaim = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PricePublicObjectDecay = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PricePublicObjectDelete = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PriceParcelClaim = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PriceParcelClaimFactor = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    PriceUpload = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PriceRentLight = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    TeleportMinPrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    TeleportPriceExponent = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    EnergyEfficiency = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    PriceObjectRent = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    PriceObjectScaleFactor = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    PriceParcelRent = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PriceGroupCreate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
            public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.intToBytesLit(ObjectCapacity, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ObjectCount, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceEnergyUnit, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceObjectClaim, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PricePublicObjectDecay, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PricePublicObjectDelete, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceParcelClaim, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(PriceParcelClaimFactor, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceUpload, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceRentLight, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(TeleportMinPrice, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(TeleportPriceExponent, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(EnergyEfficiency, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(PriceObjectRent, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(PriceObjectScaleFactor, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceParcelRent, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PriceGroupCreate, bytes, i[0]); i[0] += 4;
            }

        }

        @Override
        public int getLength()
        {
                int length = 10;
                length += info.getLength();
                return length;
        }
        public InfoBlock info;

        public EconomyDataPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.EconomyData;
            header = new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 25;
            header.Reliable = true;
            header.Zerocoded = true;
            info = new InfoBlock();
        }

        public EconomyDataPacket(byte[] bytes, int[] i) throws MalformedDataException
        {
        	this();
            int[] packetEnd = new int[1];
            packetEnd[0]= bytes.length - 1;
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
            try {
				info.FromBytes(bytes, i);
			} catch (MalformedDataException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }

        public EconomyDataPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
        {
        	this();
        	int[] packetEnd = new int[1];
        	packetEnd[0] = bytes.length - 1;
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
        public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header = header;
            try {
				info.FromBytes(bytes, i);
			} catch (MalformedDataException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
        }

        @Override
        public byte[] ToBytes()
        {
            int length = 10;
            length += info.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[1];
            i[0]= 0;
            header.ToBytes(bytes, i);
            info.ToBytes(bytes, i);
            if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
            return bytes;
        }

        @Override
        public byte[][] ToBytesMultiple()
        {
            return new byte[][] { ToBytes() };
        }
    }
