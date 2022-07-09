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

import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;


    public final class ParcelPropertiesPacket extends Packet
    {
        public final class ParcelDataBlock extends PacketBlock
        {
            public int RequestResult;
            public int SequenceID;
            public boolean SnapSelection;
            public int SelfCount;
            public int OtherCount;
            public int PublicCount;
            public int LocalID;
            public UUID OwnerID = new UUID();
            public boolean IsGroupOwned;
            public long AuctionID;
            public int ClaimDate;
            public int ClaimPrice;
            public int RentPrice;
            public Vector3 AABBMin = new Vector3();
            public Vector3 AABBMax = new Vector3();
		/** Unsigned Byte */ 
		public byte[] Bitmap;
            public int Area;
		/** Unsigned Byte */ 
		public byte Status;
            public int SimWideMaxPrims;
            public int SimWideTotalPrims;
            public int MaxPrims;
            public int TotalPrims;
            public int OwnerPrims;
            public int GroupPrims;
            public int OtherPrims;
            public int SelectedPrims;
            public float ParcelPrimBonus;
            public int OtherCleanTime;
            public long ParcelFlags;
            public int SalePrice;
		/** Unsigned Byte */ 
		public byte[] Name;
		/** Unsigned Byte */ 
		public byte[] Desc;
		/** Unsigned Byte */ 
		public byte[] MusicURL;
		/** Unsigned Byte */ 
		public byte[] MediaURL;
            public UUID MediaID = new UUID();
		/** Unsigned Byte */ 
		public byte MediaAutoScale;
            public UUID GroupID = new UUID();
            public int PassPrice;
            public float PassHours;
		/** Unsigned Byte */ 
		public byte Category;
            public UUID AuthBuyerID = new UUID();
            public UUID SnapshotID = new UUID();
            public Vector3 UserLocation = new Vector3();
            public Vector3 UserLookAt = new Vector3();
		/** Unsigned Byte */ 
		public byte LandingType;
            public boolean RegionPushOverride;
            public boolean RegionDenyAnonymous;
            public boolean RegionDenyIdentified;
            public boolean RegionDenyTransacted;

            @Override
			public int getLength()
            {
                                {
                    int length = 244;
                    if (Bitmap != null) { length += Bitmap.length; }
                    if (Name != null) { length += Name.length; }
                    if (Desc != null) { length += Desc.length; }
                    if (MusicURL != null) { length += MusicURL.length; }
                    if (MediaURL != null) { length += MediaURL.length; }
                    return length;
                }
            }

            public ParcelDataBlock() { }
            public ParcelDataBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                int length;
                try
                {
                    RequestResult = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SequenceID = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SnapSelection = (bytes[i[0]++] != 0) ? true : false;
                    SelfCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    OtherCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PublicCount = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    LocalID = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    OwnerID.FromBytes(bytes, i[0]); i[0] += 16;
                    IsGroupOwned = (bytes[i[0]++] != 0) ? true : false;
                    AuctionID = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    ClaimDate = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ClaimPrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    RentPrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    AABBMin.fromBytesLit(bytes, i[0]); i[0] += 12;
                    AABBMax.fromBytesLit(bytes, i[0]); i[0] += 12;
                    length = Utils.bytesToUInt16Lit(bytes, i[0]); i[0] += 2;
                    Bitmap = new byte[length];
                    Utils.arraycopy(bytes, i[0], Bitmap, 0, length); i[0] +=  length;
                    Area = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    Status = (byte)bytes[i[0]++];
                    SimWideMaxPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SimWideTotalPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    MaxPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    TotalPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    OwnerPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    GroupPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    OtherPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    SelectedPrims = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ParcelPrimBonus = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    OtherCleanTime = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    ParcelFlags = Utils.bytesToUIntLit(bytes, i[0]); i[0] += 4;
                    SalePrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Name = new byte[length];
                    Utils.arraycopy(bytes, i[0], Name, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    Desc = new byte[length];
                    Utils.arraycopy(bytes, i[0], Desc, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    MusicURL = new byte[length];
                    Utils.arraycopy(bytes, i[0], MusicURL, 0, length); i[0] +=  length;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    MediaURL = new byte[length];
                    Utils.arraycopy(bytes, i[0], MediaURL, 0, length); i[0] +=  length;
                    MediaID.FromBytes(bytes, i[0]); i[0] += 16;
                    MediaAutoScale = (byte)bytes[i[0]++];
                    GroupID.FromBytes(bytes, i[0]); i[0] += 16;
                    PassPrice = Utils.bytesToIntLit(bytes, i[0]); i[0]+=4;
                    PassHours = Utils.bytesToFloatLit(bytes, i[0]); i[0] += 4;
                    Category = (byte)bytes[i[0]++];
                    AuthBuyerID.FromBytes(bytes, i[0]); i[0] += 16;
                    SnapshotID.FromBytes(bytes, i[0]); i[0] += 16;
                    UserLocation.fromBytesLit(bytes, i[0]); i[0] += 12;
                    UserLookAt.fromBytesLit(bytes, i[0]); i[0] += 12;
                    LandingType = (byte)bytes[i[0]++];
                    RegionPushOverride = (bytes[i[0]++] != 0) ? true : false;
                    RegionDenyAnonymous = (bytes[i[0]++] != 0) ? true : false;
                    RegionDenyIdentified = (bytes[i[0]++] != 0) ? true : false;
                    RegionDenyTransacted = (bytes[i[0]++] != 0) ? true : false;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                Utils.intToBytesLit(RequestResult, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(SequenceID, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)((SnapSelection) ? 1 : 0);
                Utils.intToBytesLit(SelfCount, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(OtherCount, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(PublicCount, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(LocalID, bytes, i[0]); i[0] += 4;
                OwnerID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)((IsGroupOwned) ? 1 : 0);
                Utils.uintToBytesLit(AuctionID, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ClaimDate, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(ClaimPrice, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(RentPrice, bytes, i[0]); i[0] += 4;
                AABBMin.toBytesLit(bytes, i[0]); i[0] += 12;
                AABBMax.toBytesLit(bytes, i[0]); i[0] += 12;
                bytes[i[0]++] = (byte)(Bitmap.length % 256);
                bytes[i[0]++] = (byte)((Bitmap.length >> 8) % 256);
                Utils.arraycopy(Bitmap, 0, bytes, i[0], Bitmap.length); i[0] +=  Bitmap.length;
                Utils.intToBytesLit(Area, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = Status;
                Utils.intToBytesLit(SimWideMaxPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(SimWideTotalPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(MaxPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(TotalPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(OwnerPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(GroupPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(OtherPrims, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(SelectedPrims, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(ParcelPrimBonus, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(OtherCleanTime, bytes, i[0]); i[0] += 4;
                Utils.uintToBytesLit(ParcelFlags, bytes, i[0]); i[0] += 4;
                Utils.intToBytesLit(SalePrice, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = (byte)Name.length;
                Utils.arraycopy(Name, 0, bytes, i[0], Name.length); i[0] +=  Name.length;
                bytes[i[0]++] = (byte)Desc.length;
                Utils.arraycopy(Desc, 0, bytes, i[0], Desc.length); i[0] +=  Desc.length;
                bytes[i[0]++] = (byte)MusicURL.length;
                Utils.arraycopy(MusicURL, 0, bytes, i[0], MusicURL.length); i[0] +=  MusicURL.length;
                bytes[i[0]++] = (byte)MediaURL.length;
                Utils.arraycopy(MediaURL, 0, bytes, i[0], MediaURL.length); i[0] +=  MediaURL.length;
                MediaID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = MediaAutoScale;
                GroupID.ToBytes(bytes, i[0]); i[0] += 16;
                Utils.intToBytesLit(PassPrice, bytes, i[0]); i[0] += 4;
                Utils.floatToBytesLit(PassHours, bytes, i[0]); i[0] += 4;
                bytes[i[0]++] = Category;
                AuthBuyerID.ToBytes(bytes, i[0]); i[0] += 16;
                SnapshotID.ToBytes(bytes, i[0]); i[0] += 16;
                UserLocation.toBytesLit(bytes, i[0]); i[0] += 12;
                UserLookAt.toBytesLit(bytes, i[0]); i[0] += 12;
                bytes[i[0]++] = LandingType;
                bytes[i[0]++] = (byte)((RegionPushOverride) ? 1 : 0);
                bytes[i[0]++] = (byte)((RegionDenyAnonymous) ? 1 : 0);
                bytes[i[0]++] = (byte)((RegionDenyIdentified) ? 1 : 0);
                bytes[i[0]++] = (byte)((RegionDenyTransacted) ? 1 : 0);
            }

        }

        public final class AgeVerificationBlockBlock extends PacketBlock
        {
            public boolean RegionDenyAgeUnverified;

            @Override
			public int getLength()
            {
                    return 1;
            }

            public AgeVerificationBlockBlock() { }
            public AgeVerificationBlockBlock(byte[] bytes, int[] i) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
			public void FromBytes(byte[] bytes, int[] i) throws MalformedDataException
            {
                try
                {
                    RegionDenyAgeUnverified = (bytes[i[0]++] != 0) ? true : false;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
			public void ToBytes(byte[] bytes, int[] i)
            {
                bytes[i[0]++] = (byte)((RegionDenyAgeUnverified) ? 1 : 0);
            }

        }

        @Override
			public int getLength()
        {
                        {
                int length = 7;
                length += ParcelData.getLength();
                length += AgeVerificationBlock.getLength();
                return length;
            }
        }
        public ParcelDataBlock ParcelData;
        public AgeVerificationBlockBlock AgeVerificationBlock;

        public ParcelPropertiesPacket()
        {
            HasVariableBlocks = false;
            Type = PacketType.ParcelProperties;
            this.header =  new Header();
            header.Frequency = PacketFrequency.High;
            header.ID = 23;
            header.Reliable = true;
            header.Zerocoded = true;
            ParcelData = new ParcelDataBlock();
            AgeVerificationBlock = new AgeVerificationBlockBlock();
        }

        public ParcelPropertiesPacket(byte[] bytes, int[] i) throws MalformedDataException 
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
            ParcelData.FromBytes(bytes, i);
            AgeVerificationBlock.FromBytes(bytes, i);
        }

        public ParcelPropertiesPacket(Header head, byte[] bytes, int[] i) throws MalformedDataException
		{
		this();
            int[] packetEnd = new int[] {bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
		public void FromBytes(Header header, byte[] bytes, int[] i, int[] packetEnd) throws MalformedDataException
        {
            this.header =  header;
            ParcelData.FromBytes(bytes, i);
            AgeVerificationBlock.FromBytes(bytes, i);
        }

        @Override
			public byte[] ToBytes()
        {
            int length = 7;
            length += ParcelData.getLength();
            length += AgeVerificationBlock.getLength();
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            ParcelData.ToBytes(bytes, i);
            AgeVerificationBlock.ToBytes(bytes, i);
            if (header.AckList != null && header.AckList.length > 0) { header.AcksToBytes(bytes, i); }
            return bytes;
        }

        @Override
			public byte[][] ToBytesMultiple()
        {
            return new byte[][] { ToBytes() };
        }
    }