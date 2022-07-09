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

import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;



   /// <exclude/>
    public final class TelehubInfoPacket extends Packet
    {
        /// <exclude/>
        public static final class TelehubBlockBlock extends PacketBlock
        {
            public UUID ObjectID = new UUID();
		/** Unsigned Byte */ 
		public byte[] ObjectName;
            public Vector3 TelehubPos = new Vector3();
            public Quaternion TelehubRot = new Quaternion();

            @Override
            public int getLength()
            {
                    int length = 41;
                    if (ObjectName != null) { length += ObjectName.length; }
                    return length;
            }

            public TelehubBlockBlock() { }
            public TelehubBlockBlock(byte[] bytes, int i[]) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
            public void FromBytes(byte[] bytes, int i[]) throws MalformedDataException
            {
                int length;
                try
                {
                    ObjectID.FromBytes(bytes, i[0]); i[0] += 16;
                    length = Utils.ubyteToInt(bytes[i[0]++]);
                    ObjectName = new byte[length];
                    System.arraycopy(bytes, i[0], ObjectName, 0, length); i[0] += length;
                    TelehubPos.fromBytesLit(bytes, i[0]); i[0] += 12;
                    TelehubRot.fromBytesLit(bytes, i[0], true); i[0] += 12;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
            public void ToBytes(byte[] bytes, int i[])
            {
                ObjectID.ToBytes(bytes, i[0]); i[0] += 16;
                bytes[i[0]++] = (byte)ObjectName.length;
                System.arraycopy(ObjectName, 0, bytes, i[0], ObjectName.length); i[0] += ObjectName.length;
                TelehubPos.toBytesLit(bytes, i[0]); i[0] += 12;
                TelehubRot.toBytesLit(bytes, i[0]); i[0] += 12;
            }

        }

        /// <exclude/>
        public static final class SpawnPointBlockBlock extends PacketBlock
        {
            public Vector3 SpawnPointPos = new Vector3();

            @Override
            public int getLength()
            {
                    return 12;
            }

            public SpawnPointBlockBlock() { }
            public SpawnPointBlockBlock(byte[] bytes, int i[]) throws MalformedDataException
            {
                FromBytes(bytes, i);
            }

            @Override
            public void FromBytes(byte[] bytes, int i[]) throws MalformedDataException
            {
                try
                {
                    SpawnPointPos.fromBytesLit(bytes, i[0]); i[0] += 12;
                }
                catch (Exception e)
                {
                    throw new MalformedDataException(Utils.getExceptionStackTraceAsString(e));
                }
            }

            @Override
            public void ToBytes(byte[] bytes, int i[])
            {
                SpawnPointPos.toBytesLit(bytes, i[0]); i[0] += 12;
            }

        }

        @Override
        public int getLength()
        {
                int length = 11;
                length += TelehubBlock.getLength();
                for (int j = 0; j < SpawnPointBlock.length; j++)
                    length += SpawnPointBlock[j].getLength();
                return length;
        }
        
        public TelehubBlockBlock TelehubBlock;
        public SpawnPointBlockBlock[] SpawnPointBlock;

        public TelehubInfoPacket()
        {
            HasVariableBlocks = true;
            Type = PacketType.TelehubInfo;
            header = new Header();
            header.Frequency = PacketFrequency.Low;
            header.ID = 10;
            header.Reliable = true;
            TelehubBlock = new TelehubBlockBlock();
            SpawnPointBlock = null;
        }

        public TelehubInfoPacket(byte[] bytes, int i[]) throws MalformedDataException
        {
        	this();
            int[] packetEnd = new int[]{bytes.length - 1};
            FromBytes(bytes, i, packetEnd, null);
        }

        @Override
        public void FromBytes(byte[] bytes, int i[], int packetEnd[], byte[] zeroBuffer) throws MalformedDataException
        {
            header.FromBytes(bytes, i, packetEnd);
            if (header.Zerocoded && zeroBuffer != null)
            {
                packetEnd[0] = Helpers.ZeroDecode(bytes, packetEnd[0] + 1, zeroBuffer) - 1;
                bytes = zeroBuffer;
            }
            TelehubBlock.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(SpawnPointBlock == null || SpawnPointBlock.length != -1) {
                SpawnPointBlock = new SpawnPointBlockBlock[count];
                for(int j = 0; j < count; j++)
                { SpawnPointBlock[j] = new SpawnPointBlockBlock(); }
            }
            for (int j = 0; j < count; j++)
            { SpawnPointBlock[j].FromBytes(bytes, i); }
        }

        public TelehubInfoPacket(Header head, byte[] bytes, int i[]) throws MalformedDataException
        {
        	this();
            int[] packetEnd = new int[]{bytes.length - 1};
            FromBytes(head, bytes, i, packetEnd);
        }

        @Override
        public void FromBytes(Header header, byte[] bytes, int i[], int packetEnd[]) throws MalformedDataException
        {
            this.header = header;
            TelehubBlock.FromBytes(bytes, i);
            int count = Utils.ubyteToInt(bytes[i[0]++]);
            if(SpawnPointBlock == null || SpawnPointBlock.length != count) {
                SpawnPointBlock = new SpawnPointBlockBlock[count];
                for(int j = 0; j < count; j++)
                { SpawnPointBlock[j] = new SpawnPointBlockBlock(); }
            }
            for (int j = 0; j < count; j++)
            { SpawnPointBlock[j].FromBytes(bytes, i); }
        }

        @Override
        public byte[] ToBytes()
        {
            int length = 10;
            length += TelehubBlock.getLength();
            length++;
            for (int j = 0; j < SpawnPointBlock.length; j++) { length += SpawnPointBlock[j].getLength(); }
            if (header.AckList != null && header.AckList.length > 0) { length += header.AckList.length * 4 + 1; }
            byte[] bytes = new byte[length];
            int[] i = new int[]{0};
            header.ToBytes(bytes, i);
            TelehubBlock.ToBytes(bytes, i);
            bytes[i[0]++] = (byte)SpawnPointBlock.length;
            for (int j = 0; j < SpawnPointBlock.length; j++) { SpawnPointBlock[j].ToBytes(bytes, i); }
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

            fixedLength += TelehubBlock.getLength();
            byte[] fixedBytes = new byte[fixedLength];
            header.ToBytes(fixedBytes,  i);
            TelehubBlock.ToBytes(fixedBytes, i);
            fixedLength += 1;

            int SpawnPointBlockStart = 0;
            do
            {
                int variableLength = 0;
                int SpawnPointBlockCount = 0;

                i[0] = SpawnPointBlockStart;
                while (fixedLength + variableLength + acksLength[0] < Packet.MTU && i[0] < SpawnPointBlock.length) {
                    int blockLength = SpawnPointBlock[i[0]].getLength();
                    if (fixedLength + variableLength + blockLength + acksLength[0] <= MTU) {
                        variableLength += blockLength;
                        ++SpawnPointBlockCount;
                    }
                    else { break; }
                    ++i[0];
                }

                byte[] packet = new byte[fixedLength + variableLength + acksLength[0]];
                int[] length = new int[]{fixedBytes.length};
                System.arraycopy(fixedBytes, 0, packet, 0, length[0]);
                if (packets.size() > 0) { packet[0] = (byte)(packet[0] & ~0x10); }

                packet[length[0]++] = (byte)SpawnPointBlockCount;
                for (i[0] = SpawnPointBlockStart; i[0] < SpawnPointBlockStart + SpawnPointBlockCount; i[0]++) { SpawnPointBlock[i[0]].ToBytes(packet, length); }
                SpawnPointBlockStart += SpawnPointBlockCount;

                if (acksLength[0] > 0) {
                	System.arraycopy(ackBytes, 0, packet, length[0], acksLength[0]);
                    acksLength[0] = 0;
                }

                packets.add(packet);
            } while (
                SpawnPointBlockStart < SpawnPointBlock.length);

            return (byte[][]) packets.toArray(new byte[0][0]);
        }
    }
