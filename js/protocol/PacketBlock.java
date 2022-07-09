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


  /// <summary>
    /// A block of data in a packet. Packets are composed of one or more blocks,
    /// each block containing one or more fields
    /// </summary>
    public abstract class PacketBlock implements Serializable
    {
        /// <summary>Current length of the data in this packet</summary>
        public abstract int getLength();

        /// <summary>
        /// Create a block from a byte array
        /// </summary>
        /// <param name="bytes">Byte array containing the serialized block</param>
        /// <param name="i">Starting position of the block in the byte array.
        /// This will point to the data after the end of the block when the
        /// call returns</param>
        public abstract void FromBytes(byte[] bytes, int[] i) throws MalformedDataException;

        /// <summary>
        /// Serialize this block into a byte array
        /// </summary>
        /// <param name="bytes">Byte array to serialize this block into</param>
        /// <param name="i">Starting position in the byte array to serialize to.
        /// This will point to the position directly after the end of the
        /// serialized block when the call returns</param>
        public abstract void ToBytes(byte[] bytes, int[] i);
    }
