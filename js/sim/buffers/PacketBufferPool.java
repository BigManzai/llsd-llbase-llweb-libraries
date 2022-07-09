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
package com.ngt.jopenmetaverse.shared.sim.buffers;

import java.net.InetSocketAddress;

public class PacketBufferPool  
{
//
//	 private InetSocketAddress EndPoint;
//
//     /// <summary>
//     /// Initialize the object pool in client mode
//     /// </summary>
//     /// <param name="endPoint">Server to connect to</param>
//     /// <param name="itemsPerSegment"></param>
//     /// <param name="minSegments"></param>
//     public PacketBufferPool(InetSocketAddress endPoint, int itemsPerSegment, int minSegments)
//         : base()
//     {
//         EndPoint = endPoint;
//         Initialize(itemsPerSegment, minSegments, true, 1000 * 60 * 5);
//     }
//
//     /// <summary>
//     /// Initialize the object pool in server mode
//     /// </summary>
//     /// <param name="itemsPerSegment"></param>
//     /// <param name="minSegments"></param>
//     public PacketBufferPool(int itemsPerSegment, int minSegments)
//         : base()
//     {
//         EndPoint = null;
//         Initialize(itemsPerSegment, minSegments, true, 1000 * 60 * 5);
//     }
//
//     /// <summary>
//     /// Returns a packet buffer with EndPoint set if the buffer is in
//     /// client mode, or with EndPoint set to null in server mode
//     /// </summary>
//     /// <returns>Initialized UDPPacketBuffer object</returns>
//     @Override
//     protected  UDPPacketBuffer GetObjectInstance()
//     {
//         if (EndPoint != null)
//             // Client mode
//             return new UDPPacketBuffer(EndPoint);
//         else
//             // Server mode
//             return new UDPPacketBuffer();
//     }
}
