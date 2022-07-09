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
import java.net.SocketAddress;

import com.ngt.jopenmetaverse.shared.sim.Settings;

public class UDPPacketBuffer 
{
	 /// <summary>Default Size of the byte array used to store raw packet data</summary>
    public final int BUFFER_SIZE = 4096;
    /// <summary>Raw packet data buffer</summary>
    //readonly
    private byte[] Data;
    /// <summary>Length of the actual data to transmit or received. Need to set explicitly</summary>
    private int DataLength;
    
    /// <summary>EndPoint of the remote host</summary>
    private SocketAddress RemoteEndPoint;

    /// <summary>
    /// Create an allocated UDP packet buffer for receiving a packet
    /// </summary>
    public UDPPacketBuffer()
    {
        this(new InetSocketAddress(Settings.BIND_ADDR, 0));
    }

    /// <summary>
    /// Create an allocated UDP packet buffer for sending a packet
    /// </summary>
    /// <param name="endPoint">EndPoint of the remote host</param>
    public UDPPacketBuffer(InetSocketAddress endPoint)
    {
        Data = new byte[BUFFER_SIZE];
        //initially no data in the buffer
        DataLength=0;
        RemoteEndPoint = endPoint;
    }

    /// <summary>
    /// Create an allocated UDP packet buffer for sending a packet
    /// </summary>
    /// <param name="endPoint">EndPoint of the remote host</param>
    /// <param name="bufferSize">Size of the buffer to allocate for packet data</param>
    public UDPPacketBuffer(InetSocketAddress endPoint, int bufferSize)
    {
        Data = new byte[bufferSize];
        RemoteEndPoint = endPoint;
    }

	public byte[] getData() {
		return Data;
	}
	
	public int getDataLength() {
		return DataLength;
	}

	public void setDataLength(int dataLength) {
		DataLength = dataLength;
	}

	public SocketAddress getRemoteEndPoint() {
		return RemoteEndPoint;
	}

	public void setRemoteEndPoint(SocketAddress remoteEndPoint) {
		RemoteEndPoint = remoteEndPoint;
	}
    
}

