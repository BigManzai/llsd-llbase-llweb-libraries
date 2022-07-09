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
package com.ngt.jopenmetaverse.shared.sim;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import com.ngt.jopenmetaverse.shared.sim.buffers.UDPPacketBuffer;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public abstract class UDPBase {
	// these abstract methods must be implemented in a derived class to actually do
	// something with the packets that are sent and received.
	protected abstract void PacketReceived(UDPPacketBuffer buffer);
	protected abstract void PacketSent(UDPPacketBuffer buffer, int bytesSent);
	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();


	// the port to listen on
	protected int udpPort;

	// the remote endpoint to communicate with
	protected InetSocketAddress remoteEndPoint = null;

	// the UDP socket
	private DatagramSocket udpSocket;

	// the all important shutdownFlag.
	private volatile boolean shutdownFlag = true;

	/// <summary>
	/// Initialize the UDP packet handler in server mode
	/// </summary>
	/// <param name="port">Port to listening for incoming UDP packets on</param>
	public UDPBase(int port)
	{
		udpPort = port;
	}

	/// <summary>
	/// Initialize the UDP packet handler in client mode
	/// </summary>
	/// <param name="endPoint">Remote UDP server to connect to</param>
	public UDPBase(InetSocketAddress endPoint)
	{
		remoteEndPoint = endPoint;
		//FIXME Need to remove following hardcoding . Used only for testing with Android Emulator
		udpPort = Settings.UDP_BIND_PORT;
	}

	/// <summary>
	/// 
	/// </summary>
	public void Start() throws SocketException
	{
		if (shutdownFlag)
		{
			//            final int SIO_UDP_CONNRESET = -1744830452;

			SocketAddress ipep = new InetSocketAddress(Settings.BIND_ADDR, udpPort);
			udpSocket = new DatagramSocket(ipep);
			//            try
			//            {
			//                // this udp socket flag is not supported under mono, 
			//                // so we'll catch the exception and continue
			//                udpSocket.IOControl(SIO_UDP_CONNRESET, new byte[] { 0 }, null);
			//            }
			//            catch (SocketException)
			//            {
			//                Logger.DebugLog("UDP SIO_UDP_CONNRESET flag not supported on this platform");
			//            }
			//            udpSocket.bind(ipep);

			// we're not shutting down, we're starting up
			shutdownFlag = false;

			// kick off an async receive.  The Start() method will return, the
			// actual receives will occur asynchronously and will be caught in
			// AsyncEndRecieve().
			AsyncBeginReceive();
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public void Stop()
	{
		if (!shutdownFlag)
		{
			// wait indefinitely for a writer lock.  Once this is called, the .NET runtime
			// will deny any more reader locks, in effect blocking all other send/receive
			// threads.  Once we have the lock, we set shutdownFlag to inform the other
			// threads that the socket is closed.
			shutdownFlag = true;
			udpSocket.close();
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public boolean isRunning()
	{
		return !shutdownFlag; 
	}

	private void AsyncBeginReceive()
	{
		// allocate a dedicated thread.
		threadPool.execute(new Runnable(){
			public void run()
			{
				//While it is not shutdown
				while (!shutdownFlag)
				{
					final UDPPacketBuffer buf = new UDPPacketBuffer();
					DatagramPacket receivePacket = new DatagramPacket(buf.getData(), buf.getData().length);
					try {
						udpSocket.receive(receivePacket);
						buf.setDataLength(receivePacket.getLength());
						buf.setRemoteEndPoint(receivePacket.getSocketAddress());
						//Start another receiving, it will keep the server going

						//start another thread to process the packet
						threadPool.execute(new Runnable(){
							public void run()
							{
								PacketReceived(buf);
							}
						});
					}
					catch (Exception e) {
						//TODO handle error in a better way
						JLogger.error("Error while recieving packet\n" + Utils.getExceptionStackTraceAsString(e));	
					}
				}
			}});
	}

	public void AsyncBeginSend(final UDPPacketBuffer buf)
	{
		if (!shutdownFlag)
		{
			threadPool.execute(new Runnable(){
				public void run()
				{
					try {
						DatagramPacket sendPacket = new DatagramPacket(buf.getData(), buf.getDataLength(), buf.getRemoteEndPoint());
						//						JLogger.debug(String.format("Data sending to server of length %d \n%s ", buf.getDataLength(), Utils.bytesToHexDebugString(buf.getData(), buf.getDataLength(), "")));
						udpSocket.send(sendPacket);
						PacketSent(buf, buf.getDataLength());
					}
					catch (Exception e) {
						//TODO handle error in a better way
						JLogger.error("Error while sending packet\n" + Utils.getExceptionStackTraceAsString(e));
					}
				}
			});
		}
	}
}

