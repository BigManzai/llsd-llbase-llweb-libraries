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
package com.ngt.jopenmetaverse.shared.sim.events;

import java.util.HashMap;
import java.util.Map;
import java.util.Observer;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.protocol.Packet;

public class PacketEventDictionary {
	private final class PacketCallback
	{
		public PacketObservable Callback;
		public boolean IsAsync;
		
		public PacketCallback(Observer callback, boolean isAsync)
		{
			Callback = new PacketObservable();
			Callback.addObserver(callback);
			IsAsync = isAsync;
		}
	}

	/// <summary>
	/// Object that is passed to worker threads in the ThreadPool for
	/// firing packet callbacks
	/// </summary>
	private class PacketCallbackWrapper
	{
		/// <summary>Callback to fire for this packet</summary>
		public PacketObservable Callback;
		/// <summary>Reference to the simulator that this packet came from</summary>
		public Simulator Simulator;
		/// <summary>The packet that needs to be processed</summary>
		public Packet Packet;
	}
	
	
	/// <summary>Reference to the GridClient object</summary>
	public GridClient Client;
	private ThreadPool threadPool = ThreadPoolFactory.getThreadPool();
	private Logger logger = Logger.getLogger("");
	private Map<PacketType, PacketCallback> _EventTable = new HashMap<PacketType, PacketCallback>();

	/// <summary>
	/// Default constructor
	/// </summary>
	/// <param name="client"></param>
	public PacketEventDictionary(GridClient client)
	{
		Client = client;
	}

	/// <summary>
	/// Register an event handler
	/// </summary>
	/// <remarks>Use PacketType.Default to fire this event on every 
	/// incoming packet</remarks>
	/// <param name="packetType">Packet type to register the handler for</param>
	/// <param name="eventHandler">Callback to be fired</param>
	/// <param name="isAsync">True if this callback should be ran 
	/// asynchronously, false to run it synchronous</param>
	public void RegisterEvent(PacketType packetType, Observer event, boolean isAsync)
	{
		synchronized (_EventTable)
		{
			PacketCallback callback;
			if ((callback = _EventTable.get(packetType)) != null)
			{
				callback.Callback.addObserver(event);
				callback.IsAsync = callback.IsAsync || isAsync;
			}
			else
			{
				callback = new PacketCallback(event, isAsync);
				_EventTable.put(packetType, callback);
			}
		}
	}

	/// <summary>
	/// Unregister an event handler
	/// </summary>
	/// <param name="packetType">Packet type to unregister the handler for</param>
	/// <param name="eventHandler">Callback to be unregistered</param>
	public void UnregisterEvent(PacketType packetType, Observer eventHandler)
	{
		synchronized (_EventTable)
		{
			PacketCallback callback;
			if ((callback = _EventTable.get(packetType)) != null)
			{
					callback.Callback.deleteObserver(eventHandler);				
				if (callback.Callback == null || callback.Callback.countObservers() == 0)
					_EventTable.remove(packetType);
			}
		}
	}

	/// <summary>
	/// Fire the events registered for this packet type
	/// </summary>
	/// <param name="packetType">Incoming packet type</param>
	/// <param name="packet">Incoming packet</param>
	/// <param name="simulator">Simulator this packet was received from</param>
	public void RaiseEvent(PacketType packetType, Packet packet, Simulator simulator)
	{
		PacketCallback callback;
		
		// Default handler first, if one exists
		if ( ((callback = _EventTable.get(PacketType.Default)) != null) && (callback.Callback != null))
		{
			final PacketCallbackWrapper wrapper = new PacketCallbackWrapper(); 
			wrapper.Callback = callback.Callback;
			wrapper.Packet = packet;
			wrapper.Simulator = simulator;
			if (callback.IsAsync)
			{
				threadPool.execute(new Runnable(){
					public void run()
		            {
						ThreadPoolDelegate(wrapper);
		            }
				});
			}
			else
			{
				try 
				{ 
//					callback.Callback(this, new PacketReceivedEventArgs(packet, simulator)); 
					ThreadPoolDelegate(wrapper);					
				}
				catch (Exception ex)
				{
					logger.log(Level.SEVERE, "Default packet event handler: " + ex.toString(), Client);
//					Logger.Log("Default packet event handler: " + ex.ToString(), Helpers.LogLevel.Error, Client);
				}
			}
		}

		if (((callback = _EventTable.get(packetType)) != null) && (callback.Callback != null))
		{
			final PacketCallbackWrapper wrapper = new PacketCallbackWrapper(); 
			wrapper.Callback = callback.Callback;
			wrapper.Packet = packet;
			wrapper.Simulator = simulator;
			if (callback.IsAsync)
			{
				threadPool.execute(new Runnable(){
					public void run()
		            {
						ThreadPoolDelegate(wrapper);
		            }
				});		
			}
			else
			{
				try 
				{ 
//					callback.Callback(this, new PacketReceivedEventArgs(packet, simulator));
					ThreadPoolDelegate(wrapper);					
				}
				catch (Exception ex)
				{
					logger.log(Level.INFO, "Default packet event handler: " + ex.toString(), Client);
				}
			}
			return;
		}

		if (packetType != PacketType.Default && packetType != PacketType.PacketAck)
		{
			JLogger.info("No handler registered for packet event " + packetType);
		}
	}

	private void ThreadPoolDelegate(Object state)
	{
		PacketCallbackWrapper wrapper = (PacketCallbackWrapper)state;
		try
		{
			wrapper.Callback.raiseEvent(new PacketReceivedEventArgs(wrapper.Packet, wrapper.Simulator));	
//			wrapper.Callback(this, new PacketReceivedEventArgs(wrapper.Packet, wrapper.Simulator));
		}
		catch (Exception ex)
		{
			logger.log(Level.SEVERE, "Async Packet Event Handler: " + ex.toString(),  Client);
		}
	}
}


