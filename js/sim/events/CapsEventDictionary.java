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
import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class CapsEventDictionary {

	//TODO Need to implement
	/// <summary>
		/// Object that is passed to worker threads in the ThreadPool for
		/// firing CAPS callbacks
		/// </summary>
		private class CapsCallbackWrapper
		{
			/// <summary>Callback to fire for this packet</summary>
			public EventObservable<CapsEventObservableArg> Callback;
			/// <summary>Name of the CAPS event</summary>
			public String CapsEvent;
			/// <summary>Strongly typed decoded data</summary>
			public IMessage Message;
			/// <summary>Reference to the simulator that generated this event</summary>
			public Simulator Simulator;
		}

		/// <summary>Reference to the GridClient object</summary>
		public GridClient Client;

		private Map<String, EventObservable<CapsEventObservableArg>> _EventTable =
				new HashMap<String, EventObservable<CapsEventObservableArg>>();
		private ThreadPool threadPool = ThreadPoolFactory.getThreadPool();
//		private WaitCallback _ThreadPoolCallback;

		/// <summary>
		/// Default constructor
		/// </summary>
		/// <param name="client">Reference to the GridClient object</param>
		public CapsEventDictionary(GridClient client)
		{
			Client = client;
//			_ThreadPoolCallback = new WaitCallback(ThreadPoolDelegate);
		}

		/// <summary>
		/// Register an new event handler for a capabilities event sent via the EventQueue
		/// </summary>
		/// <remarks>Use String.Empty to fire this event on every CAPS event</remarks>
		/// <param name="capsEvent">Capability event name to register the 
		/// handler for</param>
		/// <param name="eventHandler">Callback to fire</param>
		public void RegisterEvent(String capsEvent, EventObserver<CapsEventObservableArg> eventHandler)
		{
			synchronized (_EventTable)
			{
//				if (_EventTable.containsKey(capsEvent))
//					_EventTable.put(capsEvent, eventHandler);
//				else
//					_EventTable[capsEvent] = eventHandler;
				
				EventObservable<CapsEventObservableArg> callback;
				if ((callback = _EventTable.get(capsEvent)) != null)
				{
					callback.addObserver(eventHandler);
				}
				else
				{
					_EventTable.put(capsEvent, 
							new EventObservable<CapsEventObservableArg>());
					_EventTable.get(capsEvent).addObserver(eventHandler);
				}
				
			}
		}

		/// <summary>
		/// Unregister a previously registered capabilities handler 
		/// </summary>
		/// <param name="capsEvent">Capability event name unregister the 
		/// handler for</param>
		/// <param name="eventHandler">Callback to unregister</param>
		public void UnregisterEvent(String capsEvent, EventObserver<CapsEventObservableArg> eventHandler)
		{
			synchronized (_EventTable)
			{
//				if (_EventTable.containsKey(capsEvent) && _EventTable[capsEvent] != null)
//					_EventTable[capsEvent] -= eventHandler;
				
				EventObservable<CapsEventObservableArg> callback;
				if ((callback = _EventTable.get(capsEvent)) != null)
				{
					callback.deleteObserver(eventHandler);
				}
			}
		}

		/// <summary>
		/// Fire the events registered for this event type synchronously
		/// </summary>
		/// <param name="capsEvent">Capability name</param>
		/// <param name="message">Decoded event body</param>
		/// <param name="simulator">Reference to the simulator that 
		/// generated this event</param>
		public void RaiseEvent(String capsEvent, IMessage message, Simulator simulator)
		{
			boolean specialHandler = false;
			EventObservable<CapsEventObservableArg> callback;

			// Default handler first, if one exists
			if ( ((callback = _EventTable.get(""))!=null) )
			{
				try 
				{ 
					callback.raiseEvent(new CapsEventObservableArg(capsEvent, message, simulator)); 
				}
					catch (Exception ex) { JLogger.error("CAPS Event Handler: " + Utils.getExceptionStackTraceAsString(ex)); }
			}

			// Explicit handler next
			if ( ((callback = _EventTable.get(capsEvent))!=null) )
			{
				try 
				{ 
					callback.raiseEvent(new CapsEventObservableArg(capsEvent, message, simulator));
					specialHandler = true;
				}
					catch (Exception ex) { JLogger.error("CAPS Event Handler: " + Utils.getExceptionStackTraceAsString(ex)); }
			}

			if (!specialHandler)
				JLogger.warn("Unhandled CAPS event " + capsEvent);
		}

		/// <summary>
		/// Fire the events registered for this event type asynchronously
		/// </summary>
		/// <param name="capsEvent">Capability name</param>
		/// <param name="message">Decoded event body</param>
		/// <param name="simulator">Reference to the simulator that 
		/// generated this event</param>
		public void BeginRaiseEvent(String capsEvent, IMessage message, Simulator simulator)
		{
			boolean specialHandler = false;
			EventObservable<CapsEventObservableArg> callback;

			// Default handler first, if one exists
			if ( ((callback = _EventTable.get(""))!=null) )
			{
				if (callback != null)
				{
					final CapsCallbackWrapper wrapper = new CapsCallbackWrapper();
					wrapper.Callback = callback;
					wrapper.CapsEvent = capsEvent;
					wrapper.Message = message;
					wrapper.Simulator = simulator;
//					ThreadPool.QueueUserWorkItem(_ThreadPoolCallback, wrapper);
					threadPool.execute(new Runnable(){
						public void run()
			            {
							ThreadPoolDelegate(wrapper);
			            }
					});		
				}
			}

			// Explicit handler next
			if ( ((callback = _EventTable.get(capsEvent))!=null) )
			{
				final CapsCallbackWrapper wrapper = new CapsCallbackWrapper();
				wrapper.Callback = callback;
				wrapper.CapsEvent = capsEvent;
				wrapper.Message = message;
				wrapper.Simulator = simulator;
//				ThreadPool.QueueUserWorkItem(_ThreadPoolCallback, wrapper);
				threadPool.execute(new Runnable(){
					public void run()
		            {
						ThreadPoolDelegate(wrapper);
		            }
				});

				specialHandler = true;
			}

			if (!specialHandler)
				JLogger.warn("Unhandled CAPS event " + capsEvent);
		}

		private void ThreadPoolDelegate(Object state)
		{
			CapsCallbackWrapper wrapper = (CapsCallbackWrapper)state;
			try
			{
				wrapper.Callback.raiseEvent(new CapsEventObservableArg(wrapper.CapsEvent, wrapper.Message, wrapper.Simulator));
			}
			catch (Exception ex)
			{
				JLogger.error("Async CAPS Event Handler: " +  Utils.getExceptionStackTraceAsString(ex));
			}
		}
}
