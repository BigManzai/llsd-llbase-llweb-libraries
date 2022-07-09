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
package com.ngt.jopenmetaverse.shared.cap.http;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.Iterator;
import java.util.Observable;
import java.util.Observer;
import java.util.Random;
import java.util.logging.Logger;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpRequestBase;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.XmlLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class EventQueueClient {
	protected Logger logger = Logger.getLogger(getClass().toString());

	/// <summary>=</summary>
	public static final int REQUEST_TIMEOUT = 1000 * 120;

	//	public delegate void ConnectedCallback();
	//	public delegate void EventCallback(string eventName, OSDMap body);
	//
	//	public ConnectedCallback OnConnected;
	//	public EventCallback OnEvent;

	protected EventObservable connectedObservable = new EventObservable();
	protected EventObservable<EventQueueClientEventObservableArg> eventObservable = new EventObservable<EventQueueClientEventObservableArg>();

	//TODO Need to handle following in HTTPBaseCLient 
	protected MethodDelegate<Void, HttpRequestBase> internalOpenWriteObservable;
	protected MethodDelegate<Void, HttpBaseRequestCompletedArg> internalRequestCompletedObservable;

	public void registerConnectedObserver(Observer o)
	{
		connectedObservable.addObserver(o);
	}

	public void registerEventObserver(Observer o)
	{
		eventObservable.addObserver(o);
	}


	public boolean getRunning() {return _Running; } 

	protected URI _Address;
	protected boolean _Dead;
	protected boolean _Running;
	protected HttpRequestBase _Request;

	/// <summary>Number of times we've received an unknown CAPS exception in series.</summary>
	private int _errorCount;
	/// <summary>For exponential backoff on error.</summary>
	private static Random _random = new Random();

	public EventQueueClient(URI eventQueueLocation)
	{
		_Address = eventQueueLocation;
		internalOpenWriteObservable = new MethodDelegate<Void, HttpRequestBase>()
				{
					public Void execute(HttpRequestBase request) {
						openWriteHandler(request);
						return null;
					}
				};
		
		internalRequestCompletedObservable =  new MethodDelegate<Void, HttpBaseRequestCompletedArg>()
				{
			public Void execute(HttpBaseRequestCompletedArg obj) {
				requestCompletedHandler(obj.getRequest(), 
						obj.getResponse(), obj.getResponseData(), 
						obj.getError());
				return null;
			}
		};
	}

	public void Start() throws Exception
	{
		_Dead = false;

		// Create an EventQueueGet request
		OSDMap request = new OSDMap();
		request.put("ack", new OSD());
		request.put("done", OSD.FromBoolean(false));

		byte[] postData = XmlLLSDOSDParser.SerializeLLSDXmlBytes(request);

		_Request = HttpBaseClient.UploadDataAsync(_Address, null, "application/xml", postData, REQUEST_TIMEOUT
				, internalOpenWriteObservable, null, internalRequestCompletedObservable);
	}

	public void Stop(boolean immediate)
	{
		_Dead = true;

		if (immediate)
			_Running = false;

		if (_Request != null)
			_Request.abort();
	}

	void openWriteHandler(HttpRequestBase request)
	{
		_Running = true;
		_Request = request;

		logger.info("Capabilities event queue connected");

		// The event queue is starting up for the first time
		if (connectedObservable != null)
		{
			try { connectedObservable.raiseEvent(null); }
			catch (Exception ex) { logger.warning(Utils.getExceptionStackTraceAsString(ex)); }
		}
	}

	void requestCompletedHandler(HttpRequestBase request, HttpResponse response, byte[] responseData, Exception error)
	{
		// We don't care about this request now that it has completed
		_Request = null;

		OSDArray events = null;
		int ack = 0;

		if (responseData != null)
		{
			_errorCount = 0;
			// Got a response
			OSDMap result = (OSDMap)XmlLLSDOSDParser.DeserializeLLSDXml(responseData);

			if (result != null)
			{
				events = (OSDArray)result.get("events");
				ack = result.get("id").asInteger();
			}
			else
			{
				
				try {
					logger.warning("Got an unparseable response from the event queue: \"" + 
							Utils.bytesToString(responseData) + "\"");
				} catch (UnsupportedEncodingException e) {
					logger.warning("Got an unparseable response from the event queue: \"" + 
							Utils.getExceptionStackTraceAsString(e) + "\"");
				}
			}
		}
		else if (error != null)
		{
			//TODO Handle the case when the request is aborted or there is some client side error

			if(response !=null)
			{
			//region Error handling
			int code = response.getStatusLine().getStatusCode();

			if(code == HttpStatus.SC_GONE || code == HttpStatus.SC_NOT_FOUND)
			{
				logger.info(String.format("Closing event queue at {0} due to missing caps URI", _Address));
				_Running = false;
				_Dead = true;				
			}
			else if(code == HttpStatus.SC_BAD_GATEWAY)
			{
				// This is not good (server) protocol design, but it's normal.
				// The EventQueue server is a proxy that connects to a Squid
				// cache which will time out periodically. The EventQueue server
				// interprets this as a generic error and returns a 502 to us
				// that we ignore
			}
			else
			{
				++_errorCount;				
				// Try to log a meaningful error message
				if (code != HttpStatus.SC_OK)
				{
					logger.warning(String.format("Unrecognized caps connection problem from %s: %s",
							_Address, code));
				}
				else
				{
					logger.warning(String.format("Unrecognized caps exception from %s: %s",
							_Address, error.getMessage()));
				}				
			}
			}
			else
			{
				++_errorCount;
				logger.warning("Some client Side Error" + Utils.getExceptionStackTraceAsString(error));
			}
		}
		else
		{
			++_errorCount;
			logger.warning("No response from the event queue but no reported error either");
		}	

		//			if (error instanceof HttpException)
		//			{
		//				HttpException webException = (HttpException)error;
		//
		//				if (response != null)
		//					code = response.getStatusLine().getStatusCode();
		//				else if (webException.Status == WebExceptionStatus.RequestCanceled)
		//					goto HandlingDone;
		//			}
		//
		//			if (error is WebException && ((WebException)error).Response != null)
		//				code = ((HttpWebResponse)((WebException)error).Response).StatusCode;
		//
		//			if (code == HttpStatusCode.NotFound || code == HttpStatusCode.Gone)
		//			{
		//				Logger.Log(String.format("Closing event queue at {0} due to missing caps URI", _Address), Helpers.LogLevel.Info);
		//
		//				_Running = false;
		//				_Dead = true;
		//			}
		//			else if (code == HttpStatusCode.BadGateway)
		//			{
		//				// This is not good (server) protocol design, but it's normal.
		//				// The EventQueue server is a proxy that connects to a Squid
		//				// cache which will time out periodically. The EventQueue server
		//				// interprets this as a generic error and returns a 502 to us
		//				// that we ignore
		//			}
		//			else
		//			{
		//				++_errorCount;
		//
		//				// Try to log a meaningful error message
		//				if (code != HttpStatusCode.OK)
		//				{
		//					Logger.Log(String.format("Unrecognized caps connection problem from {0}: {1}",
		//							_Address, code), Helpers.LogLevel.Warning);
		//				}
		//				else if (error.InnerException != null)
		//				{
		//					Logger.Log(String.Format("Unrecognized internal caps exception from {0}: {1}",
		//							_Address, error.InnerException.Message), Helpers.LogLevel.Warning);
		//				}
		//				else
		//				{
		//					Logger.Log(String.Format("Unrecognized caps exception from {0}: {1}",
		//							_Address, error.Message), Helpers.LogLevel.Warning);
		//				}
		//			}
		//
		//			//endregion Error handling
		//		}
		//		else
		//		{
		//			++_errorCount;
		//
		//			Logger.Log("No response from the event queue but no reported error either", Helpers.LogLevel.Warning);
		//		}
		//
		//		HandlingDone:
		//
		//			//region Resume the connection
		//
		//			if (_Running)
		//			{
		//				OSDMap osdRequest = new OSDMap();
		//				if (ack != 0) osdRequest["ack"] = OSD.FromInteger(ack);
		//				else osdRequest["ack"] = new OSD();
		//				osdRequest["done"] = OSD.FromBoolean(_Dead);
		//
		//				byte[] postData = OSDParser.SerializeLLSDXmlBytes(osdRequest);
		//
		//				if (_errorCount > 0) // Exponentially back off, so we don't hammer the CPU
		//					Thread.Sleep(_random.Next(500 + (int)Math.Pow(2, _errorCount)));
		//
		//				// Resume the connection. The event handler for the connection opening
		//				// just sets class _Request variable to the current HttpRequest
		//				CapsBase.UploadDataAsync(_Address, null, "application/xml", postData, REQUEST_TIMEOUT,
		//						delegate(HttpRequest newRequest) { _Request = newRequest; }, null, RequestCompletedHandler);
		//
		//				// If the event queue is dead at this point, turn it off since
		//				// that was the last thing we want to do
		//				if (_Dead)
		//				{
		//					_Running = false;
		//					Logger.DebugLog("Sent event queue shutdown message");
		//				}
		//			}
		//
		//		//endregion Resume the connection
		//
		//		//region Handle incoming events
		//
		//		if (OnEvent != null && events != null && events.Count > 0)
		//		{
		//			// Fire callbacks for each event received
		//			foreach (OSDMap evt in events)
		//			{
		//				string msg = evt["message"].AsString();
		//				OSDMap body = (OSDMap)evt["body"];
		//
		//				try { OnEvent(msg, body); }
		//				catch (Exception ex) { Logger.Log(ex.Message, Helpers.LogLevel.Error, ex); }
		//			}
		//		}
		//
		////		endregion Handle incoming events
		
		
		 //region Handle incoming events
         if (eventObservable != null && events != null && events.count() > 0)
         {
             //Fire callbacks for each event received
        	 Iterator<OSD> itr = events.iterator();
             while(itr.hasNext())
             {
            	 OSDMap evt = (OSDMap)itr.next();
                 String msg = evt.get("message").asString();
                 OSDMap body = (OSDMap)evt.get("body");

                 try 
                 { 
                	 eventObservable.raiseEvent(new EventQueueClientEventObservableArg(msg, body)); 
                 }
                 catch (Exception ex) 
                 { 
                	 logger.warning(Utils.getExceptionStackTraceAsString(ex)); 
                 }
             }
         }
         //endregion Handle incoming events
	}
}
