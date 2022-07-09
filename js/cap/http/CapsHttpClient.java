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

import java.net.URI;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import org.apache.http.client.methods.HttpRequestBase;

import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDParser;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.BinaryLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.JsonLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.XmlLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class CapsHttpClient
{
	Logger logger = Logger.getLogger(getClass().toString());
	final CapsHttpClient capsHttpClient = this;

//	public class RequestCompletedObserver implements Observer
//	{
//		public void update(Observable arg0, Object arg1) {
//			HttpBaseRequestCompletedArg rcha = (HttpBaseRequestCompletedArg) arg1;
//
//			_Request = rcha.getRequest();
//
//			OSD result = null;
//
//			if (rcha.getResponseData() != null)
//			{
//				try { result = OSDParser.Deserialize(rcha.getResponseData()); }
//				catch (Exception ex) { rcha.setError(ex); }
//			}
//			FireCompleteCallback(result, rcha.getError());
//		}	
//	}

	private void FireCompleteCallback(OSD result, Exception error)
	{
		CapsHttpRequestCompletedArg r = new CapsHttpRequestCompletedArg(capsHttpClient, result, error);
		requestCompleteObservable.raiseEvent(r);
		if(requestCompleteDelegate !=null)
			requestCompleteDelegate.execute(r);
		_Response = result;
        _ResponseEvent.set();
	}

//	public class DownloadProgressObserver implements Observer
//	{
//		public void update(Observable arg0, Object arg1) {
//			HttpBaseDownloadProgressArg rcha = (HttpBaseDownloadProgressArg) arg1;
//			CapsHttpRequestProgressArg chrpa = new CapsHttpRequestProgressArg(capsHttpClient, rcha.bytesReceived, rcha.totalBytesToReceive);
//			requestProgressObservable.raiseEvent(chrpa);
//		}	
//	}		 	

	protected EventObservable<CapsHttpRequestCompletedArg> requestCompleteObservable;
	protected EventObservable<CapsHttpRequestProgressArg> requestProgressObservable;

	protected MethodDelegate<Void, CapsHttpRequestCompletedArg> requestCompleteDelegate = null;
	protected MethodDelegate<Void, CapsHttpRequestProgressArg> requestProgressDelegate = null;

	protected MethodDelegate<Void, HttpBaseRequestCompletedArg> internalRequestCompletedDelegate;
	protected MethodDelegate<Void, HttpBaseDownloadProgressArg> internaldownloadProgressDelegate;	 	
	protected Object UserData;
	protected URI _Address;
	protected byte[] _PostData;
	protected X509Certificate _ClientCert;
	protected String _ContentType;
	protected HttpRequestBase _Request;
	protected OSD _Response;
	protected AutoResetEvent _ResponseEvent = new AutoResetEvent(false);

	public CapsHttpClient(URI capability)
	{
		this(capability, null);
	}


	public CapsHttpClient(URI capability, X509Certificate clientCert)
	{

		_Address = capability;
		_ClientCert = clientCert;

		requestCompleteObservable = new EventObservable<CapsHttpRequestCompletedArg>();
		requestProgressObservable = new EventObservable<CapsHttpRequestProgressArg>();;
		
		internalRequestCompletedDelegate = new MethodDelegate<Void, HttpBaseRequestCompletedArg>()
				{
					public Void execute(HttpBaseRequestCompletedArg rcha) {
						_Request = rcha.getRequest();

						OSD result = null;

						if (rcha.getResponseData() != null)
						{
							try { result = OSDParser.Deserialize(rcha.getResponseData()); }
							catch (Exception ex) { rcha.setError(ex); }
						}
						FireCompleteCallback(result, rcha.getError());						
						return null;
					}
				};
				
		internaldownloadProgressDelegate = new MethodDelegate<Void, HttpBaseDownloadProgressArg>()
				{
					public Void execute(HttpBaseDownloadProgressArg rcha) {
						CapsHttpRequestProgressArg chrpa = new CapsHttpRequestProgressArg(capsHttpClient, rcha.bytesReceived, rcha.totalBytesToReceive);
						requestProgressObservable.raiseEvent(chrpa);
						if(requestProgressDelegate !=null)
							requestProgressDelegate.execute(chrpa);
						return null;
					}
				};
	}        

	public void addRequestCompleteObserver(EventObserver<CapsHttpRequestCompletedArg> o)
	{
		requestCompleteObservable.addObserver(o);
	}

	public void addRequestProgressObservable(EventObserver<CapsHttpRequestProgressArg> o)
	{
		requestProgressObservable.addObserver(o);
	}
	
	
	public MethodDelegate<Void, CapsHttpRequestCompletedArg> getRequestCompleteDelegate() {
		return requestCompleteDelegate;
	}


	public void setRequestCompleteDelegate(
			MethodDelegate<Void, CapsHttpRequestCompletedArg> requestCompleteDelegate) {
		this.requestCompleteDelegate = requestCompleteDelegate;
	}


	public MethodDelegate<Void, CapsHttpRequestProgressArg> getRequestProgressDelegate() {
		return requestProgressDelegate;
	}


	public void setRequestProgressDelegate(
			MethodDelegate<Void, CapsHttpRequestProgressArg> requestProgressDelegate) {
		this.requestProgressDelegate = requestProgressDelegate;
	}


	public Object getUserData() {
		return UserData;
	}


	public void setUserData(Object userData) {
		UserData = userData;
	}


	public void BeginGetResponse(int millisecondsTimeout)
	{
		BeginGetResponse2(null, null, millisecondsTimeout);
	}

	/*
	 * Serialise data into format type and upload
	 */
	public void BeginGetResponse(OSD data, OSDFormat format, int millisecondsTimeout) throws Exception
	{
		byte[] postData;
		String contentType;

		switch (format)
		{
		case Xml:
			postData = XmlLLSDOSDParser.SerializeLLSDXmlBytes(data);
			contentType = "application/llsd+xml";
			break;
		case Binary:
			postData = BinaryLLSDOSDParser.SerializeLLSDBinary(data);
			contentType = "application/llsd+binary";
			break;
		case Json:
		default:
			postData = Utils.stringToBytes((JsonLLSDOSDParser.SerializeLLSDJsonString(data)));
			contentType = "application/llsd+json";
			break;
		}

		BeginGetResponse(postData, contentType, millisecondsTimeout);
	}

	public void BeginGetResponse(byte[] postData, String contentType, int millisecondsTimeout)
	{
		BeginGetResponse2(postData, contentType, millisecondsTimeout);
	}

	private void BeginGetResponse2(byte[] postData, String contentType, int millisecondsTimeout)
	{
		_PostData = postData;
		_ContentType = contentType;

		if (_Request != null)
		{
			_Request.abort();
			_Request = null;
		}

		if (postData == null)
		{
			// GET
			//Logger.Log.Debug("[CapsClient] GET " + _Address);
//			System.out.println("Going to send GET request");
			_Request = HttpBaseClient.DownloadStringAsync(_Address, _ClientCert, millisecondsTimeout,internaldownloadProgressDelegate, 
					internalRequestCompletedDelegate);
		}
		else
		{
			// POST
			//Logger.Log.Debug("[CapsClient] POST (" + postData.Length + " bytes) " + _Address);
			_Request = HttpBaseClient.UploadDataAsync(_Address, _ClientCert, contentType, postData, millisecondsTimeout, null,
					internaldownloadProgressDelegate, internalRequestCompletedDelegate);
		}
	}
	public OSD GetResponse(int millisecondsTimeout) throws InterruptedException
	{
		BeginGetResponse(millisecondsTimeout);
		_ResponseEvent.waitOne(millisecondsTimeout);
		return _Response;
	}

	public OSD GetResponse(OSD data, OSDFormat format, int millisecondsTimeout) throws Exception
	{
		BeginGetResponse(data, format, millisecondsTimeout);
		_ResponseEvent.waitOne(millisecondsTimeout);
		return _Response;
	}

	public OSD GetResponse(byte[] postData, String contentType, int millisecondsTimeout) throws InterruptedException
	{
		BeginGetResponse(postData, contentType, millisecondsTimeout);
		_ResponseEvent.waitOne(millisecondsTimeout);
		return _Response;
	}

	public void Cancel()
	{
		if (_Request != null)
			_Request.abort();
	}
	
	
//	private void waitForRequestCompletion(int millisecondsTimeout)
//	{
//		final Thread currentThread = Thread.currentThread();
//		internalRequestCompletedObservable.addObserver(new Observer(){
//			public void update(Observable arg0, Object arg1) {
//				currentThread.interrupt();
//			}
//		});
//		sleep(millisecondsTimeout);
//	}
//
//	private void sleep(int timeout)
//	{
//		try {
//			Thread.sleep(timeout);
//		} catch (InterruptedException e) {
////			e.printStackTrace();
//			logger.info(e.getMessage());
//		}
//	}
}