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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpVersion;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.conn.scheme.PlainSocketFactory;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.entity.ByteArrayEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.util.EntityUtils;

import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.util.Utils;


/*
 * TODO
 *  * Handle client side certificates
 *  * Handle timeout
 *  * Handle HTTP error codes in getting and posting
 */
public class HttpBaseClient {

	private static HttpClient httpclient;

	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();

	static 
	{
		//TODO manage httpclient level resources 
		
		
		/* FIXME 
		 * ThreadSafeClientConnManager is deprecated in latest httpclient 4.2
		 * however android api supports only httpclient 4.0.2
		 * as a result we have to revert back to ThreadSafeClientConnManager 
		 */
//		httpclient = new DefaultHttpClient(new PoolingClientConnectionManager());

		  //sets up parameters
        HttpParams params = new BasicHttpParams();
        HttpProtocolParams.setVersion(params, HttpVersion.HTTP_1_1);
        HttpProtocolParams.setContentCharset(params, "utf-8");
        params.setBooleanParameter("http.protocol.expect-continue", false);

        //registers schemes for both http and https
        SchemeRegistry registry = new SchemeRegistry();
        registry.register(new Scheme("http", PlainSocketFactory.getSocketFactory(), 80));
        final SSLSocketFactory sslSocketFactory = SSLSocketFactory.getSocketFactory();
        sslSocketFactory.setHostnameVerifier(SSLSocketFactory.BROWSER_COMPATIBLE_HOSTNAME_VERIFIER);
        registry.register(new Scheme("https", sslSocketFactory, 443));

        ThreadSafeClientConnManager manager = new ThreadSafeClientConnManager(params, registry);
		
        httpclient = new DefaultHttpClient(manager, params);        

		// When HttpClient instance is no longer needed,
		// shut down the connection manager to ensure
		// immediate deallocation of all system resources
		//httpclient.getConnectionManager().shutdown();

	}

	public static HttpRequestBase UploadDataAsync(URI _Address,
			java.security.cert.X509Certificate _ClientCert, String contentType,
			byte[] postData, int millisecondsTimeout, MethodDelegate<Void, HttpRequestBase> openWriteCallback,
			final MethodDelegate<Void, HttpBaseDownloadProgressArg> downloadProgressObservable,
			final MethodDelegate<Void, HttpBaseRequestCompletedArg> requestCompletedObservable) 
	{
		   HttpPost httppost = new HttpPost(_Address);
		   
		   if(!Utils.isNullOrEmpty(contentType))
			   httppost.setHeader("Content-Type", contentType);

	        httppost.setEntity(new ByteArrayEntity(postData));

			HttpRequestStringThreadPoolTaskAsyc(httppost, millisecondsTimeout, openWriteCallback,
					downloadProgressObservable, requestCompletedObservable);
		    return httppost;
	}


	public static HttpRequestBase DownloadStringAsync(URI _Address, 
			java.security.cert.X509Certificate _ClientCert,
			final int millisecondsTimeout,
			final MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressObservable,
			final MethodDelegate<Void,HttpBaseRequestCompletedArg> requestCompletedObservable)
			{
		// Prepare a request object
		final HttpGet httpget = new HttpGet(_Address);

		HttpRequestStringThreadPoolTaskAsyc(httpget, millisecondsTimeout, null,
				downloadProgressObservable, requestCompletedObservable);
		
		return httpget;

			}
	
	public static void DownloadStringAsync(final HttpGet httprequest,
			final int millisecondsTimeout,
			final MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressObservable,
			final MethodDelegate<Void,HttpBaseRequestCompletedArg> requestCompletedObservable)
			{
		
		HttpRequestStringThreadPoolTaskAsyc(httprequest, millisecondsTimeout, null, 
				downloadProgressObservable, requestCompletedObservable);
		
			}
	
	private static void HttpRequestStringThreadPoolTaskAsyc(final HttpRequestBase httprequest, 
			final int millisecondsTimeout,
			final MethodDelegate<Void, HttpRequestBase> openWriteCallback,
			final MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressObservable,
			final MethodDelegate<Void,HttpBaseRequestCompletedArg> requestCompletedObservable)
	{
		threadPool.execute(new Runnable(){
			public void run()
			{
				HttpRequestStringThreadPoolTask(httprequest, millisecondsTimeout, openWriteCallback,
						downloadProgressObservable, requestCompletedObservable);
			}
		});
	}
	
	private static void HttpRequestStringThreadPoolTask(HttpRequestBase httpget, int millisecondsTimeout,
			final MethodDelegate<Void, HttpRequestBase> openWriteCallback,
			final MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressObservable,
			final MethodDelegate<Void,HttpBaseRequestCompletedArg> requestCompletedObservable)
	{
		// Execute the request
		HttpResponse response = null;
		int statusCode;
		try {
			response = httpclient.execute(httpget);
			
			if(openWriteCallback!=null)
				openWriteCallback.execute(httpget);
			// Examine the response status
//			System.out.println(response.getStatusLine());
			statusCode = response.getStatusLine().getStatusCode();

			// Get hold of the response entity
			HttpEntity entity = response.getEntity();

			if (entity != null) {
				InputStream instream = entity.getContent();
				try {

					byte[] bytes = readBytes(httpget, response, instream, downloadProgressObservable);
					//release the resources
					
					//FIXME following is not supported in httpclient 4.0.2
//					 EntityUtils.(entity);
					 
					HttpBaseRequestCompletedArg r = new HttpBaseRequestCompletedArg(httpget, response, bytes, null);
					if(requestCompletedObservable !=null)
						raiseRequestCompletedEvent(requestCompletedObservable, r);
				} catch (IOException ex) {

					// In case of an IOException the connection will be released
					// back to the connection manager automatically
					throw ex;

				} catch (RuntimeException ex) {

					// In case of an unexpected exception you may want to abort
					// the HTTP request in order to shut down the underlying
					// connection and release it back to the connection manager.
					httpget.abort();
					throw ex;

				} finally {

					// Closing the input stream will trigger connection release
					instream.close();
				}
			}
			else
			{
				HttpBaseRequestCompletedArg r = new HttpBaseRequestCompletedArg(httpget, response, new byte[0], null);
				raiseRequestCompletedEvent(requestCompletedObservable, r);
			}
		} 
		catch (Exception e) {
			HttpBaseRequestCompletedArg r = new HttpBaseRequestCompletedArg(httpget, null, null, e);
			raiseRequestCompletedEvent(requestCompletedObservable, r);
		}
	}
	
	
	private static byte[] readBytes(HttpRequestBase request, HttpResponse response, InputStream input, 
			final MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressObservable) throws IOException
	{
		int totalBytesRead = 0;
		int bytesRead = 0;
		//TODO Need to review following LIMITs and Performance Parameters
		final int maxByteRead = 10000;
		ByteArrayOutputStream byteBuffer = new ByteArrayOutputStream(2048);
		byte[] bytes = new byte[maxByteRead];

		//TODO byteBuffer should not grow forever. There should be some upper limit
		while(bytesRead >= 0){
//			System.out.println("Starting Reading...");
			bytesRead = input.read(bytes, 0, maxByteRead); 
			if (bytesRead > 0){
				totalBytesRead = totalBytesRead + bytesRead;
				byteBuffer.write(bytes, 0, bytesRead);
//				System.out.println("Read: " + bytesRead);
				HttpBaseDownloadProgressArg r = new HttpBaseDownloadProgressArg(request, response, bytesRead, totalBytesRead); 
				raiseDownloadProgressEvent(downloadProgressObservable, r);
			}
		}
		byteBuffer.flush();
		bytes = byteBuffer.toByteArray();
		byteBuffer.close();
		return bytes;
	}

	private static void raiseRequestCompletedEvent(MethodDelegate<Void,HttpBaseRequestCompletedArg> requestCompletedObservable, HttpBaseRequestCompletedArg r)
	{
		if(requestCompletedObservable != null)
		requestCompletedObservable.execute(r);
	}
	
	private static void raiseDownloadProgressEvent(MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressObservable, HttpBaseDownloadProgressArg r)
	{
		if(downloadProgressObservable != null)
			downloadProgressObservable.execute(r);
	}	
	
}

