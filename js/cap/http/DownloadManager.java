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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpRequestBase;
import com.ngt.jopenmetaverse.shared.sim.Settings;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
/// <summary>
/// Manages async HTTP downloads with a limit on maximum
/// concurrent downloads
/// </summary>
public class DownloadManager
{
	/// <summary>
	/// Represends individual HTTP Download request
	/// </summary>
	public static class DownloadRequest
	{
		/// <summary>URI of the item to fetch</summary>
		public URI Address;
		/// <summary>Timout specified in milliseconds</summary>
		public int MillisecondsTimeout;
		/// <summary>Download progress callback</summary>
		public MethodDelegate<Void,HttpBaseDownloadProgressArg> DownloadProgressCallback;
		/// <summary>Download completed callback</summary>
		public MethodDelegate<Void,HttpBaseRequestCompletedArg>  CompletedCallback;
		/// <summary>Accept the following content type</summary>
		public String ContentType;

		/// <summary>Default constructor</summary>
		public DownloadRequest()
		{
		}

		/// <summary>Constructor</summary>
		public DownloadRequest(URI address, int millisecondsTimeout,
				String contentType,
				MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressCallback,
				MethodDelegate<Void,HttpBaseRequestCompletedArg> completedCallback)
		{
			this.Address = address;
			this.MillisecondsTimeout = millisecondsTimeout;
			this.DownloadProgressCallback = downloadProgressCallback;
			this.CompletedCallback = completedCallback;
			this.ContentType = contentType;
		}
	}

	private class ActiveDownload
	{
		public List<MethodDelegate<Void,HttpBaseDownloadProgressArg>> ProgresHadlers 
		= new ArrayList<MethodDelegate<Void,HttpBaseDownloadProgressArg>>();
		public List<MethodDelegate<Void,HttpBaseRequestCompletedArg>> CompletedHandlers 
		= new ArrayList<MethodDelegate<Void,HttpBaseRequestCompletedArg>>();
		public HttpRequestBase Request;
	}


	//        Queue<DownloadRequest> queue = new Queue<DownloadRequest>();
	public BlockingQueue<DownloadRequest> queue = new ArrayBlockingQueue<DownloadRequest>(Settings.PACKET_INBOX_SIZE);

	Map<String, ActiveDownload> activeDownloads = new HashMap<String, ActiveDownload>();

	int m_ParallelDownloads = 20;
	X509Certificate m_ClientCert = null;

	/// <summary>Maximum number of parallel downloads from a single endpoint</summary>
	public int getParallelDownloads()
	{
		return m_ParallelDownloads; 
	}

	public void setParallelDownloads(int value)
	{
		m_ParallelDownloads = value;
	}

	/// <summary>Client certificate</summary>
	public X509Certificate getClientCert()
	{
		return m_ClientCert; 
	}

	public void setClientCert(X509Certificate value)
	{
		m_ClientCert = value; 
	}

	/// <summary>Default constructor</summary>
	public DownloadManager()
	{
	}

	/// <summary>Cleanup method</summary>
	//virtual
	public void Dispose()
	{
		synchronized (activeDownloads)
		{
			for(ActiveDownload download : activeDownloads.values())
			{
				try
				{
					if (download.Request != null)
					{
						//TODO need to handle
						//                            download.Request.Abort();
					}
				}
				catch(Exception e) { }
			}
			activeDownloads.clear();
		}
	}

	/// <summary>Setup http download request</summary>
	//virtual
	protected HttpRequestBase SetupRequest(URI address, String acceptHeader)
	{
		HttpGet request = new HttpGet(address);
		//            request.Method = "GET";

		if (!Utils.isNullOrEmpty(acceptHeader))
			request.setHeader("Accept", acceptHeader);

		//TODO need to handle
		//            // Add the client certificate to the request if one was given
		//            if (m_ClientCert != null)
		//                request.ClientCertificates.Add(m_ClientCert);

		//TODO need to handle
		//            // Leave idle connections to this endpoint open for up to 60 seconds
		//            request.ServicePoint.MaxIdleTime = 0;
		//            // Disable stupid Expect-100: Continue header
		//            request.ServicePoint.Expect100Continue = false;
		//            // Crank up the max number of connections per endpoint (default is 2!)
		//            request.ServicePoint.ConnectionLimit = Math.Max(request.ServicePoint.ConnectionLimit, m_ParallelDownloads);

		return request;
	}

	/// <summary>Check the queue for pending work</summary>
	private void EnqueuePending()
	{
		synchronized (queue)
		{
			if (queue.size() > 0)
			{
				int nr = 0;
				synchronized (activeDownloads)
				{
					nr = activeDownloads.size();
				}

				for (int i = nr; i < getParallelDownloads() && queue.size() > 0; i++)
				{
					DownloadRequest item = queue.poll();
					synchronized (activeDownloads)
					{
						final String addr = item.Address.toString();
						if (activeDownloads.containsKey(addr))
						{
							activeDownloads.get(addr).CompletedHandlers.add(item.CompletedCallback);
							if (item.DownloadProgressCallback != null)
							{
								activeDownloads.get(addr).ProgresHadlers.add(item.DownloadProgressCallback);
							}
						}
						else
						{
							final ActiveDownload activeDownload = new ActiveDownload();
							activeDownload.CompletedHandlers.add(item.CompletedCallback);
							if (item.DownloadProgressCallback != null)
							{
								activeDownload.ProgresHadlers.add(item.DownloadProgressCallback);
							}

							JLogger.debug("Requesting " + item.Address.toString());
							activeDownload.Request = SetupRequest(item.Address, item.ContentType);

							MethodDelegate<Void,HttpBaseDownloadProgressArg> downloadProgressDelegate 
							= new MethodDelegate<Void, HttpBaseDownloadProgressArg>()
							{
								public Void execute(HttpBaseDownloadProgressArg arg) {
									for (MethodDelegate<Void,HttpBaseDownloadProgressArg> handler : activeDownload.ProgresHadlers)
									{
										handler.execute(arg);
									}
									return null;
								}

							};
							
							MethodDelegate<Void,HttpBaseRequestCompletedArg> doawnloadCompletedDelegate 
							= new MethodDelegate<Void, HttpBaseRequestCompletedArg>()
							{
								public Void execute(HttpBaseRequestCompletedArg arg) {
									synchronized (activeDownloads) 
									{activeDownloads.remove(addr);}
									for (MethodDelegate<Void,HttpBaseRequestCompletedArg> handler : activeDownload.CompletedHandlers)
									{
										handler.execute(arg);
									}
									EnqueuePending();	
									return null;
								}
							};

							HttpBaseClient.DownloadStringAsync(
									(HttpGet)activeDownload.Request,
									item.MillisecondsTimeout, 
									downloadProgressDelegate, doawnloadCompletedDelegate);

							activeDownloads.put(addr, activeDownload);                                		

							//                                HttpBaseClient.DownloadDataAsync(
									//                                    activeDownload.Request,
									//                                    item.MillisecondsTimeout,
									//                                    (HttpWebRequest request, HttpWebResponse response, int bytesReceived, int totalBytesToReceive) =>
									//                                    {
										//                                        for (CapsBase.DownloadProgressEventHandler handler in activeDownload.ProgresHadlers)
											//                                        {
											//                                            handler(request, response, bytesReceived, totalBytesToReceive);
							//                                        }
							//                                    },
							//                                    (HttpWebRequest request, HttpWebResponse response, byte[] responseData, Exception error) =>
							//                                    {
							//                                        lock (activeDownloads) activeDownloads.Remove(addr);
							//                                        for (CapsBase.RequestCompletedEventHandler handler in activeDownload.CompletedHandlers)
							//                                        {
							//                                            handler(request, response, responseData, error);
							//                                        }
							//                                        EnqueuePending();
							//                                    }
							//                                );
							//
							//                                activeDownloads[addr] = activeDownload;
						}
					}
				}
			}
		}
	}

	/// <summary>Enqueue a new HTPP download</summary>
	public void QueueDownlad(DownloadRequest req)
	{
		synchronized (activeDownloads)
		{
			String addr = req.Address.toString();
			JLogger.debug("Enque new http request: " + addr);
			if (activeDownloads.containsKey(addr))
			{
				activeDownloads.get(addr).CompletedHandlers.add(req.CompletedCallback);
				if (req.DownloadProgressCallback != null)
				{
					activeDownloads.get(addr).ProgresHadlers.add(req.DownloadProgressCallback);
				}
				return;
			}
		}

		synchronized (queue)
		{
			queue.offer(req);
		}
		EnqueuePending();
	}
}