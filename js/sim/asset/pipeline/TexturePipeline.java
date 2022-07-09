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
package com.ngt.jopenmetaverse.shared.sim.asset.pipeline;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.SortedMap;
import java.util.TimerTask;
import java.util.TreeMap;
import com.ngt.jopenmetaverse.shared.protocol.ImageDataPacket;
import com.ngt.jopenmetaverse.shared.protocol.ImageNotInDatabasePacket;
import com.ngt.jopenmetaverse.shared.protocol.ImagePacketPacket;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.RequestImagePacket;
import com.ngt.jopenmetaverse.shared.sim.AssetManager.ImageCodec;
import com.ngt.jopenmetaverse.shared.sim.AssetManager.ImageType;
import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.sim.Settings;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetTexture;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.EventTimer;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.asm.ImageDownload;
import com.ngt.jopenmetaverse.shared.sim.events.asm.TextureDownloadCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.DisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginProgressEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginStatus;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;


/// <summary>
/// Texture request download handler, allows a configurable number of download slots which manage multiple
/// concurrent texture downloads from the <seealso cref="Simulator"/>
/// </summary>
/// <remarks>This class makes full use of the internal <seealso cref="TextureCache"/> 
/// system for full texture downloads.</remarks>
public class TexturePipeline
{

	/// <summary>
	/// The current status of a texture request as it moves through the pipeline or final result of a texture request. 
	/// </summary>
	public enum TextureRequestState
	{
		/// <summary>The initial state given to a request. Requests in this state
		/// are waiting for an available slot in the pipeline</summary>
		Pending,
		/// <summary>A request that has been added to the pipeline and the request packet
		/// has been sent to the simulator</summary>
		Started,
		/// <summary>A request that has received one or more packets back from the simulator</summary>
		Progress,
		/// <summary>A request that has received all packets back from the simulator</summary>
		Finished,
		/// <summary>A request that has taken longer than <seealso cref="Settings.PIPELINE_REQUEST_TIMEOUT"/>
		/// to download OR the initial packet containing the packet information was never received</summary>
		Timeout,
		/// <summary>The texture request was aborted by request of the agent</summary>
		Aborted,
		/// <summary>The simulator replied to the request that it was not able to find the requested texture</summary>
		NotFound
	}
	/// <summary>
	/// A callback fired to indicate the status or final state of the requested texture. For progressive 
	/// downloads this will fire each time new asset data is returned from the simulator.
	/// </summary>
	/// <param name="state">The <see cref="TextureRequestState"/> indicating either Progress for textures not fully downloaded,
	/// or the final result of the request after it has been processed through the TexturePipeline</param>
	/// <param name="assetTexture">The <see cref="AssetTexture"/> object containing the Assets ID, raw data
	/// and other information. For progressive rendering the <see cref="Asset.AssetData"/> will contain
	/// the data from the beginning of the file. For failed, aborted and timed out requests it will contain
	/// an empty byte array.</param>
	//        public delegate void TextureDownloadCallback(TextureRequestState state, AssetTexture assetTexture);


	////if DEBUG_TIMING // Timing globals
	/// <summary>The combined time it has taken for all textures requested sofar. This includes the amount of time the 
	/// texture spent waiting for a download slot, and the time spent retrieving the actual texture from the Grid</summary>
	public static long TotalTime;
	/// <summary>The amount of time the request spent in the <see cref="TextureRequestState.Progress"/> state</summary>
	public static long NetworkTime;
	/// <summary>The total number of bytes transferred since the TexturePipeline was started</summary>
	public static int TotalBytes;
	////endif
	/// <summary>
	/// A request task containing information and status of a request as it is processed through the <see cref="TexturePipeline"/>
	/// </summary>
	private class TaskInfo
	{
		/// <summary>The current <seealso cref="TextureRequestState"/> which identifies the current status of the request</summary>
		public TextureRequestState State;
		/// <summary>The Unique Request ID, This is also the Asset ID of the texture being requested</summary>
		public UUID RequestID;
		/// <summary>The slot this request is occupying in the threadpoolSlots array</summary>
		public int RequestSlot;
		/// <summary>The ImageType of the request.</summary>
		public ImageType Type;

		/// <summary>The callback to fire when the request is complete, will include 
		/// the <seealso cref="TextureRequestState"/> and the <see cref="AssetTexture"/> 
		/// object containing the result data</summary>

		public List<MethodDelegate<Void, TextureDownloadCallbackArgs>> Callbacks;
		/// <summary>If true, indicates the callback will be fired whenever new data is returned from the simulator.
		/// This is used to progressively render textures as portions of the texture are received.</summary>
		public boolean ReportProgress;

		////if DEBUG_TIMING
		/// <summary>The time the request was added to the the PipeLine</summary>
		public long StartTime;
		/// <summary>The time the request was sent to the simulator</summary>
		public long NetworkTime;
		////endif
		/// <summary>An object that maintains the data of an request thats in-process.</summary>
		public ImageDownload Transfer;
	}

	/// <summary>A dictionary containing all pending and in-process transfer requests where the Key is both the RequestID
	/// and also the Asset Texture ID, and the value is an object containing the current state of the request and also
	/// the asset data as it is being re-assembled</summary>
	private  Map<UUID, TaskInfo> _Transfers;
	/// <summary>Holds the reference to the <see cref="GridClient"/> client object</summary>
	private  GridClient _Client;
	/// <summary>Maximum concurrent texture requests allowed at a time</summary>
	private  int maxTextureRequests;
	/// <summary>An array of <see cref="AutoResetEvent"/> objects used to manage worker request threads</summary>
	private  AutoResetEvent[] resetEvents;
	/// <summary>An array of worker slots which shows the availablity status of the slot</summary>
	private  int[] threadpoolSlots;
	/// <summary>The primary thread which manages the requests.</summary>
	private EventTimer downloadMaster;
	/// <summary>true if the TexturePipeline is currently running</summary>
	boolean _Running = false;
	/// <summary>A synchronization object used by the primary thread</summary>
	private Object lockerObject = new Object();
	/// <summary>A refresh timer used to increase the priority of stalled requests</summary>
	private EventTimer RefreshDownloadsTimer;

	private Map<PacketType, EventObserver<PacketReceivedEventArgs>> eventObservers 
	= new HashMap<PacketType, EventObserver<PacketReceivedEventArgs>>();

	/// <summary>Current number of pending and in-process transfers</summary>
	public int getTransferCount() {  return _Transfers.size(); } 

	/// <summary>
	/// Default constructor, Instantiates a new copy of the TexturePipeline class
	/// </summary>
	/// <param name="client">Reference to the instantiated <see cref="GridClient"/> object</param>
	public TexturePipeline(GridClient client)
	{
		_Client = client;

		maxTextureRequests = client.settings.MAX_CONCURRENT_TEXTURE_DOWNLOADS;

		resetEvents = new AutoResetEvent[maxTextureRequests];
		threadpoolSlots = new int[maxTextureRequests];

		_Transfers = new HashMap<UUID, TaskInfo>();
		_Running = false;
		
		// Pre-configure autoreset events and threadpool slots
		for (int i = 0; i < maxTextureRequests; i++)
		{
			resetEvents[i] = new AutoResetEvent(true);
			threadpoolSlots[i] = -1;
		}

		// Handle client connected and disconnected events
		client.network.RegisterLoginProgressCallback(new EventObserver<LoginProgressEventArgs>()
				{ @Override
			public void handleEvent(Observable o, LoginProgressEventArgs e) {
					if (e.getStatus() == LoginStatus.Success)
					{
						Startup();
					}
				}});

		client.network.RegisterOnDisconnectedCallback(new EventObserver<DisconnectedEventArgs>()
				{
			@Override
			public void handleEvent(Observable o, DisconnectedEventArgs e) {
				Shutdown();    			
			}});

		//            // Handle client connected and disconnected events
		//            client.network.LoginProgress += delegate(object sender, LoginProgressEventArgs e) {
		//                if (e.Status == LoginStatus.Success)
		//                {
		//                    Startup();
		//                }
		//            };
		//
		//            client.Network.Disconnected += delegate { Shutdown(); };
	}

	/// <summary>
	/// Initialize callbacks required for the TexturePipeline to operate
	/// </summary>
	public void Startup()
	{
		if (_Running)
			return;

		if (downloadMaster == null)
		{
			//                // Instantiate master thread that manages the request pool
			//                downloadMaster = new Thread(DownloadThread);
			//                downloadMaster.Name = "TexturePipeline";
			//                downloadMaster.IsBackground = true;
			downloadMaster = new EventTimer(new TimerTask(){
				@Override
				public void run() {
					if(_Running)
						DownloadThread();						
				}
			});
		}

		_Running = true;

		eventObservers.put(PacketType.ImageData, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ImageDataHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		eventObservers.put(PacketType.ImagePacket, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ImagePacketHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		eventObservers.put(PacketType.ImageNotInDatabase, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ImageNotInDatabaseHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		for(Entry<PacketType, EventObserver<PacketReceivedEventArgs>> e : eventObservers.entrySet())
		{
			_Client.network.RegisterCallback(e.getKey(), e.getValue());
		}

		//            _Client.network.RegisterCallback(PacketType.ImageData, ImageDataHandler);
		//            _Client.network.RegisterCallback(PacketType.ImagePacket, ImagePacketHandler);
		//            _Client.network.RegisterCallback(PacketType.ImageNotInDatabase, ImageNotInDatabaseHandler);

		downloadMaster.schedule(500, 500);

		if (RefreshDownloadsTimer == null)
		{
			RefreshDownloadsTimer = new EventTimer(new TimerTask(){
				@Override
				public void run() {
					RefreshDownloadsTimer_Elapsed();
				}
			});
			RefreshDownloadsTimer.schedule((long)Settings.PIPELINE_REFRESH_INTERVAL, (int)Settings.PIPELINE_REFRESH_INTERVAL);

			//                RefreshDownloadsTimer = new System.Timers.Timer(Settings.PIPELINE_REFRESH_INTERVAL);
			//                RefreshDownloadsTimer.Elapsed += RefreshDownloadsTimer_Elapsed;
			//                RefreshDownloadsTimer.start();
		}
	}

	/// <summary>
	/// Shutdown the TexturePipeline and cleanup any callbacks or transfers
	/// </summary>
	public void Shutdown()
	{
		if (!_Running)
			return;
		//if DEBUG_TIMING
		JLogger.debug(String.format("Combined Execution Time: {0}, Network Execution Time {1}, Network {2}K/sec, Image Size {3}",
				TotalTime, NetworkTime, Math.round(TotalBytes / NetworkTime/ (60*60)), TotalBytes));
		//endif
		if(null != RefreshDownloadsTimer) 
			RefreshDownloadsTimer.cancel();

		RefreshDownloadsTimer = null;

		if (downloadMaster != null)
		{
			downloadMaster.cancel();
		}
		downloadMaster = null;

		for(Entry<PacketType, EventObserver<PacketReceivedEventArgs>> e : eventObservers.entrySet())
		{
			_Client.network.UnregisterCallback(e.getKey(), e.getValue());
		}

		//            _Client.network.UnregisterCallback(PacketType.ImageNotInDatabase, ImageNotInDatabaseHandler);
		//            _Client.network.UnregisterCallback(PacketType.ImageData, ImageDataHandler);
		//            _Client.network.UnregisterCallback(PacketType.ImagePacket, ImagePacketHandler);

		synchronized (_Transfers)
		{_Transfers.clear();}

		for (int i = 0; i < resetEvents.length; i++)
			if (resetEvents[i] != null)
				resetEvents[i].set();

		_Running = false;
	}

	private void RefreshDownloadsTimer_Elapsed()
	{
		synchronized (_Transfers)
		{
			for (TaskInfo transfer : _Transfers.values())
			{
				if (transfer.State == TextureRequestState.Progress)
				{
					ImageDownload download = transfer.Transfer;

					// Find the first missing packet in the download
					int packet = 0;
					synchronized (download)
					{
						if (download.PacketsSeen != null && download.PacketsSeen.size() > 0)
							packet = GetFirstMissingPacket(download.PacketsSeen);

						if (download.getTimeSinceLastPacket() > 5000)
						{
							// We're not receiving data for this texture fast enough, bump up the priority by 5%
							download.Priority *= 1.05f;

							download.setTimeSinceLastPacket(0);
							RequestImage(download.ID, download.ImageType, download.Priority, download.DiscardLevel, packet);
						}
						if (download.getTimeSinceLastPacket() > Settings.PIPELINE_REQUEST_TIMEOUT)
						{
							resetEvents[transfer.RequestSlot].set();
						}
					}
				}
			}
		}
	}

	/// <summary>
	/// Request a texture asset from the simulator using the <see cref="TexturePipeline"/> system to 
	/// manage the requests and re-assemble the image from the packets received from the simulator
	/// </summary>
	/// <param name="textureID">The <see cref="UUID"/> of the texture asset to download</param>
	/// <param name="imageType">The <see cref="ImageType"/> of the texture asset. 
	/// Use <see cref="ImageType.Normal"/> for most textures, or <see cref="ImageType.Baked"/> for baked layer texture assets</param>
	/// <param name="priority">A float indicating the requested priority for the transfer. Higher priority values tell the simulator
	/// to prioritize the request before lower valued requests. An image already being transferred using the <see cref="TexturePipeline"/> can have
	/// its priority changed by resending the request with the new priority value</param>
	/// <param name="discardLevel">Number of quality layers to discard.
	/// This controls the end marker of the data sent</param>
	/// <param name="packetStart">The packet number to begin the request at. A value of 0 begins the request
	/// from the start of the asset texture</param>
	/// <param name="callback">The <see cref="TextureDownloadCallback"/> callback to fire when the image is retrieved. The callback
	/// will contain the result of the request and the texture asset data</param>
	/// <param name="progressive">If true, the callback will be fired for each chunk of the downloaded image. 
	/// The callback asset parameter will contain all previously received chunks of the texture asset starting 
	/// from the beginning of the request</param>
	public void RequestTexture(UUID textureID, ImageType imageType, float priority, 
			int discardLevel, long packetStart, MethodDelegate<Void, TextureDownloadCallbackArgs> callback, boolean progressive)
	{
		if (textureID.equals(UUID.Zero))
			return;

		if (callback != null)
		{
			if (_Client.assets.Cache.hasAsset(textureID))
			{
				ImageDownload image = new ImageDownload();
				image.ID = textureID;
				image.AssetData = _Client.assets.Cache.getCachedAssetBytes(textureID);
				image.Size = image.AssetData.length;
				image.Transferred = image.AssetData.length;
				image.ImageType = imageType;
				image.AssetType = AssetType.Texture;
				image.Success = true;

				callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Finished, 
						new AssetTexture(image.ID, image.AssetData)));

				_Client.assets.FireImageProgressEvent(image.ID, image.Transferred, image.Size);
			}
			else
			{
				synchronized (_Transfers)
				{
					TaskInfo request;

					if ((request = _Transfers.get(textureID))!=null)
					{
						request.Callbacks.add(callback);
					}
					else
					{
						request = new TaskInfo();
						request.State = TextureRequestState.Pending;
						request.RequestID = textureID;
						request.ReportProgress = progressive;
						request.RequestSlot = -1;
						request.Type = imageType;

						request.Callbacks = new ArrayList<MethodDelegate<Void, TextureDownloadCallbackArgs>>();
						request.Callbacks.add(callback);

						ImageDownload downloadParams = new ImageDownload();
						downloadParams.ID = textureID;
						downloadParams.Priority = priority;
						downloadParams.ImageType = imageType;
						downloadParams.DiscardLevel = discardLevel;

						request.Transfer = downloadParams;
						////if DEBUG_TIMING
						request.StartTime = Utils.getUnixTime();
						////endif
						_Transfers.put(textureID, request);
					}
				}
			}
		}
	}

	/// <summary>
	/// Sends the actual request packet to the simulator
	/// </summary>
	/// <param name="imageID">The image to download</param>
	/// <param name="type">Type of the image to download, either a baked
	/// avatar texture or a normal texture</param>
	/// <param name="priority">Priority level of the download. Default is
	/// <c>1,013,000.0f</c></param>
	/// <param name="discardLevel">Number of quality layers to discard.
	/// This controls the end marker of the data sent</param>
	/// <param name="packetNum">Packet number to start the download at.
	/// This controls the start marker of the data sent</param>
	/// <remarks>Sending a priority of 0 and a discardlevel of -1 aborts
	/// download</remarks>
	private void RequestImage(UUID imageID, ImageType type, float priority, int discardLevel, long packetNum)
	{
		// Priority == 0 && DiscardLevel == -1 means cancel the transfer
		if (priority ==0 && discardLevel ==-1)
		{
			AbortTextureRequest(imageID);
		}
		else
		{
			TaskInfo task;
			if ((task = TryGetTransferValue(imageID))!=null)
			{
				if (task.Transfer.Simulator != null)
				{
					// Already downloading, just updating the priority
					float percentComplete = ((float)task.Transfer.Transferred / (float)task.Transfer.Size) * 100f;
					if (Float.isNaN(percentComplete))
						percentComplete = 0f;

					if (percentComplete > 0f)
						JLogger.debug(String.format("Updating priority on image transfer %s to %f, %f% complete",
								imageID, task.Transfer.Priority, Math.round(percentComplete)));
				}
				else
				{
					ImageDownload transfer = task.Transfer;
					transfer.Simulator = _Client.network.getCurrentSim();
				}

				// Build and send the request packet
				RequestImagePacket request = new RequestImagePacket();
				request.AgentData.AgentID = _Client.self.getAgentID();
				request.AgentData.SessionID = _Client.self.getSessionID();
				request.RequestImage = new RequestImagePacket.RequestImageBlock[1];
				request.RequestImage[0] = new RequestImagePacket.RequestImageBlock();
				request.RequestImage[0].DiscardLevel = (byte)discardLevel;
				request.RequestImage[0].DownloadPriority = priority;
				request.RequestImage[0].Packet = packetNum;
				request.RequestImage[0].Image = imageID;
				request.RequestImage[0].Type = (byte)type.getIndex();

				_Client.network.SendPacket(request, _Client.network.getCurrentSim());
			}
			else
			{
				JLogger.warn("Received texture download request for a texture that isn't in the download queue: " + imageID);
			}
		}
	}

	/// <summary>
	/// Cancel a pending or in process texture request
	/// </summary>
	/// <param name="textureID">The texture assets unique ID</param>
	public void AbortTextureRequest(UUID textureID)
	{
		TaskInfo task;
		if ((task = TryGetTransferValue(textureID))!=null)
		{
			// this means we've actually got the request assigned to the threadpool
			if (task.State == TextureRequestState.Progress)
			{
				RequestImagePacket request = new RequestImagePacket();
				request.AgentData.AgentID = _Client.self.getAgentID();
				request.AgentData.SessionID = _Client.self.getSessionID();
				request.RequestImage = new RequestImagePacket.RequestImageBlock[1];
				request.RequestImage[0] = new RequestImagePacket.RequestImageBlock();
				request.RequestImage[0].DiscardLevel = -1;
				request.RequestImage[0].DownloadPriority = 0;
				request.RequestImage[0].Packet = 0;
				request.RequestImage[0].Image = textureID;
				request.RequestImage[0].Type = (byte)task.Type.getIndex();
				_Client.network.SendPacket(request);

				for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback : task.Callbacks)
					callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Aborted, new AssetTexture(textureID, Utils.EmptyBytes)));

				_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred, task.Transfer.Size);

				resetEvents[task.RequestSlot].set();

				RemoveTransfer(textureID);
			}
			else
			{
				RemoveTransfer(textureID);

				for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback : task.Callbacks)
					callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Aborted, new AssetTexture(textureID, Utils.EmptyBytes)));

				_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred, task.Transfer.Size);
			}
		}
	}

	/// <summary>
	/// Master Download Thread, Queues up downloads in the threadpool
	/// </summary>
	private void DownloadThread()
	{
		int slot;

		boolean slotAvailable = true;
		while (slotAvailable)
		{
			slotAvailable = false;
			// find free slots
			int pending = 0;
			int active = 0;

			TaskInfo nextTask = null;
			JLogger.debug("Transfer Size: " + this.getTransferCount());
			synchronized (_Transfers)
			{
				for (Entry<UUID, TaskInfo> request : _Transfers.entrySet())
				{
					if (request.getValue().State == TextureRequestState.Pending)
					{
						nextTask = request.getValue();
						++pending;
					}
					else if (request.getValue().State == TextureRequestState.Progress)
					{
						++active;
					}
				}
			}

			if (pending > 0 && active <= maxTextureRequests)
			{
				slot = -1;
				// find available slot for reset event
				synchronized (lockerObject)
				{
					for (int i = 0; i < threadpoolSlots.length; i++)
					{
						if (threadpoolSlots[i] == -1)
						{
							// found a free slot
							threadpoolSlots[i] = 1;
							slot = i;
							break;
						}
					}
				}

				// -1 = slot not available
				if (slot != -1 && nextTask != null)
				{
					nextTask.State = TextureRequestState.Started;
					nextTask.RequestSlot = slot;

					//Logger.DebugLog(String.Format("Sending Worker thread new download request {0}", slot));
					final TaskInfo threadNextTask = nextTask;
					ThreadPoolFactory.getThreadPool().execute(new Runnable(){
						public void run()
						{
							try{
								TextureRequestDoWork(threadNextTask);
							}
							catch(Exception e)
							{ JLogger.error(Utils.getExceptionStackTraceAsString(e));}
						}
					});

					//                        ThreadPool.QueueUserWorkItem(TextureRequestDoWork, nextTask);
					slotAvailable = true;
				}
			}

			// Queue was empty or all download slots are inuse, let's give up some CPU time
			//                Thread.Sleep(500);
		}
	}


	/// <summary>
	/// The worker thread that sends the request and handles timeouts
	/// </summary>
	/// <param name="threadContext">A <see cref="TaskInfo"/> object containing the request details</param>
	private void TextureRequestDoWork(Object threadContext) throws InterruptedException
	{
		TaskInfo task = (TaskInfo)threadContext;

		task.State = TextureRequestState.Progress;

		//if DEBUG_TIMING
		task.NetworkTime = Utils.getUnixTime();
		//endif
		// Find the first missing packet in the download
		//ushort
		int packet = 0;
		synchronized (task.Transfer) 
		{
			if (task.Transfer.PacketsSeen != null && task.Transfer.PacketsSeen.size() > 0)
				packet = GetFirstMissingPacket(task.Transfer.PacketsSeen);
		}

		// Request the texture
		RequestImage(task.RequestID, task.Type, task.Transfer.Priority, task.Transfer.DiscardLevel, packet);

		// Set starting time
		task.Transfer.setTimeSinceLastPacket(0);

		// Don't release this worker slot until texture is downloaded or timeout occurs

		if (!resetEvents[task.RequestSlot].waitOne(Settings.PIPELINE_REQUEST_TIMEOUT))
		{
			// Timed out
			JLogger.warn("Worker " + task.RequestSlot + " timeout waiting for texture " + task.RequestID + " to download got " +
					task.Transfer.Transferred + " of " + task.Transfer.Size);

			AssetTexture texture = new AssetTexture(task.RequestID, task.Transfer.AssetData);
			for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback: task.Callbacks)
			{
				callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Timeout, texture));
			}

			_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred, task.Transfer.Size);

			RemoveTransfer(task.RequestID);
		}

		// Free up this download slot
		synchronized (lockerObject)
		{threadpoolSlots[task.RequestSlot] = -1;}
	}

	private int GetFirstMissingPacket(SortedMap<Integer, Integer> packetsSeen)
	{
		int packet = 0;

		synchronized (packetsSeen)
		{
			boolean first = true;
			for (Entry<Integer, Integer> packetSeen : packetsSeen.entrySet())
			{
				if (first)
				{
					// Initially set this to the earliest packet received in the transfer
					packet = packetSeen.getValue();
					first = false;
				}
				else
				{
					++packet;

					// If there is a missing packet in the list, break and request the download
					// resume here
					if (packetSeen.getValue() != packet)
					{
						--packet;
						break;
					}
				}
			}

			++packet;
		}

		return packet;
	}

	//region Raw Packet Handlers

	/// <summary>
	/// Handle responses from the simulator that tell us a texture we have requested is unable to be located
	/// or no longer exists. This will remove the request from the pipeline and free up a slot if one is in use
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ImageNotInDatabaseHandler(Object sender, PacketReceivedEventArgs e)
	{
		ImageNotInDatabasePacket imageNotFoundData = (ImageNotInDatabasePacket)e.getPacket();
		TaskInfo task;

		if ((task = TryGetTransferValue(imageNotFoundData.ImageID.ID))!=null)
		{
			// cancel acive request and free up the threadpool slot
			if (task.State == TextureRequestState.Progress)
				resetEvents[task.RequestSlot].set();

			// fire callback to inform the caller 
			for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback: task.Callbacks)
				callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.NotFound, new AssetTexture(imageNotFoundData.ImageID.ID, Utils.EmptyBytes)));

			resetEvents[task.RequestSlot].set();

			RemoveTransfer(imageNotFoundData.ImageID.ID);
		}
		else
		{
			JLogger.warn("Received an ImageNotFound packet for an image we did not request: " + imageNotFoundData.ImageID.ID);
		}
	}

	/// <summary>
	/// Handles the remaining Image data that did not fit in the initial ImageData packet
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ImagePacketHandler(Object sender, PacketReceivedEventArgs e) throws InterruptedException
	{
		ImagePacketPacket image = (ImagePacketPacket)e.getPacket();
		TaskInfo task;

		if ((task = TryGetTransferValue(image.ImageID.ID))!=null)
		{
			if (task.Transfer.Size == 0)
			{
				// We haven't received the header yet, block until it's received or times out
				task.Transfer.HeaderReceivedEvent.waitOne(1000 * 5);

				if (task.Transfer.Size == 0)
				{
					JLogger.warn("Timed out while waiting for the image header to download for " +
							task.Transfer.ID);

					RemoveTransfer(task.Transfer.ID);
					resetEvents[task.RequestSlot].set(); // free up request slot

					for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback: task.Callbacks)
						callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Timeout, new AssetTexture(task.RequestID, task.Transfer.AssetData)));

					return;
				}
			}

			// The header is downloaded, we can insert this data in to the proper position
			// Only insert if we haven't seen this packet before
			synchronized (task.Transfer)
			{
				if (!task.Transfer.PacketsSeen.containsKey(image.ImageID.Packet))
				{
					task.Transfer.PacketsSeen.put(image.ImageID.Packet, image.ImageID.Packet);
					Utils.arraycopy(image.ImageData.Data, 0, task.Transfer.AssetData,
							task.Transfer.InitialDataSize + (1000 * (image.ImageID.Packet - 1)),
							image.ImageData.Data.length);
					task.Transfer.Transferred += image.ImageData.Data.length;
				}
			}

			task.Transfer.setTimeSinceLastPacket(0);

			if (task.Transfer.Transferred >= task.Transfer.Size)
			{
				//if DEBUG_TIMING
				long stopTime = Utils.getUnixTime();
				long requestDuration = stopTime - task.StartTime;

				long networkDuration = stopTime - task.NetworkTime;

				TotalTime += requestDuration;
				NetworkTime += networkDuration;
				TotalBytes += task.Transfer.Size;

				JLogger.debug(
						String.format(
								"Transfer Complete {0} [{1}] Total Request Time: {2}, Download Time {3}, Network {4}Kb/sec, Image Size {5} bytes",
								task.RequestID, task.RequestSlot, requestDuration, networkDuration,
								Math.round(task.Transfer.Size/networkDuration/(60*60)), task.Transfer.Size));
				//endif

				task.Transfer.Success = true;
				RemoveTransfer(task.Transfer.ID);
				resetEvents[task.RequestSlot].set(); // free up request slot
				_Client.assets.Cache.saveAssetToCache(task.RequestID, task.Transfer.AssetData);
				for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback: task.Callbacks)
					callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Finished, new AssetTexture(task.RequestID, task.Transfer.AssetData)));

				_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred, task.Transfer.Size);
			}
			else
			{
				if (task.ReportProgress)
				{
					for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback: task.Callbacks)
						callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Progress,
								new AssetTexture(task.RequestID, task.Transfer.AssetData)));
				}
				_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred,
						task.Transfer.Size);
			}
		}
	}

	/// <summary>
	/// Handle the initial ImageDataPacket sent from the simulator
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ImageDataHandler(Object sender, PacketReceivedEventArgs e)
	{
		ImageDataPacket data = (ImageDataPacket)e.getPacket();
		TaskInfo task;

		if ((task = TryGetTransferValue(data.ImageID.ID))!=null)
		{
			// reset the timeout interval since we got data
			task.Transfer.setTimeSinceLastPacket(0);

			synchronized (task.Transfer)
			{
				if (task.Transfer.Size == 0)
				{
					task.Transfer.Codec = ImageCodec.get(data.ImageID.Codec);
					task.Transfer.PacketCount = data.ImageID.Packets;
					task.Transfer.Size = (int)data.ImageID.Size;
					task.Transfer.AssetData = new byte[task.Transfer.Size];
					task.Transfer.AssetType = AssetType.Texture;
					task.Transfer.PacketsSeen = new TreeMap<Integer, Integer>();
					Utils.arraycopy(data.ImageData.Data, 0, task.Transfer.AssetData, 0, data.ImageData.Data.length);
					task.Transfer.InitialDataSize = data.ImageData.Data.length;
					task.Transfer.Transferred += data.ImageData.Data.length;
				}
			}

			task.Transfer.HeaderReceivedEvent.set();

			if (task.Transfer.Transferred >= task.Transfer.Size)
			{
				//if DEBUG_TIMING
				long stopTime = Utils.getUnixTime();
				long requestDuration = stopTime - task.StartTime;

				long networkDuration = stopTime - task.NetworkTime;

				TotalTime += requestDuration;
				NetworkTime += networkDuration;
				TotalBytes += task.Transfer.Size;

				JLogger.debug(
						String.format(
								"Transfer Complete {0} [{1}] Total Request Time: {2}, Download Time {3}, Network {4}Kb/sec, Image Size {5} bytes",
								task.RequestID, task.RequestSlot, requestDuration, networkDuration,
								Math.round(task.Transfer.Size/networkDuration/(60*60)), task.Transfer.Size));
				//endif
				task.Transfer.Success = true;
				RemoveTransfer(task.RequestID);
				resetEvents[task.RequestSlot].set();

				_Client.assets.Cache.saveAssetToCache(task.RequestID, task.Transfer.AssetData);

				for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback : task.Callbacks)
					callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Finished, new AssetTexture(task.RequestID, task.Transfer.AssetData)));

				_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred, task.Transfer.Size);
			}
			else
			{
				if (task.ReportProgress)
				{
					for (MethodDelegate<Void, TextureDownloadCallbackArgs> callback: task.Callbacks)
						callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Progress,
								new AssetTexture(task.RequestID, task.Transfer.AssetData)));
				}

				_Client.assets.FireImageProgressEvent(task.RequestID, task.Transfer.Transferred,
						task.Transfer.Size);
			}
		}
	}

	//endregion

	private TaskInfo TryGetTransferValue(UUID textureID)
	{
		synchronized (_Transfers)
		{
			return _Transfers.get(textureID);
		}
	}

	private TaskInfo RemoveTransfer(UUID textureID)
	{
		synchronized (_Transfers)
		{
			return _Transfers.remove(textureID);
		}
	}
}