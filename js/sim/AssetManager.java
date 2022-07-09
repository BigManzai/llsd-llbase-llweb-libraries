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

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.cap.http.DownloadManager;
import com.ngt.jopenmetaverse.shared.cap.http.HttpBaseDownloadProgressArg;
import com.ngt.jopenmetaverse.shared.cap.http.HttpBaseRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.cap.http.DownloadManager.DownloadRequest;
import com.ngt.jopenmetaverse.shared.protocol.AbortXferPacket;
import com.ngt.jopenmetaverse.shared.protocol.AssetUploadCompletePacket;
import com.ngt.jopenmetaverse.shared.protocol.AssetUploadRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.ConfirmXferPacketPacket;
import com.ngt.jopenmetaverse.shared.protocol.InitiateDownloadPacket;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.RequestXferPacket;
import com.ngt.jopenmetaverse.shared.protocol.SendXferPacketPacket;
import com.ngt.jopenmetaverse.shared.protocol.TransferAbortPacket;
import com.ngt.jopenmetaverse.shared.protocol.TransferInfoPacket;
import com.ngt.jopenmetaverse.shared.protocol.TransferPacketPacket;
import com.ngt.jopenmetaverse.shared.protocol.TransferRequestPacket;
import com.ngt.jopenmetaverse.shared.sim.asset.Asset;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetAnimation;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetBodypart;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetClothing;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetLandmark;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetMesh;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetNotecard;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetPrim;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetScriptBinary;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetScriptText;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetSound;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetTexture;
import com.ngt.jopenmetaverse.shared.sim.asset.pipeline.TexturePipeline;
import com.ngt.jopenmetaverse.shared.sim.asset.pipeline.TexturePipeline.TextureRequestState;
import com.ngt.jopenmetaverse.shared.sim.cache.AssetCache;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.Caps;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetDownload;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetReceivedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetUpload;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetUploadEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.BakedTextureUploadedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.ImageDownload;
import com.ngt.jopenmetaverse.shared.sim.events.asm.ImageReceiveProgressEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.InitiateDownloadEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.MeshDownloadCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.TextureDownloadCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.Transfer;
import com.ngt.jopenmetaverse.shared.sim.events.asm.XferDownload;
import com.ngt.jopenmetaverse.shared.sim.events.asm.XferReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryItem;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.UploadBakedTextureMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.UploaderRequestComplete;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.UploaderRequestUpload;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class AssetManager {
    
    //region Enums

    public enum EstateAssetTypeint
    {
        None (-1),
        Covenant (0);
        private int index;
        EstateAssetTypeint(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,EstateAssetTypeint> lookup  
		= new HashMap<Integer,EstateAssetTypeint>();

		static {
			for(EstateAssetTypeint s : EnumSet.allOf(EstateAssetTypeint.class))
				lookup.put(s.getIndex(), s);
		}

		public static EstateAssetTypeint get(Integer index)
		{
			return lookup.get(index);
		}   
    }

    /// <summary>
    /// 
    /// </summary>
    public enum StatusCode
    {
        /// <summary>OK</summary>
        OK(0),
        /// <summary>Transfer completed</summary>
        Done(1),
        /// <summary></summary>
        Skip(2),
        /// <summary></summary>
        Abort(3),
        /// <summary>Unknown error occurred</summary>
        Error(-1),
        /// <summary>Equivalent to a 404 error</summary>
        UnknownSource(-2),
        /// <summary>Client does not have permission for that resource</summary>
        InsufficientPermissions(-3),
        /// <summary>Unknown status</summary>
        Unknown(-4);
        private int index;
        StatusCode(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,StatusCode> lookup  
		= new HashMap<Integer,StatusCode>();

		static {
			for(StatusCode s : EnumSet.allOf(StatusCode.class))
				lookup.put(s.getIndex(), s);
		}

		public static StatusCode get(Integer index)
		{
			return lookup.get(index);
		}   
    }

    /// <summary>
    /// 
    /// </summary>
    public enum ChannelType
    {
        /// <summary></summary>
        Unknown(0),
        /// <summary>Unknown</summary>
        Misc (1),
        /// <summary>Virtually all asset transfers use this channel</summary>
        Asset (2);
        
        private int index;
        ChannelType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,ChannelType> lookup  
		= new HashMap<Integer,ChannelType>();

		static {
			for(ChannelType s : EnumSet.allOf(ChannelType.class))
				lookup.put(s.getIndex(), s);
		}

		public static ChannelType get(Integer index)
		{
			return lookup.get(index);
		}   
    }

    /// <summary>
    /// 
    /// </summary>
    public enum SourceType
    {
        /// <summary></summary>
        Unknown(0),
        /// <summary>Asset from the asset server</summary>
        Asset (2),
        /// <summary>Inventory item</summary>
        SimInventoryItem (3),
        /// <summary>Estate asset, such as an estate covenant</summary>
        SimEstate (4);
        
        private int index;
        SourceType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,SourceType> lookup  
		= new HashMap<Integer,SourceType>();

		static {
			for(SourceType s : EnumSet.allOf(SourceType.class))
				lookup.put(s.getIndex(), s);
		}

		public static SourceType get(Integer index)
		{
			return lookup.get(index);
		}   
    }

    /// <summary>
    /// 
    /// </summary>
    public enum TargetType 
    {
        /// <summary></summary>
        Unknown (0),
        /// <summary></summary>
        File (1),
        /// <summary></summary>
        VFile (2);
        
        private int index;
        TargetType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,TargetType> lookup  
		= new HashMap<Integer,TargetType>();

		static {
			for(TargetType s : EnumSet.allOf(TargetType.class))
				lookup.put(s.getIndex(), s);
		}

		public static TargetType get(Integer index)
		{
			return lookup.get(index);
		}   
    }

    /// <summary>
    /// 
    /// </summary>
    public static enum ImageType 
    {
        /// <summary></summary>
        Normal((byte)0),
        /// <summary></summary>
        Baked ((byte)1);
		private byte index;
		ImageType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ImageType> lookup  = new HashMap<Byte,ImageType>();

		static {
			for(ImageType s : EnumSet.allOf(ImageType.class))
				lookup.put(s.getIndex(), s);
		}

		public static ImageType get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Image file format
    /// </summary>
    public enum ImageCodec
    {
        Invalid((byte)0),
        RGB((byte)1),
        J2C((byte)2),
        BMP((byte)3),
        TGA((byte)4),
        JPEG((byte)5),
        DXT ((byte)6),
        PNG ((byte)7);
        
    	private byte index;
    	ImageCodec(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ImageCodec> lookup  = new HashMap<Byte,ImageCodec>();

		static {
			for(ImageCodec s : EnumSet.allOf(ImageCodec.class))
				lookup.put(s.getIndex(), s);
		}

		public static ImageCodec get(Byte index)
		{
			return lookup.get(index);
		}
    }

    public static enum TransferError 
    {
        None(0),
        Failed(-1),
        AssetNotFound(-3),
        AssetNotFoundInDatabase(-4),
        InsufficientPermissions(-5),
        EOF(-39),
        CannotOpenFile(-42),
        FileNotFound(-43),
        FileIsEmpty(-44),
        TCPTimeout(-23016),
        CircuitGone(-23017);
        private int index;
        TransferError(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,TransferError> lookup  
		= new HashMap<Integer,TransferError>();

		static {
			for(TransferError s : EnumSet.allOf(TransferError.class))
				lookup.put(s.getIndex(), s);
		}

		public static TransferError get(Integer index)
		{
			return lookup.get(index);
		}
    }

    public static class UploadRequestResult
    {
    	UUID transactionID;
    	UUID agentID;
    	
		public UploadRequestResult() {
			super();
			transactionID = new UUID();
			agentID = new UUID();
		}
		
		public UploadRequestResult(UUID transactionID, UUID agentID) {
			super();
			this.transactionID = transactionID;
			this.agentID = agentID;
		}
		public UUID getTransactionID() {
			return transactionID;
		}
		public void setTransactionID(UUID transactionID) {
			this.transactionID = transactionID;
		}
		public UUID getAgentID() {
			return agentID;
		}
		public void setAgentID(UUID agentID) {
			this.agentID = agentID;
		}
    	
    }
    
    //endregion Enums   
    
	
    /// <summary>Number of milliseconds to wait for a transfer header packet if out of order data was received</summary>
    final int TRANSFER_HEADER_TIMEOUT = 1000 * 15;

    //region Delegates
    /// <summary>
    /// Callback used for various asset download requests
    /// </summary>
    /// <param name="transfer">Transfer information</param>
    /// <param name="asset">Downloaded asset, null on fail</param>
    
    public MethodDelegate<Void, AssetReceivedCallbackArgs> assetReceivedCallback; 
//    public delegate void AssetReceivedCallback(AssetDownload transfer, Asset asset);
    /// <summary>
    /// Callback used upon competition of baked texture upload
    /// </summary>
    /// <param name="newAssetID">Asset UUID of the newly uploaded baked texture</param>
    public MethodDelegate<Void, BakedTextureUploadedCallbackArgs> bakedTextureUploadedCallback; 
    
//    public delegate void BakedTextureUploadedCallback(UUID newAssetID);
    /// <summary>
    /// A callback that fires upon the completition of the RequestMesh call
    /// </summary>
    /// <param name="success">Was the download successfull</param>
    /// <param name="assetMesh">Resulting mesh or null on problems</param>

    public MethodDelegate<Void, MeshDownloadCallbackArgs> meshDownloadCallback; 
//    public delegate void MeshDownloadCallback(boolean success, AssetMesh assetMesh);

    //endregion Delegates

    //region Events
    private EventObservable<XferReceivedEventArgs> onXferReceived = new EventObservable<XferReceivedEventArgs>();
    public void registerOnXferReceived(EventObserver<XferReceivedEventArgs> o)
    {
    	onXferReceived.addObserver(o);
    }
    public void unregisterOnXferReceived(EventObserver<XferReceivedEventArgs> o) 
    {
    	onXferReceived.deleteObserver(o);
    }
    private EventObservable<AssetUploadEventArgs> onAssetUploaded = new EventObservable<AssetUploadEventArgs>();
    public void registerOnAssetUploaded(EventObserver<AssetUploadEventArgs> o)
    {
    	onAssetUploaded.addObserver(o);
    }
    public void unregisterOnAssetUploaded(EventObserver<AssetUploadEventArgs> o) 
    {
    	onAssetUploaded.deleteObserver(o);
    }
    private EventObservable<AssetUploadEventArgs> onUploadProgress = new EventObservable<AssetUploadEventArgs>();
    public void registerOnUploadProgress(EventObserver<AssetUploadEventArgs> o)
    {
    	onUploadProgress.addObserver(o);
    }
    public void unregisterOnUploadProgress(EventObserver<AssetUploadEventArgs> o) 
    {
    	onUploadProgress.deleteObserver(o);
    }
    private EventObservable<InitiateDownloadEventArgs> onInitiateDownload = new EventObservable<InitiateDownloadEventArgs>();
    public void registerOnInitiateDownload(EventObserver<InitiateDownloadEventArgs> o)
    {
    	onInitiateDownload.addObserver(o);
    }
    public void unregisterOnInitiateDownload(EventObserver<InitiateDownloadEventArgs> o) 
    {
    	onInitiateDownload.deleteObserver(o);
    }
    private EventObservable<ImageReceiveProgressEventArgs> onImageReceiveProgress = new EventObservable<ImageReceiveProgressEventArgs>();
    public void registerOnImageReceiveProgress(EventObserver<ImageReceiveProgressEventArgs> o)
    {
    	onImageReceiveProgress.addObserver(o);
    }
    public void unregisterOnImageReceiveProgress(EventObserver<ImageReceiveProgressEventArgs> o) 
    {
    	onImageReceiveProgress.deleteObserver(o);
    }
    
//    //region XferReceived
//    /// <summary>The event subscribers. null if no subcribers</summary>
//    private EventHandler<XferReceivedEventArgs> m_XferReceivedEvent;
//
//    /// <summary>Raises the XferReceived event</summary>
//    /// <param name="e">A XferReceivedEventArgs object containing the
//    /// data returned from the simulator</param>
//    protected virtual void OnXferReceived(XferReceivedEventArgs e)
//    {
//        EventHandler<XferReceivedEventArgs> handler = m_XferReceivedEvent;
//        if (handler != null)
//            handler(this, e);
//    }
//
//    /// <summary>Thread sync lock object</summary>
//    private readonly object m_XferReceivedLock = new object();
//
//    /// <summary>Raised when the simulator responds sends </summary>
//    public event EventHandler<XferReceivedEventArgs> XferReceived 
//    {
//        add { lock (m_XferReceivedLock) { m_XferReceivedEvent += value; } }
//        remove { lock (m_XferReceivedLock) { m_XferReceivedEvent -= value; } }
//    }
//    //endregion
//
//    //region AssetUploaded
//    /// <summary>The event subscribers. null if no subcribers</summary>
//    private EventHandler<AssetUploadEventArgs> m_AssetUploadedEvent;
//
//    /// <summary>Raises the AssetUploaded event</summary>
//    /// <param name="e">A AssetUploadedEventArgs object containing the
//    /// data returned from the simulator</param>
//    protected virtual void OnAssetUploaded(AssetUploadEventArgs e)
//    {
//        EventHandler<AssetUploadEventArgs> handler = m_AssetUploadedEvent;
//        if (handler != null)
//            handler(this, e);
//    }
//
//    /// <summary>Thread sync lock object</summary>
//    private readonly object m_AssetUploadedLock = new object();
//
//    /// <summary>Raised during upload completes</summary>
//    public event EventHandler<AssetUploadEventArgs> AssetUploaded 
//    {
//        add { lock (m_AssetUploadedLock) { m_AssetUploadedEvent += value; } }
//        remove { lock (m_AssetUploadedLock) { m_AssetUploadedEvent -= value; } }
//    }
//    //endregion
//
//    //region UploadProgress
//    /// <summary>The event subscribers. null if no subcribers</summary>
//    private EventHandler<AssetUploadEventArgs> m_UploadProgressEvent;
//
//    /// <summary>Raises the UploadProgress event</summary>
//    /// <param name="e">A UploadProgressEventArgs object containing the
//    /// data returned from the simulator</param>
//    protected virtual void OnUploadProgress(AssetUploadEventArgs e)
//    {
//        EventHandler<AssetUploadEventArgs> handler = m_UploadProgressEvent;
//        if (handler != null)
//            handler(this, e);
//    }
//
//    /// <summary>Thread sync lock object</summary>
//    private readonly object m_UploadProgressLock = new object();
//
//    /// <summary>Raised during upload with progres update</summary>
//    public event EventHandler<AssetUploadEventArgs> UploadProgress 
//    {
//        add { lock (m_UploadProgressLock) { m_UploadProgressEvent += value; } }
//        remove { lock (m_UploadProgressLock) { m_UploadProgressEvent -= value; } }
//    }
//    //endregion UploadProgress
//
//    //region InitiateDownload
//    /// <summary>The event subscribers. null if no subcribers</summary>
//    private EventHandler<InitiateDownloadEventArgs> m_InitiateDownloadEvent;
//
//    /// <summary>Raises the InitiateDownload event</summary>
//    /// <param name="e">A InitiateDownloadEventArgs object containing the
//    /// data returned from the simulator</param>
//    protected virtual void OnInitiateDownload(InitiateDownloadEventArgs e)
//    {
//        EventHandler<InitiateDownloadEventArgs> handler = m_InitiateDownloadEvent;
//        if (handler != null)
//            handler(this, e);
//    }
//
//    /// <summary>Thread sync lock object</summary>
//    private readonly object m_InitiateDownloadLock = new object();
//
//    /// <summary>Fired when the simulator sends an InitiateDownloadPacket, used to download terrain .raw files</summary>
//    public event EventHandler<InitiateDownloadEventArgs> InitiateDownload 
//    {
//        add { lock (m_InitiateDownloadLock) { m_InitiateDownloadEvent += value; } }
//        remove { lock (m_InitiateDownloadLock) { m_InitiateDownloadEvent -= value; } }
//    }
//    //endregion InitiateDownload
//
//    //region ImageReceiveProgress
//    /// <summary>The event subscribers. null if no subcribers</summary>
//    private EventHandler<ImageReceiveProgressEventArgs> m_ImageReceiveProgressEvent;
//
//    /// <summary>Raises the ImageReceiveProgress event</summary>
//    /// <param name="e">A ImageReceiveProgressEventArgs object containing the
//    /// data returned from the simulator</param>
//    protected virtual void OnImageReceiveProgress(ImageReceiveProgressEventArgs e)
//    {
//        EventHandler<ImageReceiveProgressEventArgs> handler = m_ImageReceiveProgressEvent;
//        if (handler != null)
//            handler(this, e);
//    }
//
//    /// <summary>Thread sync lock object</summary>
//    private readonly object m_ImageReceiveProgressLock = new object();
//
//    /// <summary>Fired when a texture is in the process of being downloaded by the TexturePipeline class</summary>
//    public event EventHandler<ImageReceiveProgressEventArgs> ImageReceiveProgress 
//    {
//        add { lock (m_ImageReceiveProgressLock) { m_ImageReceiveProgressEvent += value; } }
//        remove { lock (m_ImageReceiveProgressLock) { m_ImageReceiveProgressEvent -= value; } }
//    }
//    //endregion ImageReceiveProgress
//
//    //endregion Events

    
    
    
	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();
    
    /// <summary>Texture download cache</summary>
    public AssetCache Cache;

    private TexturePipeline Texture;

    private DownloadManager HttpDownloads;

    private GridClient Client;

    private Map<UUID, Transfer> Transfers = new HashMap<UUID, Transfer>();

    private AssetUpload PendingUpload;
    private Object PendingUploadLock = new Object();
    private volatile boolean WaitingForUploadConfirm = false;

    /// <summary>
    /// Default constructor
    /// </summary>
    /// <param name="client">A reference to the GridClient object</param>
    public AssetManager(GridClient client)
    {
        Client = client;
        Cache = new AssetCache(client);
        Texture = new TexturePipeline(client);
        HttpDownloads = new DownloadManager();

        //        // Transfer packets for downloading large assets
        //        // Client.network.RegisterCallback(PacketType.TransferInfo, TransferInfoHandler);
        Client.network.RegisterCallback(PacketType.TransferInfo, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ TransferInfoHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        
        //        // Client.network.RegisterCallback(PacketType.TransferPacket, TransferPacketHandler);
        Client.network.RegisterCallback(PacketType.TransferPacket, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ TransferPacketHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        
        //        // Xfer packets for uploading large assets
        //        // Client.network.RegisterCallback(PacketType.RequestXfer, RequestXferHandler);
        Client.network.RegisterCallback(PacketType.RequestXfer, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ RequestXferHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        
        //        // Client.network.RegisterCallback(PacketType.ConfirmXferPacket, ConfirmXferPacketHandler);
        Client.network.RegisterCallback(PacketType.ConfirmXferPacket, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ ConfirmXferPacketHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        //        // Client.network.RegisterCallback(PacketType.AssetUploadComplete, AssetUploadCompleteHandler);

        Client.network.RegisterCallback(PacketType.AssetUploadComplete, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ AssetUploadCompleteHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        
                // Xfer packets for downloading misc assets
        //        // Client.network.RegisterCallback(PacketType.SendXferPacket, SendXferPacketHandler);
        Client.network.RegisterCallback(PacketType.SendXferPacket, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ SendXferPacketHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        
        //        // Client.network.RegisterCallback(PacketType.AbortXfer, AbortXferHandler);
        Client.network.RegisterCallback(PacketType.AbortXfer, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ AbortXferHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
        
        //        // Simulator is responding to a request to download a file
        //        // Client.network.RegisterCallback(PacketType.InitiateDownload, InitiateDownloadPacketHandler);
        Client.network.RegisterCallback(PacketType.InitiateDownload, new EventObserver<PacketReceivedEventArgs>()
        		{ 
        	@Override
        	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		try{ InitiateDownloadPacketHandler(o, arg);}
        		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	}}
        		);
    }
    
    /// <summary>
    /// Request an asset download
    /// </summary>
    /// <param name="assetID">Asset UUID</param>
    /// <param name="type">Asset type, must be correct for the transfer to succeed</param>
    /// <param name="priority">Whether to give this transfer an elevated priority</param>
    /// <param name="callback">The callback to fire when the simulator responds with the asset data</param>
    public void RequestAsset(UUID assetID, AssetType type, boolean priority, 
    		MethodDelegate<Void, AssetReceivedCallbackArgs> callback)
    {
        RequestAsset(assetID, type, priority, SourceType.Asset, UUID.Random(), callback);
    }

    /// <summary>
    /// Request an asset download
    /// </summary>
    /// <param name="assetID">Asset UUID</param>
    /// <param name="type">Asset type, must be correct for the transfer to succeed</param>
    /// <param name="priority">Whether to give this transfer an elevated priority</param>
    /// <param name="sourceType">Source location of the requested asset</param>
    /// <param name="callback">The callback to fire when the simulator responds with the asset data</param>
    public void RequestAsset(UUID assetID, AssetType type, boolean priority, SourceType sourceType, MethodDelegate<Void, AssetReceivedCallbackArgs> callback)
    {
        RequestAsset(assetID, type, priority, sourceType, UUID.Random(), callback);
    }

    /// <summary>
    /// Request an asset download
    /// </summary>
    /// <param name="assetID">Asset UUID</param>
    /// <param name="type">Asset type, must be correct for the transfer to succeed</param>
    /// <param name="priority">Whether to give this transfer an elevated priority</param>
    /// <param name="sourceType">Source location of the requested asset</param>
    /// <param name="transactionID">UUID of the transaction</param>
    /// <param name="callback">The callback to fire when the simulator responds with the asset data</param>
    public void RequestAsset(UUID assetID, AssetType type, boolean priority, 
    		SourceType sourceType, UUID transactionID, 
    		MethodDelegate<Void, AssetReceivedCallbackArgs> callback)
    {
        AssetDownload transfer = new AssetDownload();
        transfer.ID = transactionID;
        transfer.AssetID = assetID;
        //transfer.AssetType = type; // Set in TransferInfoHandler.
        transfer.Priority = 100.0f + (priority ? 1.0f : 0.0f);
        transfer.Channel = ChannelType.Asset;
        transfer.Source = sourceType;
        transfer.Simulator = Client.network.getCurrentSim();
        transfer.Callback = callback;

        // Check asset cache first
        if (callback != null && Cache.hasAsset(assetID))
        {
            byte[] data = Cache.getCachedAssetBytes(assetID);
            transfer.AssetData = data;
            transfer.Success = true;
            transfer.Status = StatusCode.OK;

            Asset asset = CreateAssetWrapper(type);
            asset.AssetData = data;
            asset.setAssetID(assetID);

            try { callback.execute(new AssetReceivedCallbackArgs(transfer, asset)); }
            catch (Exception e) { JLogger.error(e); }

            return;
        }

        // Add this transfer to the dictionary
        synchronized (Transfers) 
        {
        	Transfers.put(transfer.ID, transfer);
        }

        // Build the request packet and send it
        TransferRequestPacket request = new TransferRequestPacket();
        request.TransferInfo.ChannelType = (int)transfer.Channel.getIndex();
        request.TransferInfo.Priority = transfer.Priority;
        request.TransferInfo.SourceType = (int)transfer.Source.getIndex();
        request.TransferInfo.TransferID = transfer.ID;

        byte[] paramField = new byte[20];
        Utils.arraycopy(assetID.GetBytes(), 0, paramField, 0, 16);
        Utils.arraycopy(Utils.intToBytes(Utils.ubyteToInt(type.getIndex())), 0, paramField, 16, 4);
        request.TransferInfo.Params = paramField;

        Client.network.SendPacket(request, transfer.Simulator);
    }

    /// <summary>
    /// Request an asset download through the almost deprecated Xfer system
    /// </summary>
    /// <param name="filename">Filename of the asset to request</param>
    /// <param name="deleteOnCompletion">Whether or not to delete the asset
    /// off the server after it is retrieved</param>
    /// <param name="useBigPackets">Use large transfer packets or not</param>
    /// <param name="vFileID">UUID of the file to request, if filename is
    /// left empty</param>
    /// <param name="vFileType">Asset type of <code>vFileID</code>, or
    /// <code>AssetType.Unknown</code> if filename is not empty</param>
    /// <param name="fromCache">Sets the FilePath in the request to Cache
    /// (4) if true, otherwise Unknown (0) is used</param>
    /// <returns></returns>
    public BigInteger RequestAssetXfer(String filename, boolean deleteOnCompletion, boolean useBigPackets, UUID vFileID, AssetType vFileType,
        boolean fromCache)
    {
        UUID uuid = UUID.Random();
        BigInteger id = uuid.GetULong();

        XferDownload transfer = new XferDownload();
        transfer.XferID = id;
        transfer.ID = new UUID(id.longValue()); // Our dictionary tracks transfers with UUIDs, so convert the ulong back
        transfer.Filename = filename;
        transfer.VFileID = vFileID;
        transfer.AssetType = vFileType;

        // Add this transfer to the dictionary
        synchronized(Transfers) 
        {
        	Transfers.put(transfer.ID, transfer);
        }

        RequestXferPacket request = new RequestXferPacket();
        request.XferID.ID = id;
        request.XferID.Filename = Utils.stringToBytesWithTrailingNullByte(filename);
        request.XferID.FilePath = fromCache ? (byte)4 : (byte)0;
        request.XferID.DeleteOnCompletion = deleteOnCompletion;
        request.XferID.UseBigPackets = useBigPackets;
        request.XferID.VFileID = vFileID;
        request.XferID.VFileType = (short)Utils.ubyteToInt(vFileType.getIndex());

        Client.network.SendPacket(request);

        return id;
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="assetID">Use UUID.Zero if you do not have the 
    /// asset ID but have all the necessary permissions</param>
    /// <param name="itemID">The item ID of this asset in the inventory</param>
    /// <param name="taskID">Use UUID.Zero if you are not requesting an 
    /// asset from an object inventory</param>
    /// <param name="ownerID">The owner of this asset</param>
    /// <param name="type">Asset type</param>
    /// <param name="priority">Whether to prioritize this asset download or not</param>
    /// <param name="callback"></param>
    public void RequestInventoryAsset(UUID assetID, UUID itemID, UUID taskID, 
    		UUID ownerID, AssetType type, boolean priority, 
    		MethodDelegate<Void, AssetReceivedCallbackArgs> callback)
    {
        AssetDownload transfer = new AssetDownload();
        transfer.ID = UUID.Random();
        transfer.AssetID = assetID;
        //transfer.AssetType = type; // Set in TransferInfoHandler.
        transfer.Priority = 100.0f + (priority ? 1.0f : 0.0f);
        transfer.Channel = ChannelType.Asset;
        transfer.Source = SourceType.SimInventoryItem;
        transfer.Simulator = Client.network.getCurrentSim();
        transfer.Callback = callback;

        // Check asset cache first
        if (callback != null && Cache.hasAsset(assetID))
        {
            byte[] data = Cache.getCachedAssetBytes(assetID);
            transfer.AssetData = data;
            transfer.Success = true;
            transfer.Status = StatusCode.OK;

            Asset asset = CreateAssetWrapper(type);
            asset.AssetData = data;
            asset.setAssetID(assetID);

            try { callback.execute(new AssetReceivedCallbackArgs(transfer, asset)); }
            catch (Exception e) { JLogger.error(e); }

            return;
        }

        // Add this transfer to the dictionary
        synchronized(Transfers) 
        {
        	Transfers.put(transfer.ID, transfer);
        }

        // Build the request packet and send it
        TransferRequestPacket request = new TransferRequestPacket();
        request.TransferInfo.ChannelType = (int)transfer.Channel.getIndex();
        request.TransferInfo.Priority = transfer.Priority;
        request.TransferInfo.SourceType = (int)transfer.Source.getIndex();
        request.TransferInfo.TransferID = transfer.ID;

        byte[] paramField = new byte[100];
        Utils.arraycopy(Client.self.getAgentID().GetBytes(), 0, paramField, 0, 16);
        Utils.arraycopy(Client.self.getSessionID().GetBytes(), 0, paramField, 16, 16);
        Utils.arraycopy(ownerID.GetBytes(), 0, paramField, 32, 16);
        Utils.arraycopy(taskID.GetBytes(), 0, paramField, 48, 16);
        Utils.arraycopy(itemID.GetBytes(), 0, paramField, 64, 16);
        Utils.arraycopy(assetID.GetBytes(), 0, paramField, 80, 16);
        Utils.arraycopy(Utils.intToBytes(Utils.ubyteToInt(type.getIndex())), 0, paramField, 96, 4);
        request.TransferInfo.Params = paramField;

        Client.network.SendPacket(request, transfer.Simulator);
    }

    public void RequestInventoryAsset(InventoryItem item, boolean priority, MethodDelegate<Void, AssetReceivedCallbackArgs> callback)
    {
        RequestInventoryAsset(item.AssetUUID, item.UUID, UUID.Zero, item.OwnerID, item.AssetType, priority, callback);
    }

    public void RequestEstateAsset() throws Exception
    {
        throw new Exception("This function is not implemented yet!");
    }

    /// <summary>
    /// Used to force asset data into the PendingUpload property, ie: for raw terrain uploads
    /// </summary>
    /// <param name="assetData">An AssetUpload object containing the data to upload to the simulator</param>
    public void SetPendingAssetUploadData(AssetUpload assetData)
    {
        synchronized(PendingUploadLock)
        {
            PendingUpload = assetData;
        }
    }

    /// <summary>
    /// Request an asset be uploaded to the simulator
    /// </summary>
    /// <param name="asset">The <seealso cref="Asset"/> Object containing the asset data</param>
    /// <param name="storeLocal">If True, the asset once uploaded will be stored on the simulator
    /// in which the client was connected in addition to being stored on the asset server</param>
    /// <returns>The <seealso cref="UUID"/> of the transfer, can be used to correlate the upload with
    /// events being fired</returns>
    public UUID RequestUpload(Asset asset, boolean storeLocal)
    {
        if (asset.AssetData == null)
            throw new IllegalArgumentException("Can't upload an asset with no data (did you forget to call Encode?)");
        
        UploadRequestResult uploadResult = RequestUpload(asset.getAssetType(), asset.AssetData, storeLocal);
        asset.setAssetID(uploadResult.getAgentID());
        return uploadResult.getTransactionID();
    }

    /// <summary>
    /// Request an asset be uploaded to the simulator
    /// </summary>
    /// <param name="type">The <seealso cref="AssetType"/> of the asset being uploaded</param>
    /// <param name="data">A byte array containing the encoded asset data</param>
    /// <param name="storeLocal">If True, the asset once uploaded will be stored on the simulator
    /// in which the client was connected in addition to being stored on the asset server</param>
    /// <returns>The <seealso cref="UUID"/> of the transfer, can be used to correlate the upload with
    /// events being fired</returns>
    public UploadRequestResult RequestUpload(AssetType type, byte[] data, boolean storeLocal)
    {
        return RequestUpload(type, data, storeLocal);
    }

//    /// <summary>
//    /// Request an asset be uploaded to the simulator
//    /// </summary>
//    /// <param name="assetID"></param>
//    /// <param name="type">Asset type to upload this data as</param>
//    /// <param name="data">A byte array containing the encoded asset data</param>
//    /// <param name="storeLocal">If True, the asset once uploaded will be stored on the simulator
//    /// in which the client was connected in addition to being stored on the asset server</param>
//    /// <returns>The <seealso cref="UUID"/> of the transfer, can be used to correlate the upload with
//    /// events being fired</returns>
//    public UploadRequestResult RequestUpload(UUID assetID, AssetType type, byte[] data, boolean storeLocal)
//    {
//        return RequestUpload(assetID, type, data, storeLocal, UUID.Random());
//    }
    
    /// <summary>
    /// Initiate an asset upload
    /// </summary>
    /// <param name="assetID">The ID this asset will have if the
    /// upload succeeds</param>
    /// <param name="type">Asset type to upload this data as</param>
    /// <param name="data">Raw asset data to upload</param>
    /// <param name="storeLocal">Whether to store this asset on the local
    /// simulator or the grid-wide asset server</param>
    /// <param name="transactionID">The tranaction id for the upload <see cref="RequestCreateItem"/></param>
    /// <returns>The transaction ID of this transfer</returns>
    public UploadRequestResult RequestUpload(AssetType type, byte[] data, boolean storeLocal, 
    		UUID transactionID) throws Exception
    {
    	UploadRequestResult result = new UploadRequestResult();
        AssetUpload upload = new AssetUpload();
        upload.AssetData = data;
        upload.AssetType = type;
        result.setAgentID(UUID.Combine(transactionID, Client.self.getSecureSessionID()));
        upload.AssetID = result.getAgentID();
        upload.Size = data.length;
        upload.XferID = new BigInteger("0");
        upload.ID = transactionID;

        // Build and send the upload packet
        AssetUploadRequestPacket request = new AssetUploadRequestPacket();
        request.AssetBlock.StoreLocal = storeLocal;
        request.AssetBlock.Tempfile = false; // This field is deprecated
        request.AssetBlock.TransactionID = transactionID;
        request.AssetBlock.Type = type.getIndex();

        boolean isMultiPacketUpload;
        if (data.length + 100 < Settings.MAX_PACKET_SIZE)
        {
            isMultiPacketUpload = false;
            JLogger.info(
                String.format("Beginning asset upload [Single Packet], ID: %s, AssetID: %s, Size: %d",
                upload.ID.toString(), upload.AssetID.toString(), upload.Size));

            Transfers.put(upload.ID, upload);

            // The whole asset will fit in this packet, makes things easy
            request.AssetBlock.AssetData = data;
            upload.Transferred = data.length;
        }
        else
        {
            isMultiPacketUpload = true;
            JLogger.info(
                String.format("Beginning asset upload [Multiple Packets], ID: %s, AssetID: %s, Size: %d",
                upload.ID.toString(), upload.AssetID.toString(), upload.Size));

            // Asset is too big, send in multiple packets
            request.AssetBlock.AssetData = Utils.EmptyBytes;
        }

        // Wait for the previous upload to receive a RequestXferPacket
        synchronized(PendingUploadLock)
        {
            final int UPLOAD_CONFIRM_TIMEOUT = 20 * 1000;
            final int SLEEP_INTERVAL = 50;
            int t = 0;
            AutoResetEvent sleepEvent = new AutoResetEvent(false); 
            while (WaitingForUploadConfirm && t < UPLOAD_CONFIRM_TIMEOUT)
            {
//                System.Threading.Thread.Sleep(SLEEP_INTERVAL);
                sleepEvent.waitOne(SLEEP_INTERVAL);
                t += SLEEP_INTERVAL;
            }

            if (t < UPLOAD_CONFIRM_TIMEOUT)
            {
                if (isMultiPacketUpload)
                {
                    WaitingForUploadConfirm = true;
                }
                PendingUpload = upload;
                Client.network.SendPacket(request);

                result.setTransactionID(upload.ID);
                return result;
            }
            else
            {
                throw new Exception("Timeout waiting for previous asset upload to begin");
            }
        }
    }

    public void RequestUploadBakedTexture(final byte[] textureData, final MethodDelegate<Void, 
    		BakedTextureUploadedCallbackArgs> callback) throws Exception
    {
        URI url = null;

        Caps caps = Client.network.getCurrentSim().Caps;
        if (caps != null)
            url = caps.CapabilityURI("UploadBakedTexture");

        if (url != null)
        {
//            // Fetch the uploader capability
            CapsHttpClient request = new CapsHttpClient(url);
            MethodDelegate<Void, CapsHttpRequestCompletedArg> requestCompleteDelegate 
            		= new MethodDelegate<Void, CapsHttpRequestCompletedArg>()
            		{
						public Void execute(CapsHttpRequestCompletedArg e) {
//							CapsHttpClient client = e.getClient();
							OSD result = e.getResult();
							Exception error = e.getError();							
		                    if (error == null && (result instanceof OSDMap))
		                    {
		                        UploadBakedTextureMessage message = new UploadBakedTextureMessage();
		                        message.Deserialize((OSDMap)result);
		
		                        if (message.Request.State.equals("upload"))
		                        {
		                            URI uploadUrl = ((UploaderRequestUpload)message.Request).Url;
		
		                            if (uploadUrl != null)
		                            {
		                                // POST the asset data
		                            	CapsHttpClient upload = new CapsHttpClient(uploadUrl);
		                            	upload.setRequestCompleteDelegate(new MethodDelegate<Void, CapsHttpRequestCompletedArg>()
		                            			{
		            						public Void execute(CapsHttpRequestCompletedArg e2) {
		            							OSD result2 = e2.getResult();
		            							Exception error2 = e2.getError();
		            							if (error2 == null && result2 instanceof OSDMap)
		                                        {
		                                            UploadBakedTextureMessage message2 = new UploadBakedTextureMessage();
		                                            message2.Deserialize((OSDMap)result2);
		
		                                            if (message2.Request.State.equals("complete"))
		                                            {		                                            	
		                                                callback.execute(new BakedTextureUploadedCallbackArgs(((UploaderRequestComplete)message2.Request).AssetID));
		                                            }
		                                        }
                                                return null;
		            						}
		                            			});
		                                upload.BeginGetResponse(textureData, "application/octet-stream", Client.settings.CAPS_TIMEOUT);
		                                return null;
		                            }
		                        }
		                    }
		
		                    JLogger.warn("Bake upload failed during uploader retrieval");
		                    callback.execute(new BakedTextureUploadedCallbackArgs(UUID.Zero));
							return null;
						}
            		};
            		request.setRequestCompleteDelegate(requestCompleteDelegate);
                  request.BeginGetResponse(new OSDMap(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);

//            request.OnComplete +=
//                delegate(CapsClient client, OSD result, Exception error)
//                {
//                    if (error == null && result is OSDMap)
//                    {
//                        UploadBakedTextureMessage message = new UploadBakedTextureMessage();
//                        message.Deserialize((OSDMap)result);
//
//                        if (message.Request.State == "upload")
//                        {
//                            Uri uploadUrl = ((UploaderRequestUpload)message.Request).Url;
//
//                            if (uploadUrl != null)
//                            {
//                                // POST the asset data
//                                CapsClient upload = new CapsClient(uploadUrl);
//                                upload.OnComplete +=
//                                    delegate(CapsClient client2, OSD result2, Exception error2)
//                                    {
//                                        if (error2 == null && result2 is OSDMap)
//                                        {
//                                            UploadBakedTextureMessage message2 = new UploadBakedTextureMessage();
//                                            message2.Deserialize((OSDMap)result2);
//
//                                            if (message2.Request.State == "complete")
//                                            {
//                                                callback(((UploaderRequestComplete)message2.Request).AssetID);
//                                                return;
//                                            }
//                                        }
//
//                                        Logger.Log("Bake upload failed during asset upload", Helpers.LogLevel.Warning, Client);
//                                        callback(UUID.Zero);
//                                    };
//                                upload.BeginGetResponse(textureData, "application/octet-stream", Client.Settings.CAPS_TIMEOUT);
//                                return;
//                            }
//                        }
//                    }
//
//                    Logger.Log("Bake upload failed during uploader retrieval", Helpers.LogLevel.Warning, Client);
//                    callback(UUID.Zero);
//                };
//            request.BeginGetResponse(new OSDMap(), OSDFormat.Xml, Client.Settings.CAPS_TIMEOUT);
        }
        else
        {
          JLogger.info("UploadBakedTexture not available, falling back to UDP method");
          final MethodDelegate<Void, BakedTextureUploadedCallbackArgs> uploadCallback = callback;
          
          threadPool.execute(new Runnable(){
				public void run()
				{
                  final UUID transactionID = UUID.Random();
                  final AutoResetEvent uploadEvent = new AutoResetEvent(false);
                  EventObserver<AssetUploadEventArgs> udpCallback = new EventObserver<AssetUploadEventArgs>()
                		  {
							@Override
							public void handleEvent(Observable sender,
									AssetUploadEventArgs e) {
								if (e.getUpload().ID.equals(transactionID))
		                          {
		                              uploadEvent.set();
		                              uploadCallback.execute(new BakedTextureUploadedCallbackArgs(e.getUpload().Success ? e.getUpload().AssetID : UUID.Zero));
		                          }
							}
                		  };
                 

                  onAssetUploaded.addObserver(udpCallback);

                  boolean success;
                  try
                  {
                	  UploadRequestResult uploadResult = RequestUpload(AssetType.Texture, textureData, true, transactionID);
                      success = uploadEvent.waitOne(Client.settings.TRANSFER_TIMEOUT);
                  }
                  catch (Exception e)
                  {
                	  JLogger.warn(e);
                      success = false;
                  }

                  onAssetUploaded.deleteObserver(udpCallback);

                  if (!success)
                	  uploadCallback.execute(new BakedTextureUploadedCallbackArgs(UUID.Zero));
				}
			});
        	
//            Logger.Log("UploadBakedTexture not available, falling back to UDP method", Helpers.LogLevel.Info, Client);
//
//            ThreadPool.QueueUserWorkItem(
//                delegate(object o)
//                {
//                    UUID transactionID = UUID.Random();
//                    BakedTextureUploadedCallback uploadCallback = (BakedTextureUploadedCallback)o;
//                    AutoResetEvent uploadEvent = new AutoResetEvent(false);
//                    EventHandler<AssetUploadEventArgs> udpCallback =
//                        delegate(Object sender, AssetUploadEventArgs e)
//                        {
//                            if (e.Upload.ID == transactionID)
//                            {
//                                uploadEvent.Set();
//                                uploadCallback(e.Upload.Success ? e.Upload.AssetID : UUID.Zero);
//                            }
//                        };
//
//                    AssetUploaded += udpCallback;
//
//                    UUID assetID;
//                    boolean success;
//
//                    try
//                    {
//                        RequestUpload(out assetID, AssetType.Texture, textureData, true, transactionID);
//                        success = uploadEvent.WaitOne(Client.Settings.TRANSFER_TIMEOUT, false);
//                    }
//                    catch (Exception)
//                    {
//                        success = false;
//                    }
//
//                    AssetUploaded -= udpCallback;
//
//                    if (!success)
//                        uploadCallback(UUID.Zero);
//                }, callback
//            );
        }
    }

    //region Texture Downloads

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
    /// This controls the end marker of the data sent. Sending with value -1 combined with priority of 0 cancels an in-progress
    /// transfer.</param>
    /// <remarks>A bug exists in the Linden Simulator where a -1 will occasionally be sent with a non-zero priority
    /// indicating an off-by-one error.</remarks>
    /// <param name="packetStart">The packet number to begin the request at. A value of 0 begins the request
    /// from the start of the asset texture</param>
    /// <param name="callback">The <see cref="TextureDownloadCallback"/> callback to fire when the image is retrieved. The callback
    /// will contain the result of the request and the texture asset data</param>
    /// <param name="progress">If true, the callback will be fired for each chunk of the downloaded image. 
    /// The callback asset parameter will contain all previously received chunks of the texture asset starting 
    /// from the beginning of the request</param>
    /// <example>
    /// Request an image and fire a callback when the request is complete
    /// <code>
    /// Client.Assets.RequestImage(UUID.Parse("c307629f-e3a1-4487-5e88-0d96ac9d4965"), ImageType.Normal, TextureDownloader_OnDownloadFinished);
    /// 
    /// private void TextureDownloader_OnDownloadFinished(TextureRequestState state, AssetTexture asset)
    /// {
    ///     if(state == TextureRequestState.Finished)
    ///     {
    ///       Console.WriteLine("Texture {0} ({1} bytes) has been successfully downloaded", 
    ///         asset.AssetID,
    ///         asset.AssetData.Length); 
    ///     }
    /// }
    /// </code>
    /// Request an image and use an inline anonymous method to handle the downloaded texture data
    /// <code>
    /// Client.Assets.RequestImage(UUID.Parse("c307629f-e3a1-4487-5e88-0d96ac9d4965"), ImageType.Normal, delegate(TextureRequestState state, AssetTexture asset) 
    ///                                         {
    ///                                             if(state == TextureRequestState.Finished)
    ///                                             {
    ///                                                 Console.WriteLine("Texture {0} ({1} bytes) has been successfully downloaded", 
    ///                                                 asset.AssetID,
    ///                                                 asset.AssetData.Length); 
    ///                                             }
    ///                                         }
    /// );
    /// </code>
    /// Request a texture, decode the texture to a bitmap image and apply it to a imagebox 
    /// <code>
    /// Client.Assets.RequestImage(UUID.Parse("c307629f-e3a1-4487-5e88-0d96ac9d4965"), ImageType.Normal, TextureDownloader_OnDownloadFinished);
    /// 
    /// private void TextureDownloader_OnDownloadFinished(TextureRequestState state, AssetTexture asset)
    /// {
    ///     if(state == TextureRequestState.Finished)
    ///     {
    ///         ManagedImage imgData;
    ///         Image bitmap;
    ///
    ///         if (state == TextureRequestState.Finished)
    ///         {
    ///             OpenJPEG.DecodeToImage(assetTexture.AssetData, out imgData, out bitmap);
    ///             picInsignia.Image = bitmap;
    ///         }               
    ///     }
    /// }
    /// </code>
    /// </example>
    public void RequestImage(UUID textureID, ImageType imageType, float priority, int discardLevel,
        long packetStart, MethodDelegate<Void, TextureDownloadCallbackArgs> callback, boolean progress) throws Exception
    {
        if (Client.settings.USE_HTTP_TEXTURES &&
            Client.network.getCurrentSim().Caps != null &&
            Client.network.getCurrentSim().Caps.CapabilityURI("GetTexture") != null)
        {
            HttpRequestTexture(textureID, imageType, priority, discardLevel, packetStart, callback, progress);
        }
        else
        {
            Texture.RequestTexture(textureID, imageType, priority, discardLevel, packetStart, callback, progress);
        }
    }

    /// <summary>
    /// Overload: Request a texture asset from the simulator using the <see cref="TexturePipeline"/> system to 
    /// manage the requests and re-assemble the image from the packets received from the simulator
    /// </summary>
    /// <param name="textureID">The <see cref="UUID"/> of the texture asset to download</param>
    /// <param name="callback">The <see cref="TextureDownloadCallback"/> callback to fire when the image is retrieved. The callback
    /// will contain the result of the request and the texture asset data</param>
    public void RequestImage(UUID textureID, MethodDelegate<Void, TextureDownloadCallbackArgs> callback) throws Exception
    {
        RequestImage(textureID, ImageType.Normal, 101300.0f, 0, 0, callback, false);
    }

    /// <summary>
    /// Overload: Request a texture asset from the simulator using the <see cref="TexturePipeline"/> system to 
    /// manage the requests and re-assemble the image from the packets received from the simulator
    /// </summary>
    /// <param name="textureID">The <see cref="UUID"/> of the texture asset to download</param>
    /// <param name="imageType">The <see cref="ImageType"/> of the texture asset. 
    /// Use <see cref="ImageType.Normal"/> for most textures, or <see cref="ImageType.Baked"/> for baked layer texture assets</param>
    /// <param name="callback">The <see cref="TextureDownloadCallback"/> callback to fire when the image is retrieved. The callback
    /// will contain the result of the request and the texture asset data</param>
    public void RequestImage(UUID textureID, ImageType imageType, MethodDelegate<Void, TextureDownloadCallbackArgs> callback) throws Exception
    {
        RequestImage(textureID, imageType, 101300.0f, 0, 0, callback, false);
    }

    /// <summary>
    /// Overload: Request a texture asset from the simulator using the <see cref="TexturePipeline"/> system to 
    /// manage the requests and re-assemble the image from the packets received from the simulator
    /// </summary>
    /// <param name="textureID">The <see cref="UUID"/> of the texture asset to download</param>
    /// <param name="imageType">The <see cref="ImageType"/> of the texture asset. 
    /// Use <see cref="ImageType.Normal"/> for most textures, or <see cref="ImageType.Baked"/> for baked layer texture assets</param>
    /// <param name="callback">The <see cref="TextureDownloadCallback"/> callback to fire when the image is retrieved. The callback
    /// will contain the result of the request and the texture asset data</param>
    /// <param name="progress">If true, the callback will be fired for each chunk of the downloaded image. 
    /// The callback asset parameter will contain all previously received chunks of the texture asset starting 
    /// from the beginning of the request</param>
    public void RequestImage(UUID textureID, ImageType imageType, MethodDelegate<Void, TextureDownloadCallbackArgs> callback, boolean progress) throws Exception
    {
        RequestImage(textureID, imageType, 101300.0f, 0, 0, callback, progress);
    }

    /// <summary>
    /// Cancel a texture request
    /// </summary>
    /// <param name="textureID">The texture assets <see cref="UUID"/></param>
    public void RequestImageCancel(UUID textureID)
    {
        Texture.AbortTextureRequest(textureID);
    }

    /// <summary>
    /// Requests download of a mesh asset
    /// </summary>
    /// <param name="meshID">UUID of the mesh asset</param>
    /// <param name="callback">Callback when the request completes</param>
    public void RequestMesh(final UUID meshID, final MethodDelegate<Void, MeshDownloadCallbackArgs> callback) throws URISyntaxException
    {
        if (meshID.equals(UUID.Zero) || callback == null)
            return;

        if (Client.network.getCurrentSim().Caps != null &&
            Client.network.getCurrentSim().Caps.CapabilityURI("GetMesh") != null)
        {
            // Do we have this mesh asset in the cache?
            if (Client.assets.Cache.hasAsset(meshID))
            {
                callback.execute(new MeshDownloadCallbackArgs(true, new AssetMesh(meshID, Client.assets.Cache.getCachedAssetBytes(meshID))));
                return;
            }

            URI url = Client.network.getCurrentSim().Caps.CapabilityURI("GetMesh");

            MethodDelegate<Void,HttpBaseRequestCompletedArg> downloadCompletedCallback = new MethodDelegate<Void,HttpBaseRequestCompletedArg>()
            		{
						public Void execute(HttpBaseRequestCompletedArg e) {
//							HttpRequestBase request = e.getRequest();
//							HttpResponse response = e.getResponse();
							byte[] responseData = e.getResponseData();
							Exception error = e.getError();
							  if (error == null && responseData != null) // success
			                    {
			                        callback.execute(new MeshDownloadCallbackArgs(true, new AssetMesh(meshID, responseData)));
			                        Client.assets.Cache.saveAssetToCache(meshID, responseData);
			                    }
			                    else // download failed
			                    {
			                        JLogger.warn(
			                            String.format("Failed to fetch mesh asset %s: %s",
			                                meshID,
			                                (error == null) ? "" :Utils.getExceptionStackTraceAsString(error)
			                            ));
			                    }
							return null;
						}
            		};
            		
            
            DownloadRequest req = new DownloadRequest(
                new URI(String.format("%s/?mesh_id=%s", url.toString(), meshID.toString())),
                Client.settings.CAPS_TIMEOUT,
                null,
                null,
                downloadCompletedCallback
            );

            HttpDownloads.QueueDownlad(req);
        }
        else
        {
            JLogger.error("GetMesh capability not available");
            callback.execute(new MeshDownloadCallbackArgs(false, null));
        }
    }

    /// <summary>
    /// Lets TexturePipeline class fire the progress event
    /// </summary>
    /// <param name="texureID">The texture ID currently being downloaded</param>
    /// <param name="transferredBytes">the number of bytes transferred</param>
    /// <param name="totalBytes">the total number of bytes expected</param>
    public void FireImageProgressEvent(UUID texureID, int transferredBytes, int totalBytes)
    {
        try { onImageReceiveProgress.raiseEvent(new ImageReceiveProgressEventArgs(texureID, transferredBytes, totalBytes)); }
        catch (Exception e) { JLogger.error(e); }
    }

    // Helper method for downloading textures via GetTexture cap
    // Same signature as the UDP variant since we need all the params to
    // pass to the UDP TexturePipeline in case we need to fall back to it
    // (Linden servers currently (1.42) don't support bakes downloads via HTTP)
    private void HttpRequestTexture(final UUID textureID, final ImageType imageType, final float priority, final int discardLevel,
    		final long packetStart, final MethodDelegate<Void, TextureDownloadCallbackArgs> callback, final boolean progress) throws Exception
    {
        if (textureID.equals(UUID.Zero) || callback == null)
            return;

        byte[] assetData;
        // Do we have this image in the cache?
        if (Client.assets.Cache.hasAsset(textureID)
            && (assetData = Client.assets.Cache.getCachedAssetBytes(textureID)) != null)
        {
            ImageDownload image = new ImageDownload();
            image.ID = textureID;
            image.AssetData = assetData;
            image.Size = image.AssetData.length;
            image.Transferred = image.AssetData.length;
            image.ImageType = imageType;
            image.AssetType = AssetType.Texture;
            image.Success = true;

            callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Finished, new AssetTexture(image.ID, image.AssetData)));
            
            FireImageProgressEvent(image.ID, (int)image.Transferred, image.Size);
            return;
        }

        MethodDelegate<Void, HttpBaseDownloadProgressArg> progressHandler = null;
        if (progress)
        {
        	progressHandler =  new MethodDelegate<Void, HttpBaseDownloadProgressArg>(){
				public Void execute(HttpBaseDownloadProgressArg e) {
//					HttpRequestBase request = e.getRequest();
//					HttpResponse response = e.getResponse();
					int bytesReceived = e.getBytesReceived();
					int totalBytesToReceive  = e.getTotalBytesToReceive();
                    FireImageProgressEvent(textureID, bytesReceived, totalBytesToReceive);
					return null;
				}
        	};
        }
        
        MethodDelegate<Void,HttpBaseRequestCompletedArg> downloadCompletedHanlder = new MethodDelegate<Void,HttpBaseRequestCompletedArg>()
        		{
					public Void execute(HttpBaseRequestCompletedArg e) {
//						HttpRequestBase request = e.getRequest();
//						HttpResponse response = e.getResponse();
						byte[] responseData = e.getResponseData();
						Exception error = e.getError();
						 if (error == null && responseData != null) // success
			                {
			                    ImageDownload image = new ImageDownload();
			                    image.ID = textureID;
			                    image.AssetData = responseData;
			                    image.Size = image.AssetData.length;
			                    image.Transferred = image.AssetData.length;
			                    image.ImageType = imageType;
			                    image.AssetType = AssetType.Texture;
			                    image.Success = true;

			                    callback.execute(new TextureDownloadCallbackArgs(TextureRequestState.Finished, new AssetTexture(image.ID, image.AssetData)));
			                    FireImageProgressEvent(image.ID, (int)image.Transferred, image.Size);

			                    Client.assets.Cache.saveAssetToCache(textureID, responseData);
			                }
			                else // download failed
			                {
			                    JLogger.warn(
			                        String.format("Failed to fetch texture %s over HTTP, falling back to UDP: %s",
			                            textureID,
			                            (error == null) ? "" : Utils.getExceptionStackTraceAsString(error)
			                        ));

			                    Texture.RequestTexture(textureID, imageType, priority, discardLevel, packetStart, callback, progress);
			                }
						return null;
					}
        		};
        
        URI url = Client.network.getCurrentSim().Caps.CapabilityURI("GetTexture");

        DownloadRequest req = new DownloadRequest(
            new URI(String.format("%s/?texture_id=%s", url.toString(), textureID.toString())),
            Client.settings.CAPS_TIMEOUT,
            "image/x-j2c",
            progressHandler,downloadCompletedHanlder
        );

        HttpDownloads.QueueDownlad(req);

    }

    //endregion Texture Downloads

    //region Helpers

    private Asset CreateAssetWrapper(AssetType type)
    {
        Asset asset;

        switch (type)
        {
            case Notecard:
                asset = new AssetNotecard();
                break;
            case LSLText:
                asset = new AssetScriptText();
                break;
            case LSLBytecode:
                asset = new AssetScriptBinary();
                break;
            case Texture:
                asset = new AssetTexture();
                break;
            case Object:
                asset = new AssetPrim();
                break;
            case Clothing:
                asset = new AssetClothing();
                break;
            case Bodypart:
                asset = new AssetBodypart();
                break;
            case Animation:
                asset = new AssetAnimation();
                break;
            case Sound:
                asset = new AssetSound();
                break;
            case Landmark:
                asset = new AssetLandmark();
                break;
            case Gesture:
                asset = new AssetGesture();
                break;
            default:
                JLogger.error("Unimplemented asset type: " + type);
                return null;
        }

        return asset;
    }

    private Asset WrapAsset(AssetDownload download)
    {
        Asset asset = CreateAssetWrapper(download.AssetType);
        if (asset != null)
        {
            asset.setAssetID(download.AssetID);
            asset.AssetData = download.AssetData;
            return asset;
        }
        else
        {
            return null;
        }
    }

    private void SendNextUploadPacket(AssetUpload upload)
    {
        SendXferPacketPacket send = new SendXferPacketPacket();

        send.XferID.ID = upload.XferID;
        send.XferID.Packet = upload.PacketNum++;

        if (send.XferID.Packet == 0)
        {
            // The first packet reserves the first four bytes of the data for the
            // total length of the asset and appends 1000 bytes of data after that
            send.DataPacket.Data = new byte[1004];
            Utils.arraycopy(Utils.intToBytes(upload.Size), 0, send.DataPacket.Data, 0, 4);
            Utils.arraycopy(upload.AssetData, 0, send.DataPacket.Data, 4, 1000);
            upload.Transferred += 1000;

            synchronized(Transfers)
            {
                Transfers.remove(upload.AssetID);
                Transfers.put(upload.ID, upload);
            }
        }
        else if ((send.XferID.Packet + 1) * 1000 < upload.Size)
        {
            // This packet is somewhere in the middle of the transfer, or a perfectly
            // aligned packet at the end of the transfer
            send.DataPacket.Data = new byte[1000];
            Utils.arraycopy(upload.AssetData, (int)upload.Transferred, send.DataPacket.Data, 0, 1000);
            upload.Transferred += 1000;
        }
        else
        {
            // Special handler for the last packet which will be less than 1000 bytes
            int lastlen = upload.Size - ((int)send.XferID.Packet * 1000);
            send.DataPacket.Data = new byte[lastlen];
            Utils.arraycopy(upload.AssetData, (int)send.XferID.Packet * 1000, send.DataPacket.Data, 0, lastlen);
            send.XferID.Packet |= 0x80000000L; // This signals the final packet
            upload.Transferred += lastlen;
        }

        Client.network.SendPacket(send);
    }

    private void SendConfirmXferPacket(BigInteger xferID, long packetNum)
    {
        ConfirmXferPacketPacket confirm = new ConfirmXferPacketPacket();
        confirm.XferID.ID = xferID;
        confirm.XferID.Packet = packetNum;

        Client.network.SendPacket(confirm);
    }

    //endregion Helpers

    //region Transfer Callbacks

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void TransferInfoHandler(Object sender, PacketReceivedEventArgs e)
    {
        TransferInfoPacket info = (TransferInfoPacket)e.getPacket();
        Transfer transfer;
        AssetDownload download;

        if ((transfer = Transfers.get(info.TransferInfo.TransferID))!=null)
        {
            download = (AssetDownload)transfer;

            if (download.Callback == null) return;

            download.Channel = ChannelType.get(info.TransferInfo.ChannelType);
            download.Status = StatusCode.get(info.TransferInfo.Status);
            download.Target = TargetType.get(info.TransferInfo.TargetType);
            download.Size = info.TransferInfo.Size;

            // TODO: Once we support mid-transfer status checking and aborting this
            // will need to become smarter
            if (download.Status != StatusCode.OK)
            {
                JLogger.warn("Transfer failed with status code " + download.Status);

                synchronized(Transfers) 
                {Transfers.remove(download.ID);}

                // No data could have been received before the TransferInfo packet
                download.AssetData = null;

                // Fire the event with our transfer that contains Success = false;
                try { download.Callback.execute(new AssetReceivedCallbackArgs(download, null)); }
                catch (Exception ex) { JLogger.error(ex); }
            }
            else
            {
                download.AssetData = new byte[download.Size];

                if (download.Source == SourceType.Asset && info.TransferInfo.Params.length == 20)
                {
                    download.AssetID = new UUID(info.TransferInfo.Params, 0);
                    download.AssetType = AssetType.get((byte)info.TransferInfo.Params[16]);

                    //Client.DebugLog(String.Format("TransferInfo packet received. AssetID: {0} Type: {1}",
                    //    transfer.AssetID, type));
                }
                else if (download.Source == SourceType.SimInventoryItem && info.TransferInfo.Params.length == 100)
                {
                    // TODO: Can we use these?
                    //UUID agentID = new UUID(info.TransferInfo.Params, 0);
                    //UUID sessionID = new UUID(info.TransferInfo.Params, 16);
                    //UUID ownerID = new UUID(info.TransferInfo.Params, 32);
                    //UUID taskID = new UUID(info.TransferInfo.Params, 48);
                    //UUID itemID = new UUID(info.TransferInfo.Params, 64);
                    download.AssetID = new UUID(info.TransferInfo.Params, 80);
                    download.AssetType = AssetType.get((byte)info.TransferInfo.Params[96]);

                    //Client.DebugLog(String.Format("TransferInfo packet received. AgentID: {0} SessionID: {1} " + 
                    //    "OwnerID: {2} TaskID: {3} ItemID: {4} AssetID: {5} Type: {6}", agentID, sessionID, 
                    //    ownerID, taskID, itemID, transfer.AssetID, type));
                }
                else
                {
                    JLogger.warn("Received a TransferInfo packet with a SourceType of " + download.Source.toString() +
                        " and a Params field length of " + info.TransferInfo.Params.length);
                }
            }
            download.HeaderReceivedEvent.set();
        }
        else
        {
            JLogger.warn("Received a TransferInfo packet for an asset we didn't request, TransferID: " +
                info.TransferInfo.TransferID);
        }
    }

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void TransferPacketHandler(Object sender, PacketReceivedEventArgs e) throws InterruptedException
    {
        TransferPacketPacket asset = (TransferPacketPacket)e.getPacket();
        Transfer transfer;
        AssetDownload download;

        if ((transfer = Transfers.get(asset.TransferData.TransferID))!=null)
        {
            download = (AssetDownload)transfer;

            if (download.Size == 0)
            {
                JLogger.debug("TransferPacket received ahead of the transfer header, blocking...");

                // We haven't received the header yet, block until it's received or times out
                download.HeaderReceivedEvent.waitOne(TRANSFER_HEADER_TIMEOUT);

                if (download.Size == 0)
                {
                    JLogger.warn("Timed out while waiting for the asset header to download for " +
                        download.ID.toString());

                    // Abort the transfer
                    TransferAbortPacket abort = new TransferAbortPacket();
                    abort.TransferInfo.ChannelType = (int)download.Channel.getIndex();
                    abort.TransferInfo.TransferID = download.ID;
                    Client.network.SendPacket(abort, download.Simulator);

                    download.Success = false;
                    synchronized(Transfers) 
                    {Transfers.remove(download.ID);}

                    // Fire the event with our transfer that contains Success = false
                    if (download.Callback != null)
                    {
                        try { download.Callback.execute(new AssetReceivedCallbackArgs(download, null)); }
                        catch (Exception ex) { JLogger.error(ex); }
                    }

                    return;
                }
            }

            // This assumes that every transfer packet except the last one is exactly 1000 bytes,
            // hopefully that is a safe assumption to make
            try
            {
            	byte[] bytes = new byte[asset.TransferData.Data.length];
                Utils.arraycopy(asset.TransferData.Data, 0, bytes, 0, asset.TransferData.Data.length);
                download.pmap.put(asset.TransferData.Packet, bytes);
                download.Transferred += asset.TransferData.Data.length;
                JLogger.debug(String.format("AssetManager: TransferPacketHandler: got asset %s packet length %d of packet no %d", download.AssetID.toString(), asset.TransferData.Data.length, asset.TransferData.Packet));
            }
            catch (IllegalArgumentException ex)
            {
                JLogger.error(String.format("TransferPacket handling failed. TransferData.Data.Length=%d, AssetData.Length=%d, TransferData.Packet=%d",
                    asset.TransferData.Data.length, download.AssetData.length, asset.TransferData.Packet));
                return;
            }

            //Client.DebugLog(String.Format("Transfer packet {0}, received {1}/{2}/{3} bytes for asset {4}",
            //    asset.TransferData.Packet, asset.TransferData.Data.Length, transfer.Transferred, transfer.Size,
            //    transfer.AssetID.toString()));

            // Check if we downloaded the full asset
            if (download.Transferred >= download.Size)
            {
            	int i =0;
            	
            	for(Entry<Integer, byte[]> entry:download.pmap.entrySet())
            	{
            		Utils.arraycopy(entry.getValue(), 0, download.AssetData, i, entry.getValue().length);
            		i += entry.getValue().length;
            	}
            	download.pmap.clear();
                JLogger.debug("Transfer for asset " + download.AssetID.toString() + " completed");

                download.Success = true;
                synchronized(Transfers) 
                {Transfers.remove(download.ID);}

                // Cache successful asset download
                Cache.saveAssetToCache(download.AssetID, download.AssetData);

                if (download.Callback != null)
                {
                    try { download.Callback.execute(new AssetReceivedCallbackArgs(download, WrapAsset(download))); }
                    catch (Exception ex) { JLogger.error(ex); }
                }
            }
        }
    }

    //endregion Transfer Callbacks

    //region Xfer Callbacks

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void InitiateDownloadPacketHandler(Object sender, PacketReceivedEventArgs e)
    {
        InitiateDownloadPacket request = (InitiateDownloadPacket)e.getPacket();
        try
        {
            onInitiateDownload.raiseEvent(new InitiateDownloadEventArgs(Utils.bytesToString(request.FileData.SimFilename),
                Utils.bytesToString(request.FileData.ViewerFilename)));
        }
        catch (Exception ex) { JLogger.error(ex); }
    }

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void RequestXferHandler(Object sender, PacketReceivedEventArgs e)
    {
        if (PendingUpload == null)
            JLogger.warn("Received a RequestXferPacket for an unknown asset upload");
        else
        {
            AssetUpload upload = PendingUpload;
            PendingUpload = null;
            WaitingForUploadConfirm = false;
            RequestXferPacket request = (RequestXferPacket)e.getPacket();

            upload.XferID = request.XferID.ID;
            upload.Type = AssetType.get((byte)request.XferID.VFileType);

            UUID transferID = new UUID(upload.XferID.longValue());
            Transfers.put(transferID,  upload);

            // Send the first packet containing actual asset data
            SendNextUploadPacket(upload);
        }
    }

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void ConfirmXferPacketHandler(Object sender, PacketReceivedEventArgs e)
    {
        ConfirmXferPacketPacket confirm = (ConfirmXferPacketPacket)e.getPacket();

        // Building a new UUID every time an ACK is received for an upload is a horrible
        // thing, but this whole Xfer system is horrible
        UUID transferID = new UUID(confirm.XferID.ID.longValue());
        Transfer transfer;
        AssetUpload upload = null;

        if ((transfer = Transfers.get(transferID))!=null)
        {
            upload = (AssetUpload)transfer;

            //Client.DebugLog(String.Format("ACK for upload {0} of asset type {1} ({2}/{3})",
            //    upload.AssetID.toString(), upload.Type, upload.Transferred, upload.Size));

            try { onUploadProgress.raiseEvent(new AssetUploadEventArgs(upload)); }
            catch (Exception ex) { JLogger.error(ex); }

            if (upload.Transferred < upload.Size)
                SendNextUploadPacket(upload);
        }
    }

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void AssetUploadCompleteHandler(Object sender, PacketReceivedEventArgs e)
    {
        AssetUploadCompletePacket complete = (AssetUploadCompletePacket)e.getPacket();

        // If we uploaded an asset in a single packet, RequestXferHandler()
        // will never be called so we need to set this here as well
        WaitingForUploadConfirm = false;

        if (onAssetUploaded != null)
        {
            boolean found = false;
            Entry<UUID, Transfer> foundTransfer = null;

            // Xfer system sucks really really bad. Where is the damn XferID?
            synchronized(Transfers)
            {
                for (Entry<UUID, Transfer> transfer : Transfers.entrySet())
                {
                    if (transfer.getValue() instanceof AssetUpload)
                    {
                        AssetUpload upload = (AssetUpload)transfer.getValue();

                        if ((upload).AssetID.equals(complete.AssetBlock.UUID))
                        {
                            found = true;
                            foundTransfer = transfer;
                            upload.Success = complete.AssetBlock.Success;
                            upload.Type = AssetType.get(complete.AssetBlock.Type);
                            break;
                        }
                    }
                }
            }

            if (found)
            {
                synchronized(Transfers) 
                {Transfers.remove(foundTransfer.getKey());}

                try { onAssetUploaded.raiseEvent(new AssetUploadEventArgs((AssetUpload)foundTransfer.getValue())); }
                catch (Exception ex) { JLogger.error(ex); }
            }
            else
            {
                JLogger.warn(String.format(
                    "Got an AssetUploadComplete on an unrecognized asset, AssetID: %s, Type: %s, Success: %s",
                    complete.AssetBlock.UUID, AssetType.get(complete.AssetBlock.Type), complete.AssetBlock.Success));
            }
        }
    }

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void SendXferPacketHandler(Object sender, PacketReceivedEventArgs e)
    {
        SendXferPacketPacket xfer = (SendXferPacketPacket)e.getPacket();

        // Lame ulong to UUID conversion, please go away Xfer system
        UUID transferID = new UUID(xfer.XferID.ID.longValue());
        Transfer transfer;
        XferDownload download = null;

        if ((transfer = Transfers.get(transferID))!=null)
        {
            download = (XferDownload)transfer;

            // Apply a mask to get rid of the "end of transfer" bit
            long packetNum = xfer.XferID.Packet & 0x0FFFFFFF;

            // Check for out of order packets, possibly indicating a resend
            if (packetNum != download.PacketNum)
            {
                if (packetNum == download.PacketNum - 1)
                {
                    JLogger.debug("Resending Xfer download confirmation for packet " + packetNum);
                    SendConfirmXferPacket(download.XferID, packetNum);
                }
                else
                {
                    JLogger.warn("Out of order Xfer packet in a download, got " + packetNum + " expecting " + download.PacketNum);
                    // Re-confirm the last packet we actually received
                    SendConfirmXferPacket(download.XferID, download.PacketNum - 1);
                }

                return;
            }

            if (packetNum == 0)
            {
                // This is the first packet received in the download, the first four bytes are a size integer
                // in little endian ordering
                byte[] bytes = xfer.DataPacket.Data;
                download.Size = (bytes[0] + (bytes[1] << 8) + (bytes[2] << 16) + (bytes[3] << 24));
                download.AssetData = new byte[download.Size];

                JLogger.debug("Received first packet in an Xfer download of size " + download.Size);

                Utils.arraycopy(xfer.DataPacket.Data, 4, download.AssetData, 0, xfer.DataPacket.Data.length - 4);
                download.Transferred += xfer.DataPacket.Data.length - 4;
            }
            else
            {
                Utils.arraycopy(xfer.DataPacket.Data, 0, download.AssetData, 1000 * (int)packetNum, xfer.DataPacket.Data.length);
                download.Transferred += xfer.DataPacket.Data.length;
            }

            // Increment the packet number to the packet we are expecting next
            download.PacketNum++;

            // Confirm receiving this packet
            SendConfirmXferPacket(download.XferID, packetNum);

            if ((xfer.XferID.Packet & 0x80000000) != 0)
            {
                // This is the last packet in the transfer
                if (!Utils.isNullOrEmpty(download.Filename))
                    JLogger.debug("Xfer download for asset " + download.Filename + " completed");
                else
                    JLogger.debug("Xfer download for asset " + download.VFileID.toString() + " completed");

                download.Success = true;
                synchronized(Transfers) 
                {Transfers.remove(download.ID);}

                try { onXferReceived.raiseEvent(new XferReceivedEventArgs(download)); }
                catch (Exception ex) { JLogger.error(ex); }
            }
        }
    }

    /// <summary>Process an incoming packet and raise the appropriate events</summary>
    /// <param name="sender">The sender</param>
    /// <param name="e">The EventArgs object containing the packet data</param>
    protected void AbortXferHandler(Object sender, PacketReceivedEventArgs e)
    {
        AbortXferPacket abort = (AbortXferPacket)e.getPacket();
        XferDownload download = null;

        // Lame ulong to UUID conversion, please go away Xfer system
        UUID transferID = new UUID(abort.XferID.ID.longValue());

        synchronized(Transfers)
        {
            Transfer transfer;
            if ((transfer = Transfers.get(transferID))!=null)
            {
                download = (XferDownload)transfer;
                Transfers.remove(transferID);
            }
        }

        if (download != null && onXferReceived != null)
        {
            download.Success = false;
            download.Error = TransferError.get(abort.XferID.Result);

            try { onXferReceived.raiseEvent((new XferReceivedEventArgs(download))); }
            catch (Exception ex) { JLogger.error(ex); }
        }
    }

    //endregion Xfer Callbacks	
}
