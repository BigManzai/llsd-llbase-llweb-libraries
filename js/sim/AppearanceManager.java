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

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimerTask;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import com.ngt.jopenmetaverse.shared.protocol.AgentCachedTexturePacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentCachedTexturePacket.WearableDataBlock;
import com.ngt.jopenmetaverse.shared.protocol.AgentCachedTextureResponsePacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentIsNowWearingPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentSetAppearancePacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentWearablesRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentWearablesUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.DetachAttachmentIntoInvPacket;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.RebakeAvatarTexturesPacket;
import com.ngt.jopenmetaverse.shared.protocol.RezMultipleAttachmentsFromInvPacket;
import com.ngt.jopenmetaverse.shared.protocol.RezSingleAttachmentFromInvPacket;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntry;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntryFace;
import com.ngt.jopenmetaverse.shared.sim.InventoryManager.InventoryAttachment;
import com.ngt.jopenmetaverse.shared.sim.InventoryManager.InventoryWearable;
import com.ngt.jopenmetaverse.shared.sim.asset.Asset;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetTexture;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetWearable;
import com.ngt.jopenmetaverse.shared.sim.asset.pipeline.TexturePipeline.TextureRequestState;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.EventTimer;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.appearance.AgentCachedBakesReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.appearance.AgentWearablesReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.appearance.AppearanceSetEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.appearance.RebakeAvatarTexturesEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetDownload;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetReceivedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.BakedTextureUploadedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.TextureDownloadCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.DisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.EventQueueRunningEventArgs;
import com.ngt.jopenmetaverse.shared.sim.imaging.Baker;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryBase;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryItem;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryObject;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Enums.WearableType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.AttachmentPoint;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.sim.visual.VisualParams;
import com.ngt.jopenmetaverse.shared.sim.visual.VisualParams.*;;

public class AppearanceManager {
	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();

	//region Enums

	/// <summary>
	/// Index of TextureEntry slots for avatar appearances
	/// </summary>
	public enum AvatarTextureIndex
	{
		Unknown(-1),
		HeadBodypaint(0),
		UpperShirt(1),
		LowerPants(2),
		EyesIris(3),
		Hair(4),
		UpperBodypaint(5),
		LowerBodypaint(6),
		LowerShoes(7),
		HeadBaked(8),
		UpperBaked(9),
		LowerBaked(10),
		EyesBaked(11),
		LowerSocks(12),
		UpperJacket(13),
		LowerJacket(14),
		UpperGloves(15),
		UpperUndershirt(16),
		LowerUnderpants(17),
		Skirt(18),
		SkirtBaked(19),
		HairBaked(20),
		LowerAlpha(21),
		UpperAlpha(22),
		HeadAlpha(23),
		EyesAlpha(24),
		HairAlpha(25),
		HeadTattoo(26),
		UpperTattoo(27),
		LowerTattoo(28),
		NumberOfEntries(29);
		private int index;
		AvatarTextureIndex(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,AvatarTextureIndex> lookup  
		= new HashMap<Integer,AvatarTextureIndex>();

		static {
			for(AvatarTextureIndex s : EnumSet.allOf(AvatarTextureIndex.class))
				lookup.put(s.getIndex(), s);
		}

		public static AvatarTextureIndex get(Integer index)
		{
			return lookup.get(index);

		}
	}

	/// <summary>
	/// Bake layers for avatar appearance
	/// </summary>
	public enum BakeType
	{
		Unknown (-1),
		Head (0),
		UpperBody (1),
		LowerBody (2),
		Eyes (3),
		Skirt (4),
		Hair (5);
		private int index;
		BakeType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,BakeType> lookup  
		= new HashMap<Integer,BakeType>();

		static {
			for(BakeType s : EnumSet.allOf(BakeType.class))
				lookup.put(s.getIndex(), s);
		}

		public static BakeType get(Integer index)
		{
			return lookup.get(index);

		}
	}

	//endregion Enums

	//region Constants
	/// <summary>Mapping between BakeType and AvatarTextureIndex</summary>
	public static  byte[] BakeIndexToTextureIndex = new byte[] {(byte) 8, (byte)9, (byte)10, (byte)11, (byte)19, (byte)20 };
	/// <summary>Maximum number of concurrent downloads for wearable assets and textures</summary>
	final static  int MAX_CONCURRENT_DOWNLOADS = 5;
	/// <summary>Maximum number of concurrent uploads for baked textures</summary>
	final static  int MAX_CONCURRENT_UPLOADS = 6;
	/// <summary>Timeout for fetching inventory listings</summary>
	final static  int INVENTORY_TIMEOUT = 1000 * 30;
	/// <summary>Timeout for fetching a single wearable, or receiving a single packet response</summary>
	final static  int WEARABLE_TIMEOUT = 1000 * 30;
	/// <summary>Timeout for fetching a single texture</summary>
	final static  int TEXTURE_TIMEOUT = 1000 * 120;
	/// <summary>Timeout for uploading a single baked texture</summary>
	final static  int UPLOAD_TIMEOUT = 1000 * 90;
	/// <summary>Number of times to retry bake upload</summary>
	final static  int UPLOAD_RETRIES = 2;
	/// <summary>When changing outfit, kick off rebake after
	/// 20 seconds has passed since the last change</summary>
	final static  int REBAKE_DELAY = 1000 * 20;

	/// <summary>Total number of wearables for each avatar</summary>
	public final static  int WEARABLE_COUNT = 16;
	/// <summary>Total number of baked textures on each avatar</summary>
	public final static  int BAKED_TEXTURE_COUNT = 6;
	/// <summary>Total number of wearables per bake layer</summary>
	public final static  int WEARABLES_PER_LAYER = 9;
	/// <summary>Map of what wearables are included in each bake</summary>
	public static  WearableType[][] WEARABLE_BAKE_MAP = new WearableType[][]
			{
		new WearableType[] { WearableType.Shape, WearableType.Skin,    WearableType.Tattoo,  WearableType.Hair,    WearableType.Alpha,   WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid,      WearableType.Invalid },
		new WearableType[] { WearableType.Shape, WearableType.Skin,    WearableType.Tattoo,  WearableType.Shirt,   WearableType.Jacket,  WearableType.Gloves,  WearableType.Undershirt, WearableType.Alpha,        WearableType.Invalid },
		new WearableType[] { WearableType.Shape, WearableType.Skin,    WearableType.Tattoo,  WearableType.Pants,   WearableType.Shoes,   WearableType.Socks,   WearableType.Jacket,     WearableType.Underpants,   WearableType.Alpha   },
		new WearableType[] { WearableType.Eyes,  WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid,      WearableType.Invalid },
		new WearableType[] { WearableType.Skirt, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid,      WearableType.Invalid },
		new WearableType[] { WearableType.Hair,  WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid, WearableType.Invalid,    WearableType.Invalid,      WearableType.Invalid }
			};
	/// <summary>Magic values to finalize the cache check hashes for each
	/// bake</summary>
	public static  UUID[] BAKED_TEXTURE_HASH = new UUID[]
			{
		new UUID("18ded8d6-bcfc-e415-8539-944c0f5ea7a6"),
		new UUID("338c29e3-3024-4dbb-998d-7c04cf4fa88f"),
		new UUID("91b4a2c7-1b1a-ba16-9a16-1f8f8dcc1c3f"),
		new UUID("b2cf28af-b840-1071-3c6a-78085d8128b5"),
		new UUID("ea800387-ea1a-14e0-56cb-24f2022f969a"),
		new UUID("0af1ef7c-ad24-11dd-8790-001f5bf833e8")
			};
	/// <summary>Default avatar texture, used to detect when a custom
	/// texture is not set for a face</summary>
	public static  UUID DEFAULT_AVATAR_TEXTURE = new UUID("c228d1cf-4b5d-4ba8-84f4-899a0796aa97");

	//endregion Constants

	//region Structs / Classes

	/// <summary>
	/// Contains information about a wearable inventory item
	/// </summary>
	public static class WearableData
	{
		/// <summary>Inventory ItemID of the wearable</summary>
		public UUID ItemID;
		/// <summary>AssetID of the wearable asset</summary>
		public UUID AssetID;
		/// <summary>WearableType of the wearable</summary>
		public Enums.WearableType WearableType;
		/// <summary>AssetType of the wearable</summary>
		public Enums.AssetType AssetType;
		/// <summary>Asset data for the wearable</summary>
		public AssetWearable Asset;

		@Override
		public String toString()
		{
			return String.format("ItemID: %s, AssetID: %s, WearableType: %s, AssetType: %s, Asset: %s",
					ItemID, AssetID, WearableType, AssetType, Asset != null ? Asset.Name : "(null)");
		}
	}

	/// <summary>
	/// Data collected from visual params for each wearable
	/// needed for the calculation of the color
	/// </summary>
	private class ColorParamInfo
	{
		public VisualParam VisualParam;
		public VisualColorParam VisualColorParam;
		public float Value;
		public Enums.WearableType WearableType;
	}

	/// <summary>
	/// Holds a texture assetID and the data needed to bake this layer into
	/// an outfit texture. Used to keep track of currently worn textures
	/// and baking data
	/// </summary>
	public class TextureData
	{
		/// <summary>A texture AssetID</summary>
		public UUID TextureID = new UUID();
		/// <summary>Asset data for the texture</summary>
		public AssetTexture Texture = null;
		/// <summary>Collection of alpha masks that needs applying</summary>
		public Map<VisualAlphaParam, Float> AlphaMasks = new HashMap<VisualAlphaParam, Float>();
		/// <summary>Tint that should be applied to the texture</summary>
		public Color4 Color = new Color4();
		/// <summary>Where on avatar does this texture belong</summary>
		public AvatarTextureIndex TextureIndex = null;

		@Override
		public String toString()
		{
			return String.format("TextureID: %s, Texture: %s",
					TextureID, Texture != null ? Texture.AssetData.length + " bytes" : "(null)");
		}
	}

	//endregion Structs / Classes

	private EventObservable<AgentWearablesReplyEventArgs> onAgentWearablesReply = new EventObservable<AgentWearablesReplyEventArgs>();
	public void registerOnAgentWearablesReply(EventObserver<AgentWearablesReplyEventArgs> o)
	{
		onAgentWearablesReply.addObserver(o);
	}
	public void unregisterOnAgentWearablesReply(EventObserver<AgentWearablesReplyEventArgs> o) 
	{
		onAgentWearablesReply.deleteObserver(o);
	}

	private EventObservable<AgentCachedBakesReplyEventArgs> onCachedBakesReply = new EventObservable<AgentCachedBakesReplyEventArgs>();
	public void registerOnCachedBakesReply(EventObserver<AgentCachedBakesReplyEventArgs> o)
	{
		onCachedBakesReply.addObserver(o);
	}
	public void unregisterOnCachedBakesReply(EventObserver<AgentCachedBakesReplyEventArgs> o) 
	{
		onCachedBakesReply.deleteObserver(o);
	}

	private EventObservable<AppearanceSetEventArgs> onAppearanceSet = new EventObservable<AppearanceSetEventArgs>();
	public void registerOnAppearanceSet(EventObserver<AppearanceSetEventArgs> o)
	{
		onAppearanceSet.addObserver(o);
	}
	public void unregisterOnAppearanceSet(EventObserver<AppearanceSetEventArgs> o) 
	{
		onAppearanceSet.deleteObserver(o);
	}

	private EventObservable<RebakeAvatarTexturesEventArgs> onRebakeAvatarRequested = new EventObservable<RebakeAvatarTexturesEventArgs>();
	public void registerOnRebakeAvatarRequested(EventObserver<RebakeAvatarTexturesEventArgs> o)
	{
		onRebakeAvatarRequested.addObserver(o);
	}
	public void unregisterOnRebakeAvatarRequested(EventObserver<RebakeAvatarTexturesEventArgs> o) 
	{
		onRebakeAvatarRequested.deleteObserver(o);
	}	      

	//region Event delegates, Raise Events

	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AgentWearablesReplyEventArgs> m_AgentWearablesReply;
	//
	//	        /// <summary>Raises the AgentWearablesReply event</summary>
	//	        /// <param name="e">An AgentWearablesReplyEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnAgentWearables(AgentWearablesReplyEventArgs e)
	//	        {
	//	            EventHandler<AgentWearablesReplyEventArgs> handler = m_AgentWearablesReply;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private  object m_AgentWearablesLock = new object();
	//
	//	        /// <summary>Triggered when an AgentWearablesUpdate packet is received,
	//	        /// telling us what our avatar is currently wearing
	//	        /// <see cref="RequestAgentWearables"/> request.</summary>
	//	        public event EventHandler<AgentWearablesReplyEventArgs> AgentWearablesReply  
	//	        {
	//	            add { lock (m_AgentWearablesLock) { m_AgentWearablesReply += value; } }
	//	            remove { lock (m_AgentWearablesLock) { m_AgentWearablesReply -= value; } }
	//	        }
	//
	//
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AgentCachedBakesReplyEventArgs> m_AgentCachedBakesReply;
	//
	//	        /// <summary>Raises the CachedBakesReply event</summary>
	//	        /// <param name="e">An AgentCachedBakesReplyEventArgs object containing the
	//	        /// data returned from the data server AgentCachedTextureResponse</param>
	//	        protected virtual void OnAgentCachedBakes(AgentCachedBakesReplyEventArgs e)
	//	        {
	//	            EventHandler<AgentCachedBakesReplyEventArgs> handler = m_AgentCachedBakesReply;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private  object m_AgentCachedBakesLock = new object();
	//
	//	        /// <summary>Raised when an AgentCachedTextureResponse packet is
	//	        /// received, giving a list of cached bakes that were found on the
	//	        /// simulator
	//	        /// <seealso cref="RequestCachedBakes"/> request.</summary>
	//	        public event EventHandler<AgentCachedBakesReplyEventArgs> CachedBakesReply   
	//	        {
	//	            add { lock (m_AgentCachedBakesLock) { m_AgentCachedBakesReply += value; } }
	//	            remove { lock (m_AgentCachedBakesLock) { m_AgentCachedBakesReply -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AppearanceSetEventArgs> m_AppearanceSet;
	//
	//	        /// <summary>Raises the AppearanceSet event</summary>
	//	        /// <param name="e">An AppearanceSetEventArgs object indicating if the operatin was successfull</param>
	//	        protected virtual void OnAppearanceSet(AppearanceSetEventArgs e)
	//	        {
	//	            EventHandler<AppearanceSetEventArgs> handler = m_AppearanceSet;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private  object m_AppearanceSetLock = new object();
	//
	//	        /// <summary>
	//	        /// Raised when appearance data is sent to the simulator, also indicates
	//	        /// the main appearance thread is finished.
	//	        /// </summary>
	//	        /// <seealso cref="RequestAgentSetAppearance"/> request.
	//	        public event EventHandler<AppearanceSetEventArgs> AppearanceSet   
	//	        {
	//	            add { lock (m_AppearanceSetLock) { m_AppearanceSet += value; } }
	//	            remove { lock (m_AppearanceSetLock) { m_AppearanceSet -= value; } }
	//	        }
	//
	//
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<RebakeAvatarTexturesEventArgs> m_RebakeAvatarReply;
	//
	//	        /// <summary>Raises the RebakeAvatarRequested event</summary>
	//	        /// <param name="e">An RebakeAvatarTexturesEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnRebakeAvatar(RebakeAvatarTexturesEventArgs e)
	//	        {
	//	            EventHandler<RebakeAvatarTexturesEventArgs> handler = m_RebakeAvatarReply;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private  object m_RebakeAvatarLock = new object();
	//
	//	        /// <summary>
	//	        /// Triggered when the simulator requests the agent rebake its appearance.
	//	        /// </summary>
	//	        /// <seealso cref="RebakeAvatarRequest"/>
	//	        public event EventHandler<RebakeAvatarTexturesEventArgs> RebakeAvatarRequested  
	//	        {
	//	            add { lock (m_RebakeAvatarLock) { m_RebakeAvatarReply += value; } }
	//	            remove { lock (m_RebakeAvatarLock) { m_RebakeAvatarReply -= value; } }
	//	        }
	//
	//	        //endregion
	//
	//region Properties and public fields

	/// <summary>
	/// Returns true if AppearanceManager is busy and trying to set or change appearance will fail
	/// </summary>
	public boolean getManagerBusy()
	{
		return AppearanceThreadRunning.get() != 0;
	}

	/// <summary>Visual parameters last sent to the sim</summary>
	public byte[] MyVisualParameters = null;

	/// <summary>Textures about this client sent to the sim</summary>
	public TextureEntry MyTextures = null;

	//endregion Properties

	//region Private Members

	/// <summary>A cache of wearables currently being worn</summary>
	private Map<WearableType, WearableData> Wearables = new HashMap<WearableType, WearableData>();
	/// <summary>A cache of textures currently being worn</summary>
	private TextureData[] Textures = new TextureData[(int)AvatarTextureIndex.NumberOfEntries.getIndex()];
	/// <summary>Incrementing serial number for AgentCachedTexture packets</summary>
	private AtomicInteger CacheCheckSerialNum = new AtomicInteger(-1);
	/// <summary>Incrementing serial number for AgentSetAppearance packets</summary>
	private AtomicInteger SetAppearanceSerialNum = new AtomicInteger(0);
	/// <summary>Indicates whether or not the appearance thread is currently
	/// running, to prevent multiple appearance threads from running
	/// simultaneously</summary>
	private AtomicInteger AppearanceThreadRunning = new AtomicInteger(0);
	/// <summary>Reference to our agent</summary>
	private GridClient Client;
	/// <summary>
	/// Timer used for delaying rebake on changing outfit
	/// </summary>
	private EventTimer RebakeScheduleTimer = null;
	/// <summary>
	/// Main appearance thread
	/// </summary>
	//	private Thread AppearanceThread;
	//endregion Private Members

	/// <summary>
	/// Default constructor
	/// </summary>
	/// <param name="client">A reference to our agent</param>
	public AppearanceManager(GridClient client)
	{
		Client = client;

		for(int i=0; i< Textures.length; i++)
			Textures[i] = new TextureData();

		// Client.network.RegisterCallback(PacketType.AgentWearablesUpdate, AgentWearablesUpdateHandler);
		Client.network.RegisterCallback(PacketType.AgentWearablesUpdate, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ AgentWearablesUpdateHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		// Client.network.RegisterCallback(PacketType.AgentCachedTextureResponse, AgentCachedTextureResponseHandler);
		Client.network.RegisterCallback(PacketType.AgentCachedTextureResponse, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ AgentCachedTextureResponseHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		// Client.network.RegisterCallback(PacketType.RebakeAvatarTextures, RebakeAvatarTexturesHandler);
		Client.network.RegisterCallback(PacketType.RebakeAvatarTextures, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ RebakeAvatarTexturesHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		//				            Client.network.EventQueueRunning += Network_OnEventQueueRunning;
		EventObserver<EventQueueRunningEventArgs> queueCallback = new EventObserver<EventQueueRunningEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o, EventQueueRunningEventArgs e) 
			{
				Network_OnEventQueueRunning(o, e);
			}
				};
				Client.network.RegisterOnEventQueueRunningCallback(queueCallback);

				//				            Client.network.Disconnected += Network_OnDisconnected;
				Client.network.RegisterOnDisconnectedCallback(new EventObserver<DisconnectedEventArgs>()
						{
					@Override
					public void handleEvent(Observable o, DisconnectedEventArgs arg) 
					{
						Network_OnDisconnected(o, arg);
					}
						});
	}

	//region Publics Methods

	/// <summary>
	/// Starts the appearance setting thread
	/// </summary>
	public void RequestSetAppearance()
	{
		RequestSetAppearance(false);
	}

	/// <summary>
	/// Starts the appearance setting thread
	/// </summary>
	/// <param name="forceRebake">True to force rebaking, otherwise false</param>
	public void RequestSetAppearance(final boolean forceRebake)
	{
		if(!AppearanceThreadRunning.compareAndSet(0, 1))
		{
			JLogger.warn("Appearance thread is already running, skipping");
			return;
		}

		JLogger.info("RequestSetAppearance: updaing appearance");

		// If we have an active delayed scheduled appearance bake, we dispose of it
		if (RebakeScheduleTimer != null)
		{
			RebakeScheduleTimer.cancel();
			RebakeScheduleTimer = null;
		}

		threadPool.execute(new Runnable(){
			public void run()
			{
				boolean success = true;
				try
				{
					if (forceRebake)
					{
						// Set all of the baked textures to UUID.Zero to force rebaking
						for (int bakedIndex = 0; bakedIndex < BAKED_TEXTURE_COUNT; bakedIndex++)
							Textures[(int)BakeTypeToAgentTextureIndex(BakeType.get(bakedIndex)).getIndex()].TextureID = UUID.Zero;
					}

					if (SetAppearanceSerialNum.get() == 0)
					{
						// Fetch a list of the current agent wearables
						if (!GetAgentWearables())
						{
							JLogger.error("Failed to retrieve a list of current agent wearables, appearance cannot be set");
							throw new Exception("Failed to retrieve a list of current agent wearables, appearance cannot be set");
						}
					}

					JLogger.debug("Got Agent Wearable .. going to download wearables");


					// Download and parse all of the agent wearables
					if (!DownloadWearables())
					{
						success = false;
						JLogger.error("One or more agent wearables failed to download, appearance will be incomplete");
					}

					// If this is the first time setting appearance and we're not forcing rebakes, check the server
					// for cached bakes
					if (SetAppearanceSerialNum.get() == 0 && !forceRebake)
					{
						JLogger.debug("Going to GetCachedBakes...");
						// Compute hashes for each bake layer and compare against what the simulator currently has
						if (!GetCachedBakes())
						{
							JLogger.warn("Failed to get a list of cached bakes from the simulator, appearance will be rebaked");
						}
					}

					JLogger.debug("Going to create Bakes....");

					// Download textures, compute bakes, and upload for any cache misses
					if (!CreateBakes())
					{
						success = false;
						JLogger.warn("Failed to create or upload one or more bakes, appearance will be incomplete");
					}

					// Send the appearance packet
					RequestAgentSetAppearance();
				}
				catch (Exception e)
				{
					JLogger.warn(e);
					success = false;
				}
				finally
				{
					AppearanceThreadRunning.set(0);

					onAppearanceSet.raiseEvent(new AppearanceSetEventArgs(success));
				}
			}								
		});
	}

	/// <summary>
	/// Ask the server what textures our agent is currently wearing
	/// </summary>
	public void RequestAgentWearables()
	{
		AgentWearablesRequestPacket request = new AgentWearablesRequestPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();

		Client.network.SendPacket(request);
	}

	/// <summary>
	/// Build hashes out of the texture assetIDs for each baking layer to
	/// ask the simulator whether it has cached copies of each baked texture
	/// </summary>
	public void RequestCachedBakes()
	{
		List<AgentCachedTexturePacket.WearableDataBlock> hashes = new ArrayList<AgentCachedTexturePacket.WearableDataBlock>();

		// Build hashes for each of the bake layers from the individual components
		synchronized (Wearables)
		{
			for (int bakedIndex = 0; bakedIndex < BAKED_TEXTURE_COUNT; bakedIndex++)
			{
				// Don't do a cache request for a skirt bake if we're not wearing a skirt
				if ((bakedIndex == (int)BakeType.Skirt.getIndex()) 
						&& !Wearables.containsKey(WearableType.Skirt))
					continue;

				// Build a hash of all the texture asset IDs in this baking layer
				UUID hash = UUID.Zero;
				for (int wearableIndex = 0; wearableIndex < WEARABLES_PER_LAYER; wearableIndex++)
				{
					WearableType type = WEARABLE_BAKE_MAP[bakedIndex][wearableIndex];

					WearableData wearable;
					if (type != WearableType.Invalid 
							&& (wearable = Wearables.get(type))!=null)
						hash = UUID.xor(hash, wearable.AssetID);
				}

				if (!hash.equals(UUID.Zero))
				{
					// Hash with our secret value for this baked layer
					hash = UUID.xor(hash,  BAKED_TEXTURE_HASH[bakedIndex]);

					// Add this to the list of hashes to send out
					AgentCachedTexturePacket.WearableDataBlock block = new AgentCachedTexturePacket.WearableDataBlock();
					block.ID = hash;
					block.TextureIndex = (byte)bakedIndex;
					hashes.add(block);

					JLogger.debug("Checking cache for " + BakeType.get((int)block.TextureIndex) + ", hash=" + block.ID);
				}
			}
		}

		// Only send the packet out if there's something to check
		if (hashes.size() > 0)
		{
			AgentCachedTexturePacket cache = new AgentCachedTexturePacket();
			cache.AgentData.AgentID = Client.self.getAgentID();
			cache.AgentData.SessionID = Client.self.getSessionID();
			cache.AgentData.SerialNum = CacheCheckSerialNum.incrementAndGet();

			cache.WearableData = hashes.toArray(new WearableDataBlock[0]);

			Client.network.SendPacket(cache);
		}
	}

	/// <summary>
	/// Returns the AssetID of the asset that is currently being worn in a 
	/// given WearableType slot
	/// </summary>
	/// <param name="type">WearableType slot to get the AssetID for</param>
	/// <returns>The UUID of the asset being worn in the given slot, or
	/// UUID.Zero if no wearable is attached to the given slot or wearables
	/// have not been downloaded yet</returns>
	public UUID GetWearableAsset(WearableType type)
	{
		WearableData wearable;

		if ((wearable = Wearables.get(type))!=null)
			return wearable.AssetID;
		else
			return UUID.Zero;
	}

	/// <summary>
	/// Add a wearable to the current outfit and set appearance
	/// </summary>
	/// <param name="wearableItem">Wearable to be added to the outfit</param>
	public void AddToOutfit(InventoryItem wearableItem)
	{
		List<InventoryItem> wearableItems = new ArrayList<InventoryItem>();
		wearableItems.add(wearableItem);
		AddToOutfit(wearableItems);
	}

	/// <summary>
	/// Add a list of wearables to the current outfit and set appearance
	/// </summary>
	/// <param name="wearableItems">List of wearable inventory items to
	/// be added to the outfit</param>
	public void AddToOutfit(List<InventoryItem> wearableItems)
	{
		List<InventoryWearable> wearables = new ArrayList<InventoryWearable>();
		List<InventoryItem> attachments = new ArrayList<InventoryItem>();

		for (int i = 0; i < wearableItems.size(); i++)
		{
			InventoryItem item = wearableItems.get(i);
			if (item instanceof InventoryWearable)
				wearables.add((InventoryWearable)item);
			else if ((item instanceof InventoryAttachment) || (item instanceof InventoryObject))
				attachments.add(item);
		}

		synchronized (Wearables)
		{
			// Add the given wearables to the wearables collection
			for (int i = 0; i < wearables.size(); i++)
			{
				InventoryWearable wearableItem = wearables.get(i);

				WearableData wd = new WearableData();
				wd.AssetID = wearableItem.AssetUUID;
				wd.AssetType = wearableItem.AssetType;
				wd.ItemID = wearableItem.UUID;
				wd.WearableType = wearableItem.getWearableType();

				Wearables.put(wearableItem.getWearableType(), wd);
			}
		}

		if (attachments.size() > 0)
		{
			AddAttachments(attachments, false);
		}

		if (wearables.size() > 0)
		{
			SendAgentIsNowWearing();
			DelayedRequestSetAppearance();
		}
	}

	/// <summary>
	/// Remove a wearable from the current outfit and set appearance
	/// </summary>
	/// <param name="wearableItem">Wearable to be removed from the outfit</param>
	public void RemoveFromOutfit(InventoryItem wearableItem)
	{
		List<InventoryItem> wearableItems = new ArrayList<InventoryItem>();
		wearableItems.add(wearableItem);
		RemoveFromOutfit(wearableItems);
	}


	/// <summary>
	/// Removes a list of wearables from the current outfit and set appearance
	/// </summary>
	/// <param name="wearableItems">List of wearable inventory items to
	/// be removed from the outfit</param>
	public void RemoveFromOutfit(List<InventoryItem> wearableItems)
	{
		List<InventoryWearable> wearables = new ArrayList<InventoryWearable>();
		List<InventoryItem> attachments = new ArrayList<InventoryItem>();

		for (int i = 0; i < wearableItems.size(); i++)
		{
			InventoryItem item = wearableItems.get(i);

			if (item instanceof InventoryWearable)
				wearables.add((InventoryWearable)item);
			else if (item instanceof InventoryAttachment || item instanceof InventoryObject)
				attachments.add(item);
		}

		boolean needSetAppearance = false;
		synchronized (Wearables)
		{
			// Remove the given wearables from the wearables collection
			for (int i = 0; i < wearables.size(); i++)
			{
				InventoryWearable wearableItem = wearables.get(i);
				if (wearables.get(i).AssetType != AssetType.Bodypart        // Remove if it's not a body part
						&& Wearables.containsKey(wearableItem.getWearableType()) // And we have that wearabe type
						&& Wearables.get(wearableItem.getWearableType()).ItemID.equals(wearableItem.UUID) // And we are wearing it
						)
				{
					Wearables.remove(wearableItem.getWearableType());
					needSetAppearance = true;
				}
			}
		}

		for (int i = 0; i < attachments.size(); i++)
		{
			Detach(attachments.get(i).UUID);
		}

		if (needSetAppearance)
		{
			SendAgentIsNowWearing();
			DelayedRequestSetAppearance();
		}
	}

	/// <summary>
	/// Replace the current outfit with a list of wearables and set appearance
	/// </summary>
	/// <param name="wearableItems">List of wearable inventory items that
	/// define a new outfit</param>
	public void ReplaceOutfit(List<InventoryItem> wearableItems) throws InterruptedException
	{
		ReplaceOutfit(wearableItems, true);
	}

	/// <summary>
	/// Replace the current outfit with a list of wearables and set appearance
	/// </summary>
	/// <param name="wearableItems">List of wearable inventory items that
	/// define a new outfit</param>
	/// <param name="safe">Check if we have all body parts, set this to false only
	/// if you know what you're doing</param>
	public void ReplaceOutfit(List<InventoryItem> wearableItems, boolean safe) throws InterruptedException
	{
		List<InventoryWearable> wearables = new ArrayList<InventoryWearable>();
		List<InventoryItem> attachments = new ArrayList<InventoryItem>();

		for (int i = 0; i < wearableItems.size(); i++)
		{
			InventoryItem item = wearableItems.get(i);

			if (item instanceof InventoryWearable)
				wearables.add((InventoryWearable)item);
			else if (item instanceof InventoryAttachment || item instanceof InventoryObject)
				attachments.add(item);
		}

		if (safe)
		{
			// If we don't already have a the current agent wearables downloaded, updating to a
			// new set of wearables that doesn't have all of the bodyparts can leave the avatar
			// in an inconsistent state. If any bodypart entries are empty, we need to fetch the
			// current wearables first
			boolean needsCurrentWearables = false;
			synchronized (Wearables)
			{
				for (int i = 0; i < WEARABLE_COUNT; i++)
				{
					WearableType wearableType = WearableType.get((byte)i);
					if (WearableTypeToAssetType(wearableType) == AssetType.Bodypart && !Wearables.containsKey(wearableType))
					{
						needsCurrentWearables = true;
						break;
					}
				}
			}

			if (needsCurrentWearables && !GetAgentWearables())
			{
				JLogger.error("Failed to fetch the current agent wearables, cannot safely replace outfit");
				return;
			}
		}

		// Replace our local Wearables collection, send the packet(s) to update our
		// attachments, tell sim what we are wearing now, and start the baking process
		if (!safe)
		{
			SetAppearanceSerialNum.incrementAndGet();
		}
		ReplaceOutfit2(wearables);
		AddAttachments(attachments, true);
		SendAgentIsNowWearing();
		DelayedRequestSetAppearance();
	}

	/// <summary>
	/// Checks if an inventory item is currently being worn
	/// </summary>
	/// <param name="item">The inventory item to check against the agent
	/// wearables</param>
	/// <returns>The WearableType slot that the item is being worn in,
	/// or WearbleType.Invalid if it is not currently being worn</returns>
	public WearableType IsItemWorn(InventoryItem item)
	{
		synchronized (Wearables)
		{
			for (Entry<WearableType, WearableData> entry : Wearables.entrySet())
			{
				if (entry.getValue().ItemID.equals(item.UUID))
					return entry.getKey();
			}
		}

		return WearableType.Invalid;
	}

	/// <summary>
	/// Returns a copy of the agents currently worn wearables
	/// </summary>
	/// <returns>A copy of the agents currently worn wearables</returns>
	/// <remarks>Avoid calling this function multiple times as it will make
	/// a copy of all of the wearable data each time</remarks>
	public Map<WearableType, WearableData> GetWearables()
	{
		synchronized (Wearables)
		{
			return new HashMap<WearableType, WearableData>(Wearables);
		}
	}

	/// <summary>
	/// Calls either <seealso cref="ReplaceOutfit"/> or
	/// <seealso cref="AddToOutfit"/> depending on the value of
	/// replaceItems
	/// </summary>
	/// <param name="wearables">List of wearable inventory items to add
	/// to the outfit or become a new outfit</param>
	/// <param name="replaceItems">True to replace existing items with the
	/// new list of items, false to add these items to the existing outfit</param>
	public void WearOutfit(List<InventoryBase> wearables, boolean replaceItems) throws InterruptedException
	{
		List<InventoryItem> wearableItems = new ArrayList<InventoryItem>(wearables.size());
		for (int i = 0; i < wearables.size(); i++)
		{
			if (wearables.get(i) instanceof InventoryItem)
				wearableItems.add((InventoryItem)wearables.get(i));
		}

		if (replaceItems)
			ReplaceOutfit(wearableItems);
		else
			AddToOutfit(wearableItems);
	}

	//endregion Publics Methods

	//region Attachments

	/// <summary>
	/// Adds a list of attachments to our agent
	/// </summary>
	/// <param name="attachments">A List containing the attachments to add</param>
	/// <param name="removeExistingFirst">If true, tells simulator to remove existing attachment
	/// first</param>
	public void AddAttachments(List<InventoryItem> attachments, boolean removeExistingFirst)
	{
		// Use RezMultipleAttachmentsFromInv  to clear out current attachments, and attach new ones
		RezMultipleAttachmentsFromInvPacket attachmentsPacket = new RezMultipleAttachmentsFromInvPacket();
		attachmentsPacket.AgentData.AgentID = Client.self.getAgentID();
		attachmentsPacket.AgentData.SessionID = Client.self.getSessionID();

		attachmentsPacket.HeaderData.CompoundMsgID = UUID.Random();
		attachmentsPacket.HeaderData.FirstDetachAll = removeExistingFirst;
		attachmentsPacket.HeaderData.TotalObjects = (byte)attachments.size();

		attachmentsPacket.ObjectData = new RezMultipleAttachmentsFromInvPacket.ObjectDataBlock[attachments.size()];
		for (int i = 0; i < attachments.size(); i++)
		{
			if (attachments.get(i) instanceof InventoryAttachment)
			{
				InventoryAttachment attachment = (InventoryAttachment)attachments.get(i);
				attachmentsPacket.ObjectData[i] = new RezMultipleAttachmentsFromInvPacket.ObjectDataBlock();
				attachmentsPacket.ObjectData[i].AttachmentPt = (byte)attachment.getAttachmentPoint().getIndex();
				attachmentsPacket.ObjectData[i].EveryoneMask = (long)Permissions.PermissionMask.getIndex(attachment.Permissions.EveryoneMask);
				attachmentsPacket.ObjectData[i].GroupMask = (long)Permissions.PermissionMask.getIndex(attachment.Permissions.GroupMask);
				attachmentsPacket.ObjectData[i].ItemFlags = (long)attachment.Flags;
				attachmentsPacket.ObjectData[i].ItemID = attachment.UUID;
				attachmentsPacket.ObjectData[i].Name = Utils.stringToBytesWithTrailingNullByte(attachment.Name);
				attachmentsPacket.ObjectData[i].Description = Utils.stringToBytesWithTrailingNullByte(attachment.Description);
				attachmentsPacket.ObjectData[i].NextOwnerMask = (long)Permissions.PermissionMask.getIndex(attachment.Permissions.NextOwnerMask);
				attachmentsPacket.ObjectData[i].OwnerID = attachment.OwnerID;
			}
			else if (attachments.get(i) instanceof InventoryObject)
			{
				InventoryObject attachment = (InventoryObject)attachments.get(i);
				attachmentsPacket.ObjectData[i] = new RezMultipleAttachmentsFromInvPacket.ObjectDataBlock();
				attachmentsPacket.ObjectData[i].AttachmentPt = 0;
				attachmentsPacket.ObjectData[i].EveryoneMask = (long)Permissions.PermissionMask.getIndex(attachment.Permissions.EveryoneMask);
				attachmentsPacket.ObjectData[i].GroupMask = (long)Permissions.PermissionMask.getIndex(attachment.Permissions.GroupMask);
				attachmentsPacket.ObjectData[i].ItemFlags = (long)attachment.Flags;
				attachmentsPacket.ObjectData[i].ItemID = attachment.UUID;
				attachmentsPacket.ObjectData[i].Name = Utils.stringToBytesWithTrailingNullByte(attachment.Name);
				attachmentsPacket.ObjectData[i].Description = Utils.stringToBytesWithTrailingNullByte(attachment.Description);
				attachmentsPacket.ObjectData[i].NextOwnerMask = (long)Permissions.PermissionMask.getIndex(attachment.Permissions.NextOwnerMask);
				attachmentsPacket.ObjectData[i].OwnerID = attachment.OwnerID;
			}
			else
			{
				JLogger.warn("Cannot attach inventory item " + attachments.get(i).Name);
			}
		}

		Client.network.SendPacket(attachmentsPacket);
	}

	/// <summary>
	/// Attach an item to our agent at a specific attach point
	/// </summary>
	/// <param name="item">A <seealso cref="OpenMetaverse.InventoryItem"/> to attach</param>
	/// <param name="attachPoint">the <seealso cref="OpenMetaverse.AttachmentPoint"/> on the avatar 
	/// to attach the item to</param>
	public void Attach(InventoryItem item, AttachmentPoint attachPoint)
	{
		Attach(item.UUID, item.OwnerID, item.Name, item.Description, item.Permissions, item.Flags,
				attachPoint);
	}

	/// <summary>
	/// Attach an item to our agent specifying attachment details
	/// </summary>
	/// <param name="itemID">The <seealso cref="OpenMetaverse.UUID"/> of the item to attach</param>
	/// <param name="ownerID">The <seealso cref="OpenMetaverse.UUID"/> attachments owner</param>
	/// <param name="name">The name of the attachment</param>
	/// <param name="description">The description of the attahment</param>
	/// <param name="perms">The <seealso cref="OpenMetaverse.Permissions"/> to apply when attached</param>
	/// <param name="itemFlags">The <seealso cref="OpenMetaverse.InventoryItemFlags"/> of the attachment</param>
	/// <param name="attachPoint">The <seealso cref="OpenMetaverse.AttachmentPoint"/> on the agent
	/// to attach the item to</param>
	public void Attach(UUID itemID, UUID ownerID, String name, String description,
			Permissions perms, long itemFlags, AttachmentPoint attachPoint)
	{
		// TODO: At some point it might be beneficial to have AppearanceManager track what we
		// are currently wearing for attachments to make enumeration and detachment easier
		RezSingleAttachmentFromInvPacket attach = new RezSingleAttachmentFromInvPacket();

		attach.AgentData.AgentID = Client.self.getAgentID();
		attach.AgentData.SessionID = Client.self.getSessionID();

		attach.ObjectData.AttachmentPt = (byte)attachPoint.getIndex();
		attach.ObjectData.Description = Utils.stringToBytesWithTrailingNullByte(description);
		attach.ObjectData.EveryoneMask = (long)Permissions.PermissionMask.getIndex(perms.EveryoneMask);
		attach.ObjectData.GroupMask = (long)Permissions.PermissionMask.getIndex(perms.GroupMask);
		attach.ObjectData.ItemFlags = itemFlags;
		attach.ObjectData.ItemID = itemID;
		attach.ObjectData.Name = Utils.stringToBytesWithTrailingNullByte(name);
		attach.ObjectData.NextOwnerMask = (long)Permissions.PermissionMask.getIndex(perms.NextOwnerMask);
		attach.ObjectData.OwnerID = ownerID;

		Client.network.SendPacket(attach);
	}

	/// <summary>
	/// Detach an item from our agent using an <seealso cref="OpenMetaverse.InventoryItem"/> object
	/// </summary>
	/// <param name="item">An <seealso cref="OpenMetaverse.InventoryItem"/> object</param>
	public void Detach(InventoryItem item)
	{
		Detach(item.UUID);
	}

	/// <summary>
	/// Detach an item from our agent
	/// </summary>
	/// <param name="itemID">The inventory itemID of the item to detach</param>
	public void Detach(UUID itemID)
	{
		DetachAttachmentIntoInvPacket detach = new DetachAttachmentIntoInvPacket();
		detach.ObjectData.AgentID = Client.self.getAgentID();
		detach.ObjectData.ItemID = itemID;

		Client.network.SendPacket(detach);
	}

	//endregion Attachments

	//region Appearance Helpers

	/// <summary>
	/// Inform the sim which wearables are part of our current outfit
	/// </summary>
	private void SendAgentIsNowWearing()
	{
		AgentIsNowWearingPacket wearing = new AgentIsNowWearingPacket();
		wearing.AgentData.AgentID = Client.self.getAgentID();
		wearing.AgentData.SessionID = Client.self.getSessionID();
		wearing.WearableData = new AgentIsNowWearingPacket.WearableDataBlock[WEARABLE_COUNT];

		synchronized (Wearables)
		{
			for (int i = 0; i < WEARABLE_COUNT; i++)
			{
				WearableType type = WearableType.get((byte)i);
				wearing.WearableData[i] = new AgentIsNowWearingPacket.WearableDataBlock();
				wearing.WearableData[i].WearableType = (byte)i;

				if (Wearables.containsKey(type))
					wearing.WearableData[i].ItemID = Wearables.get(type).ItemID;
				else
					wearing.WearableData[i].ItemID = UUID.Zero;
			}
		}

		Client.network.SendPacket(wearing);
	}

	/// <summary>
	/// Replaces the Wearables collection with a list of new wearable items
	/// </summary>
	/// <param name="wearableItems">Wearable items to replace the Wearables collection with</param>
	private void ReplaceOutfit2(List<InventoryWearable> wearableItems)
	{
		Map<WearableType, WearableData> newWearables = new HashMap<WearableType, WearableData>();

		synchronized (Wearables)
		{
			// Preserve body parts from the previous set of wearables. They may be overwritten,
			// but cannot be missing in the new set
			for (Entry<WearableType, WearableData> entry : Wearables.entrySet())
			{
				if (entry.getValue().AssetType == AssetType.Bodypart)
					newWearables.put(entry.getKey(), entry.getValue());
			}

			// Add the given wearables to the new wearables collection
			for (int i = 0; i < wearableItems.size(); i++)
			{
				InventoryWearable wearableItem = wearableItems.get(i);

				WearableData wd = new WearableData();
				wd.AssetID = wearableItem.AssetUUID;
				wd.AssetType = wearableItem.AssetType;
				wd.ItemID = wearableItem.UUID;
				wd.WearableType = wearableItem.getWearableType();

				newWearables.put(wearableItem.getWearableType(), wd);
			}

			// Replace the Wearables collection
			Wearables = newWearables;
		}
	}

	/// <summary>
	/// Calculates base color/tint for a specific wearable
	/// based on its params
	/// </summary>
	/// <param name="param">All the color info gathered from wearable's VisualParams
	/// passed as list of ColorParamInfo tuples</param>
	/// <returns>Base color/tint for the wearable</returns>
	private Color4 GetColorFromParams(List<ColorParamInfo> param)
	{
		// Start off with a blank slate, black, fully transparent
		Color4 res = new Color4(0, 0, 0, 0);

		// Apply color modification from each color parameter
		for (ColorParamInfo p : param)
		{
			int n = p.VisualColorParam.Colors.length;

			Color4 paramColor = new Color4(0, 0, 0, 0);

			if (n == 1)
			{
				// We got only one color in this param, use it for application
				// to the final color
				paramColor = p.VisualColorParam.Colors[0];
			}
			else if (n > 1)
			{
				// We have an array of colors in this parameter
				// First, we need to find out, based on param value
				// between which two elements of the array our value lands

				// Size of the step using which we iterate from Min to Max
				float step = (p.VisualParam.MaxValue - p.VisualParam.MinValue) / ((float)n - 1);

				// Our color should land inbetween colors in the array with index a and b
				int indexa = 0;
				int indexb = 0;

				int i = 0;

				for (float a = p.VisualParam.MinValue; a <= p.VisualParam.MaxValue; a += step)
				{
					if (a <= p.Value)
					{
						indexa = i;
					}
					else
					{
						break;
					}

					i++;
				}

				// Sanity check that we don't go outside bounds of the array
				if (indexa > n - 1)
					indexa = n - 1;

				indexb = (indexa == n - 1) ? indexa : indexa + 1;

				// How far is our value from Index A on the 
				// line from Index A to Index B
				float distance = p.Value - (float)indexa * step;

				// We are at Index A (allowing for some floating point math fuzz),
				// use the color on that index
				if (distance < 0.00001f || indexa == indexb)
				{
					paramColor = p.VisualColorParam.Colors[indexa];
				}
				else
				{
					// Not so simple as being precisely on the index eh? No problem.
					// We take the two colors that our param value places us between
					// and then find the value for each ARGB element that is
					// somewhere on the line between color1 and color2 at some
					// distance from the first color
					Color4 c1 = paramColor = p.VisualColorParam.Colors[indexa];
					Color4 c2 = paramColor = p.VisualColorParam.Colors[indexb];

					// Distance is some fraction of the step, use that fraction
					// to find the value in the range from color1 to color2
					paramColor = Color4.lerp(c1, c2, distance / step);
				}

				// Please leave this fragment even if its commented out
				// might prove useful should ($deity forbid) there be bugs in this code
				//string carray = "";
				//foreach (Color c in p.VisualColorParam.Colors)
				//{
				//    carray += c.ToString() + " - ";
				//}
				//Logger.DebugLog("Calculating color for " + p.WearableType + " from " + p.VisualParam.Name + ", value is " + p.Value + " in range " + p.VisualParam.MinValue + " - " + p.VisualParam.MaxValue + " step " + step + " with " + n + " elements " + carray + " A: " + indexa + " B: " + indexb + " at distance " + distance);
			}

			// Now that we have calculated color from the scale of colors
			// that visual params provided, lets apply it to the result
			switch (p.VisualColorParam.Operation)
			{
			case Add:
				res = Color4.add(res, paramColor);
				break;
			case Multiply:
				res = Color4.multiply(res, paramColor);
				break;
			case Blend:
				res = Color4.lerp(res, paramColor, p.Value);
				break;
			}
		}

		return res;
	}

	/// <summary>
	/// Blocking method to populate the Wearables dictionary
	/// </summary>
	/// <returns>True on success, otherwise false</returns>
	boolean GetAgentWearables() throws InterruptedException
	{
		final AutoResetEvent wearablesEvent = new AutoResetEvent(false);
		EventObserver<AgentWearablesReplyEventArgs> wearablesCallback =
				new EventObserver<AgentWearablesReplyEventArgs>()
				{
			@Override
			public void handleEvent(Observable s,
					AgentWearablesReplyEventArgs e) 
			{
				wearablesEvent.set();											
			}
				};

				this.registerOnAgentWearablesReply(wearablesCallback);			
				RequestAgentWearables();
				boolean success = wearablesEvent.waitOne(WEARABLE_TIMEOUT);
				this.unregisterOnAgentWearablesReply(wearablesCallback);
				return success;
	}

	/// <summary>
	/// Blocking method to populate the Textures array with cached bakes
	/// </summary>
	/// <returns>True on success, otherwise false</returns>
	boolean GetCachedBakes() throws InterruptedException
	{
		final AutoResetEvent cacheCheckEvent = new AutoResetEvent(false);
		EventObserver<AgentCachedBakesReplyEventArgs> cacheCallback = 
				new EventObserver<AgentCachedBakesReplyEventArgs>()
				{
			@Override
			public void handleEvent(Observable s,
					AgentCachedBakesReplyEventArgs e) {
				cacheCheckEvent.set();											
			}
				}; 

				this.registerOnCachedBakesReply(cacheCallback);

				RequestCachedBakes();

				boolean success = cacheCheckEvent.waitOne(WEARABLE_TIMEOUT);
				this.unregisterOnCachedBakesReply(cacheCallback);

				return success;
	}

	/// <summary>
	/// Populates textures and visual params from a decoded asset
	/// </summary>
	/// <param name="wearable">Wearable to decode</param>
	private void DecodeWearableParams(WearableData wearable)
	{
		Map<VisualAlphaParam, Float> alphaMasks = new HashMap<VisualAlphaParam, Float>();
		List<ColorParamInfo> colorParams = new ArrayList<ColorParamInfo>();

		// Populate collection of alpha masks from visual params
		// also add color tinting information
		for (Entry<Integer, Float> kvp : wearable.Asset.Params.entrySet())
		{
			if (!VisualParams.Params.containsKey(kvp.getKey())) continue;

			VisualParam p = VisualParams.Params.get(kvp.getKey());

			ColorParamInfo colorInfo = new ColorParamInfo();
			colorInfo.WearableType = wearable.WearableType;
			colorInfo.VisualParam = p;
			colorInfo.Value = kvp.getValue();

			// Color params
			if (p.ColorParams !=null)
			{
				colorInfo.VisualColorParam = p.ColorParams;

				// If this is not skin, just add params directly
				if (wearable.WearableType != WearableType.Skin)
				{
					colorParams.add(colorInfo);
				}
				else
				{
					// For skin we skip makeup params for now and use only the 3
					// that are used to determine base skin tone
					// Param 108 - Rainbow Color
					// Param 110 - Red Skin (Ruddiness)
					// Param 111 - Pigment
					if (kvp.getKey() == 108 || kvp.getKey() == 110 || kvp.getKey() == 111)
					{
						colorParams.add(colorInfo);
					}
				}
			}

			// Add alpha mask
			if (p.AlphaParams!=null  
					&& !p.AlphaParams.TGAFile.equals("") 
					&& !p.IsBumpAttribute 
					&& !alphaMasks.containsKey(p.AlphaParams))
			{
				alphaMasks.put(p.AlphaParams, kvp.getValue());
			}

			// Alhpa masks can also be specified in sub "driver" params
			if (p.Drivers != null)
			{
				for (int i = 0; i < p.Drivers.length; i++)
				{
					if (VisualParams.Params.containsKey(p.Drivers[i]))
					{
						VisualParam driver = VisualParams.Params.get(p.Drivers[i]);
						if (driver.AlphaParams != null 
								&& !Utils.isNullOrEmpty(driver.AlphaParams.TGAFile) 
								&& !driver.IsBumpAttribute 
								&& !alphaMasks.containsKey(driver.AlphaParams))
						{
							alphaMasks.put(driver.AlphaParams, kvp.getValue());
						}
					}
				}
			}
		}

		Color4 wearableColor = Color4.White; // Never actually used
		if (colorParams.size() > 0)
		{
			wearableColor = GetColorFromParams(colorParams);
			JLogger.debug("Setting tint " + wearableColor + " for " + wearable.WearableType);
		}

		// Loop through all of the texture IDs in this decoded asset and put them in our cache of worn textures
		for (Entry<AvatarTextureIndex, UUID> entry : wearable.Asset.Textures.entrySet())
		{
			int i = (int)entry.getKey().getIndex();

			// Update information about color and alpha masks for this texture
			Textures[i].AlphaMasks = alphaMasks;
			Textures[i].Color = wearableColor;

			// If this texture changed, update the TextureID and clear out the old cached texture asset
			if (!Textures[i].TextureID.equals(entry.getValue()))
			{
				// Treat DEFAULT_AVATAR_TEXTURE as null
				if (!entry.getValue().equals(DEFAULT_AVATAR_TEXTURE))
					Textures[i].TextureID = entry.getValue();
				else
					Textures[i].TextureID = UUID.Zero;
				JLogger.debug("Set " + entry.getKey() + " to " + Textures[i].TextureID);

				Textures[i].Texture = null;
			}
		}
	}

	/// <summary>
	/// Blocking method to download and parse currently worn wearable assets
	/// </summary>
	/// <returns>True on success, otherwise false</returns>
	private boolean DownloadWearables() throws InterruptedException
	{
		final AtomicBoolean success = new AtomicBoolean(true);

		// Make a copy of the wearables dictionary to enumerate over
		Map<WearableType, WearableData> wearables;
		synchronized (Wearables)
		{
			wearables = new HashMap<WearableType, WearableData>(Wearables);
		}

		// We will refresh the textures (zero out all non bake textures)
		for (int i = 0; i < Textures.length; i++)
		{
			boolean isBake = false;
			for (int j = 0; j < BakeIndexToTextureIndex.length; j++)
			{
				if (BakeIndexToTextureIndex[j] == i)
				{
					isBake = true;
					break;
				}
			}
			if (!isBake)
				Textures[i] = new TextureData();
		}

		final AtomicInteger pendingWearables = new AtomicInteger(wearables.size());
		for (WearableData wearable : wearables.values())
		{
			if (wearable.Asset != null)
			{
				DecodeWearableParams(wearable);
				pendingWearables.decrementAndGet();
			}
		}

		if (pendingWearables.get() == 0)
			return true;

		JLogger.debug("Downloading " + pendingWearables + " wearable assets");

		List<Runnable> tasks1 = new ArrayList<Runnable>();
		for(final WearableData wearable: wearables.values())
		{
			Runnable runnable = new Runnable(){
				public void run() {
					try{
						if (wearable.Asset == null)
						{
							final AutoResetEvent downloadEvent = new AutoResetEvent(false);

							MethodDelegate<Void, AssetReceivedCallbackArgs> assetReceivedCallback
							= new MethodDelegate<Void, AssetReceivedCallbackArgs>()
							{
								public Void execute(AssetReceivedCallbackArgs e) {
									AssetDownload transfer = e.getTransfer();
									Asset asset = e.getAsset();
									if (transfer.Success && asset instanceof AssetWearable)
									{
										// Update this wearable with the freshly downloaded asset 
										wearable.Asset = (AssetWearable)asset;

										if (wearable.Asset.Decode())
										{
											DecodeWearableParams(wearable);
											JLogger.debug("Downloaded wearable asset " + wearable.WearableType + " with " + wearable.Asset.Params.size() +
													" visual params and " + wearable.Asset.Textures.size() + " textures");

										}
										else
										{
											wearable.Asset = null;
											JLogger.error("Failed to decode asset:" + "\n" +
													Utils.bytesToHexDebugString(asset.AssetData, ""));
										}
									}
									else
									{
										JLogger.warn("Wearable " + wearable.AssetID + "(" + wearable.WearableType + ") failed to download, " +
												transfer.Status);
									}

									downloadEvent.set();
									return null;
								}
							};

							// Fetch this wearable asset
							Client.assets.RequestAsset(wearable.AssetID, wearable.AssetType, true, assetReceivedCallback);


							if (!downloadEvent.waitOne(WEARABLE_TIMEOUT))
							{
								JLogger.error("Timed out downloading wearable asset " + wearable.AssetID + " (" + wearable.WearableType + ")");
								success.set(false);
							}

							pendingWearables.decrementAndGet();
						}
					}
					catch(Exception e)
					{
						JLogger.warn("Exception while running the task: \n" + Utils.getExceptionStackTraceAsString(e));
					}
				}
			};
			tasks1.add(runnable);
		}

		ThreadPoolFactory.executeParallel(tasks1.toArray(new Runnable[0]), Math.min(pendingWearables.get(), MAX_CONCURRENT_DOWNLOADS));

		return success.get();
	}

	/// <summary>
	/// Get a list of all of the textures that need to be downloaded for a
	/// single bake layer
	/// </summary>
	/// <param name="bakeType">Bake layer to get texture AssetIDs for</param>
	/// <returns>A list of texture AssetIDs to download</returns>
	private List<UUID> GetTextureDownloadList(BakeType bakeType)
	{
		List<AvatarTextureIndex> indices = BakeTypeToTextures(bakeType);
		List<UUID> textures = new ArrayList<UUID>();

		for (int i = 0; i < indices.size(); i++)
		{
			AvatarTextureIndex index = indices.get(i);

			if (index.equals(AvatarTextureIndex.Skirt) && !Wearables.containsKey(WearableType.Skirt))
				continue;

			AddTextureDownload(index, textures);
		}

		return textures;
	}

	/// <summary>
	/// Helper method to lookup the TextureID for a single layer and add it
	/// to a list if it is not already present
	/// </summary>
	/// <param name="index"></param>
	/// <param name="textures"></param>
	private void AddTextureDownload(AvatarTextureIndex index, List<UUID> textures)
	{
		TextureData textureData = Textures[(int)index.getIndex()];
		// Add the textureID to the list if this layer has a valid textureID set, it has not already
		// been downloaded, and it is not already in the download list
		if (!textureData.TextureID.equals(UUID.Zero) && textureData.Texture == null 
				&& !textures.contains(textureData.TextureID))
		{
			textures.add(textureData.TextureID);
			JLogger.debug("Adding Texture to download: " + textureData.TextureID.toString());
		}
	}

	/// <summary>
	/// Blocking method to download all of the textures needed for baking 
	/// the given bake layers
	/// </summary>
	/// <param name="bakeLayers">A list of layers that need baking</param>
	/// <remarks>No return value is given because the baking will happen
	/// whether or not all textures are successfully downloaded</remarks>
	private void DownloadTextures(List<BakeType> bakeLayers) throws InterruptedException
	{
		List<UUID> textureIDs = new ArrayList<UUID>();

		for (int i = 0; i < bakeLayers.size(); i++)
		{
			List<UUID> layerTextureIDs = GetTextureDownloadList(bakeLayers.get(i));

			for (int j = 0; j < layerTextureIDs.size(); j++)
			{
				UUID uuid = layerTextureIDs.get(j);
				if (!textureIDs.contains(uuid))
					textureIDs.add(uuid);
			}
		}

		JLogger.debug("Downloading " + textureIDs.size() + " textures for baking");

		List<Runnable> tasks1 = new ArrayList<Runnable>();
		for(final UUID textureID: textureIDs)
		{
			Runnable runnable = new Runnable(){
				public void run() {
					try{
						final AutoResetEvent downloadEvent = new AutoResetEvent(false);

						MethodDelegate<Void, TextureDownloadCallbackArgs> textureDownloadCallback = 
								new MethodDelegate<Void, TextureDownloadCallbackArgs>()
								{
							public Void execute(TextureDownloadCallbackArgs e) 
							{
								try{
									TextureRequestState state = e.getState();
									AssetTexture assetTexture = e.getAssetTexture();
									if (state == TextureRequestState.Finished)
									{
										JLogger.info("Downloaded Texture " + textureID + " Proceeding for backing...");
										if(assetTexture.Decode())
										{
											for (int i = 0; i < Textures.length; i++)
											{
												if (Textures[i].TextureID.equals(textureID))
												{
													JLogger.info("Setting AssetTextureIndex " + i + " to "+ textureID );
													Textures[i].Texture = assetTexture;
												}
											}
										}
										else
											throw new Exception("Failed to decode Asset texture: " + textureID );
									}
									else
									{
										JLogger.warn("Texture " + textureID + " failed to download, one or more bakes will be incomplete");
									}

								}
								catch(Exception ex)
								{JLogger.warn("Texture " + textureID + " failed to download or parsed one or more bakes will be incomplete\n" + Utils.getExceptionStackTraceAsString(ex));}
								finally
								{ downloadEvent.set();}
								return null;
							}};

							Client.assets.RequestImage(textureID, textureDownloadCallback);

							downloadEvent.waitOne(TEXTURE_TIMEOUT);
					}
					catch(Exception e)
					{
						JLogger.warn("Exception while running the task: \n" + Utils.getExceptionStackTraceAsString(e));
					}
				}
			};
			tasks1.add(runnable);
		}
		ThreadPoolFactory.executeParallel(tasks1.toArray(new Runnable[0]), MAX_CONCURRENT_DOWNLOADS);
	}

	/// <summary>
	/// Blocking method to create and upload baked textures for all of the
	/// missing bakes
	/// </summary>
	/// <returns>True on success, otherwise false</returns>
	private boolean CreateBakes() throws InterruptedException
	{
		final AtomicBoolean success = new AtomicBoolean(true);
		List<BakeType> pendingBakes = new ArrayList<BakeType>();

		// Check each bake layer in the Textures array for missing bakes
		for (int bakedIndex = 0; bakedIndex < BAKED_TEXTURE_COUNT; bakedIndex++)
		{
			AvatarTextureIndex textureIndex = BakeTypeToAgentTextureIndex(BakeType.get(bakedIndex));

			if (Textures[(int)textureIndex.getIndex()].TextureID.equals(UUID.Zero))
			{
				// If this is the skirt layer and we're not wearing a skirt then skip it
				if (bakedIndex == (int)BakeType.Skirt.getIndex() 
						&& !Wearables.containsKey(WearableType.Skirt.getIndex()))
					continue;

				pendingBakes.add(BakeType.get(bakedIndex));
			}
		}

		if (pendingBakes.size() > 0)
		{
			JLogger.debug("Going to download Textures");
			DownloadTextures(pendingBakes);

			JLogger.debug("Going to CreateBake");
			List<Runnable> tasks1 = new ArrayList<Runnable>();
			for(final BakeType bakeType: pendingBakes)
			{
				Runnable runnable = new Runnable(){
					public void run() {
						try{
							if (!CreateBake(bakeType))
								success.set(false);
						}
						catch(Exception e)
						{
							JLogger.warn("Exception while running the task: \n" + Utils.getExceptionStackTraceAsString(e));
							success.set(false);
						}
					}
				};
				tasks1.add(runnable);
			}
			ThreadPoolFactory.executeParallel(tasks1.toArray(new Runnable[0]), Math.min(MAX_CONCURRENT_UPLOADS, pendingBakes.size()));
		}

		if(!success.get())
			JLogger.debug("One or more Baking of textures has failed");

		// Free up all the textures we're holding on to
		for (int i = 0; i < Textures.length; i++)
		{
			Textures[i].Texture = null;
		}

		// We just allocated and freed a ridiculous amount of memory while 
		// baking. Signal to the GC to clean up
		//				            GC.Collect();

		return success.get();
	}

	/// <summary>
	/// Blocking method to create and upload a baked texture for a single 
	/// bake layer
	/// </summary>
	/// <param name="bakeType">Layer to bake</param>
	/// <returns>True on success, otherwise false</returns>
	private boolean CreateBake(BakeType bakeType) throws Exception
	{
		List<AvatarTextureIndex> textureIndices = BakeTypeToTextures(bakeType);
		Baker oven = new Baker(bakeType);

		for (int i = 0; i < textureIndices.size(); i++)
		{
			AvatarTextureIndex textureIndex = textureIndices.get(i);
			TextureData texture = Textures[(int)textureIndex.getIndex()];
			if(texture == null)
				JLogger.warn("Texture is null for index" + textureIndex.getIndex());

			//TODO need to verfy why original code was sending even Zero textures for baking
			if(!texture.TextureID.equals(UUID.Zero))
			{
				JLogger.debug("Adding Texture to oven"  + texture.toString());
				texture.TextureIndex = textureIndex;
				oven.AddTexture(texture);
			}
		}

		long start = Utils.getUnixTime();
		JLogger.debug("Going to oven.Bake..");
		oven.Bake();
		JLogger.debug("Task Baking " + bakeType + " took " + (Utils.getUnixTime() - start) + "ms");

		UUID newAssetID = UUID.Zero;
		int retries = UPLOAD_RETRIES;

		while (newAssetID.equals(UUID.Zero) && retries > 0)
		{
			newAssetID = UploadBake(oven.getBakedTexture().AssetData);
			--retries;
		}

		Textures[(int)BakeTypeToAgentTextureIndex(bakeType).getIndex()].TextureID = newAssetID;

		if (newAssetID.equals(UUID.Zero))
		{
			JLogger.warn("Failed uploading bake " + bakeType);
			return false;
		}

		JLogger.debug(String.format("Uploaded Bake ID: %s to Server retry no %d", newAssetID, retries));

		//TODO Need to remove only for testing
//        String filepath = "openmetaverse_data/logs/" + newAssetID;
//        FileUtils.writeBytes(new File(filepath+".j2k"), oven.getBakedTexture().AssetData);
//		Client.assets.Cache.saveAssetToCache(newAssetID, oven.getBakedTexture().AssetData);
		return true;
	}

	/// <summary>
	/// Blocking method to upload a baked texture
	/// </summary>
	/// <param name="textureData">Five channel JPEG2000 texture data to upload</param>
	/// <returns>UUID of the newly created asset on success, otherwise UUID.Zero</returns>
	private UUID UploadBake(byte[] textureData) throws Exception
	{
		final UUID[] bakeID = new UUID[]{UUID.Zero};
		final AutoResetEvent uploadEvent = new AutoResetEvent(false);

		MethodDelegate<Void, BakedTextureUploadedCallbackArgs> bakedTextureUploadedCallback 
		= new MethodDelegate<Void, BakedTextureUploadedCallbackArgs>()
		{
			public Void execute(
					BakedTextureUploadedCallbackArgs e) {
				UUID newAssetID = e.getNewAssetID();
				bakeID[0] = newAssetID;
				uploadEvent.set();
				return null;
			}

		};
		JLogger.debug(String.format("Going to wait %d ms for texture to get uploaded", UPLOAD_TIMEOUT));

		Client.assets.RequestUploadBakedTexture(textureData,bakedTextureUploadedCallback);


		// FIXME: evalute the need for timeout here, RequestUploadBakedTexture() will
		// timout either on Client.Settings.TRANSFER_TIMEOUT or Client.Settings.CAPS_TIMEOUT
		// depending on which upload method is used.
		uploadEvent.waitOne(UPLOAD_TIMEOUT);
		JLogger.debug("Successfully Uploaded baked Texture got asset ID: " + bakeID[0]);
		return bakeID[0];
	}

	//TODO implement if necessary
	//	        /// <summary>
	//	        /// Creates a dictionary of visual param values from the downloaded wearables
	//	        /// </summary>
	//	        /// <returns>A dictionary of visual param indices mapping to visual param
	//	        /// values for our agent that can be fed to the Baker class</returns>
	//	        private Map<int, float> MakeParamValues()
	//	        {
	//	            Map<int, float> paramValues = new HashMap<int, float>(VisualParams.Params.Count);
	//
	//	            synchronized (Wearables)
	//	            {
	//	                foreach (Entry<int, VisualParam> kvp in VisualParams.Params)
	//	                {
	//	                    // Only Group-0 parameters are sent in AgentSetAppearance packets
	//	                    if (kvp.Value.Group == 0)
	//	                    {
	//	                        boolean found = false;
	//	                        VisualParam vp = kvp.Value;
	//
	//	                        // Try and find this value in our collection of downloaded wearables
	//	                        foreach (WearableData data in Wearables.Values)
	//	                        {
	//	                            float paramValue;
	//	                            if (data.Asset != null && data.Asset.Params.TryGetValue(vp.ParamID, out paramValue))
	//	                            {
	//	                                paramValues.Add(vp.ParamID, paramValue);
	//	                                found = true;
	//	                                break;
	//	                            }
	//	                        }
	//
	//	                        // Use a default value if we don't have one set for it
	//	                        if (!found) paramValues.Add(vp.ParamID, vp.DefaultValue);
	//	                    }
	//	                }
	//	            }
	//
	//	            return paramValues;
	//	        }
	//

	/// <summary>
	/// Create an AgentSetAppearance packet from Wearables data and the 
	/// Textures array and send it
	/// </summary>
	private void RequestAgentSetAppearance() throws Exception
	{
		AgentSetAppearancePacket set = new AgentSetAppearancePacket();
		set.AgentData.AgentID = Client.self.getAgentID();
		set.AgentData.SessionID = Client.self.getSessionID();
		set.AgentData.SerialNum = SetAppearanceSerialNum.incrementAndGet();

		// Visual params used in the agent height calculation
		float agentSizeVPHeight = 0.0f;
		float agentSizeVPHeelHeight = 0.0f;
		float agentSizeVPPlatformHeight = 0.0f;
		float agentSizeVPHeadSize = 0.5f;
		float agentSizeVPLegLength = 0.0f;
		float agentSizeVPNeckLength = 0.0f;
		float agentSizeVPHipLength = 0.0f;
		synchronized (Wearables)
		{
			//region VisualParam

			int vpIndex = 0;
			int nrParams;
			boolean wearingPhysics = false;

			for (WearableData wearable : Wearables.values())
			{
				if (wearable.WearableType == WearableType.Physics)
				{
					wearingPhysics = true;
					break;
				}
			}

			if (wearingPhysics)
			{
				nrParams = 251;
			}
			else
			{
				nrParams = 218;
			}

			set.VisualParam = new AgentSetAppearancePacket.VisualParamBlock[nrParams];

			for (Entry<Integer, VisualParam> kvp : VisualParams.Params.entrySet())
			{
				VisualParam vp = kvp.getValue();
				Float paramValue = 0f;
				boolean found = false;

				// Try and find this value in our collection of downloaded wearables
				for (WearableData data : Wearables.values())
				{
					if (data.Asset != null && ((paramValue = data.Asset.Params.get(vp.ParamID))!=null))
					{
						found = true;
						break;
					}
				}

				// Use a default value if we don't have one set for it
				if (!found)
					paramValue = vp.DefaultValue;

				// Only Group-0 parameters are sent in AgentSetAppearance packets
				if (kvp.getValue().Group == 0)
				{
					set.VisualParam[vpIndex] = new AgentSetAppearancePacket.VisualParamBlock();
					set.VisualParam[vpIndex].ParamValue = Utils.floatToByte(paramValue, vp.MinValue, vp.MaxValue);
					++vpIndex;
				}

				// Check if this is one of the visual params used in the agent height calculation
				switch (vp.ParamID)
				{
				case 33:
					agentSizeVPHeight = paramValue;
					break;
				case 198:
					agentSizeVPHeelHeight = paramValue;
					break;
				case 503:
					agentSizeVPPlatformHeight = paramValue;
					break;
				case 682:
					agentSizeVPHeadSize = paramValue;
					break;
				case 692:
					agentSizeVPLegLength = paramValue;
					break;
				case 756:
					agentSizeVPNeckLength = paramValue;
					break;
				case 842:
					agentSizeVPHipLength = paramValue;
					break;
				}

				if (vpIndex == nrParams) break;
			}

			MyVisualParameters = new byte[set.VisualParam.length];
			for (int i = 0; i < set.VisualParam.length; i++)
			{
				MyVisualParameters[i] = set.VisualParam[i].ParamValue;
			}

			//endregion VisualParam

			//region TextureEntry

			TextureEntry te = new TextureEntry(DEFAULT_AVATAR_TEXTURE);

			for (int i = 0; i < Textures.length; i++)
			{
				if ((i == 0 || i == 5 || i == 6) && !Client.settings.CLIENT_IDENTIFICATION_TAG.equals(UUID.Zero))
				{
					TextureEntryFace face = te.CreateFace(i);
					face.setTextureID(Client.settings.CLIENT_IDENTIFICATION_TAG);
					JLogger.debug("Sending client identification tag: " + Client.settings.CLIENT_IDENTIFICATION_TAG);
				}
				else if (!Textures[i].TextureID.equals(UUID.Zero))
				{
					TextureEntryFace face = te.CreateFace(i);
					face.setTextureID(Textures[i].TextureID);
					JLogger.debug("Sending texture entry for " + AvatarTextureIndex.get(i) + " to " + Textures[i].TextureID);
				}
			}

			set.ObjectData.TextureEntry = te.GetBytes();
			MyTextures = te;

			//endregion TextureEntry

			//region WearableData

			set.WearableData = new AgentSetAppearancePacket.WearableDataBlock[BAKED_TEXTURE_COUNT];

			// Build hashes for each of the bake layers from the individual components
			for (int bakedIndex = 0; bakedIndex < BAKED_TEXTURE_COUNT; bakedIndex++)
			{
				UUID hash = UUID.Zero;

				for (int wearableIndex = 0; wearableIndex < WEARABLES_PER_LAYER; wearableIndex++)
				{
					WearableType type = WEARABLE_BAKE_MAP[bakedIndex][wearableIndex];

					WearableData wearable;
					if (type != WearableType.Invalid && ((wearable = Wearables.get(type))!=null))
						hash = UUID.xor(hash, wearable.AssetID);
				}

				if (!hash.equals(UUID.Zero))
				{
					// Hash with our magic value for this baked layer
					hash = UUID.xor(hash,  BAKED_TEXTURE_HASH[bakedIndex]);
				}

				// Tell the server what cached texture assetID to use for each bake layer
				set.WearableData[bakedIndex] = new AgentSetAppearancePacket.WearableDataBlock();
				set.WearableData[bakedIndex].TextureIndex = BakeIndexToTextureIndex[bakedIndex];
				set.WearableData[bakedIndex].CacheID = hash;
				JLogger.debug("Sending TextureIndex " + BakeType.get(bakedIndex) + " with CacheID " + hash);
			}

			//endregion WearableData

			//region Agent Size

			// Takes into account the Shoe Heel/Platform offsets but not the HeadSize offset. Seems to work.
			double agentSizeBase = 1.706;

			// The calculation for the HeadSize scalar may be incorrect, but it seems to work
			double agentHeight = agentSizeBase + (agentSizeVPLegLength * .1918) + (agentSizeVPHipLength * .0375) +
					(agentSizeVPHeight * .12022) + (agentSizeVPHeadSize * .01117) + (agentSizeVPNeckLength * .038) +
					(agentSizeVPHeelHeight * .08) + (agentSizeVPPlatformHeight * .07);

			set.AgentData.Size = new Vector3(0.45f, 0.6f, (float)agentHeight);

			//endregion Agent Size

			if (Client.settings.AVATAR_TRACKING)
			{
				Avatar me;
				if ((me = Client.network.getCurrentSim().ObjectsAvatars.get(Client.self.getLocalID()))!=null)
				{
					me.Textures = MyTextures;
					me.VisualParameters = MyVisualParameters;
				}
			}
		}

//		JLogger.logPkt("AgentSetAppearancePacket", set.ToBytes());
		Client.network.SendPacket(set);
		JLogger.debug("Send AgentSetAppearance packet");
	}

	private void DelayedRequestSetAppearance()
	{
		if (RebakeScheduleTimer == null)
		{
			RebakeScheduleTimer = new EventTimer(new TimerTask()
			{
				@Override
				public void run() {
					RebakeScheduleTimerTick();
				}                		
			});
		}
		RebakeScheduleTimer.schedule(REBAKE_DELAY); 
	}

	private void RebakeScheduleTimerTick()
	{
		RequestSetAppearance(true);
	}
	//endregion Appearance Helpers

	//region Inventory Helpers

	//TODO implement if necessary
	//				        private boolean GetFolderWearables(String[] folderPath, out List<InventoryWearable> wearables, out List<InventoryItem> attachments)
	//				        {
	//				            UUID folder = Client.Inventory.FindObjectByPath(
	//				                Client.Inventory.Store.RootFolder.UUID, Client.self.getAgentID(), String.Join("/", folderPath), INVENTORY_TIMEOUT);
	//			
	//				            if (folder != UUID.Zero)
	//				            {
	//				                return GetFolderWearables(folder, out wearables, out attachments);
	//				            }
	//				            else
	//				            {
	//				                Logger.Log("Failed to resolve outfit folder path " + folderPath, Helpers.LogLevel.Error, Client);
	//				                wearables = null;
	//				                attachments = null;
	//				                return false;
	//				            }
	//				        }

	//				        private boolean GetFolderWearables(UUID folder, out List<InventoryWearable> wearables, out List<InventoryItem> attachments)
	//				        {
	//				            wearables = new ArrayList<InventoryWearable>();
	//				            attachments = new ArrayList<InventoryItem>();
	//				            List<InventoryBase> objects = Client.Inventory.FolderContents(folder, Client.self.getAgentID(), false, true,
	//				                InventorySortOrder.ByName, INVENTORY_TIMEOUT);
	//			
	//				            if (objects != null)
	//				            {
	//				                foreach (InventoryBase ib in objects)
	//				                {
	//				                    if (ib instanceof InventoryWearable)
	//				                    {
	//				                        Logger.DebugLog("Adding wearable " + ib.Name, Client);
	//				                        wearables.add((InventoryWearable)ib);
	//				                    }
	//				                    else if (ib instanceof InventoryAttachment)
	//				                    {
	//				                        Logger.DebugLog("Adding attachment (attachment) " + ib.Name, Client);
	//				                        attachments.add((InventoryItem)ib);
	//				                    }
	//				                    else if (ib instanceof InventoryObject)
	//				                    {
	//				                        Logger.DebugLog("Adding attachment (object) " + ib.Name, Client);
	//				                        attachments.add((InventoryItem)ib);
	//				                    }
	//				                    else
	//				                    {
	//				                        Logger.DebugLog("Ignoring inventory item " + ib.Name, Client);
	//				                    }
	//				                }
	//				            }
	//				            else
	//				            {
	//				                Logger.Log("Failed to download folder contents of + " + folder, Helpers.LogLevel.Error, Client);
	//				                return false;
	//				            }
	//			
	//				            return true;
	//				        }
	//			
	//endregion Inventory Helpers

	//region Callbacks

	protected void AgentWearablesUpdateHandler(Object sender, PacketReceivedEventArgs e)
	{
		boolean changed = false;
		AgentWearablesUpdatePacket update = (AgentWearablesUpdatePacket)e.getPacket();

		synchronized (Wearables)
		{
			//region Test if anything changed in this update

			for (int i = 0; i < update.WearableData.length; i++)
			{
				AgentWearablesUpdatePacket.WearableDataBlock block = update.WearableData[i];

				if (!block.AssetID.equals(UUID.Zero))
				{
					WearableData wearable;
					if ((wearable = Wearables.get(WearableType.get(block.WearableType)))!=null)
					{
						if (!wearable.AssetID.equals(block.AssetID) || !wearable.ItemID.equals(block.ItemID))
						{
							// A different wearable is now set for this index
							changed = true;
							break;
						}
					}
					else
					{
						// A wearable is now set for this index
						changed = true;
						break;
					}
				}
				else if (Wearables.containsKey(WearableType.get(block.WearableType)))
				{
					// This index is now empty
					changed = true;
					break;
				}
			}

			//endregion Test if anything changed in this update

			if (changed)
			{
				JLogger.debug("New wearables received in AgentWearablesUpdate");
				Wearables.clear();

				for (int i = 0; i < update.WearableData.length; i++)
				{
					AgentWearablesUpdatePacket.WearableDataBlock block = update.WearableData[i];

					if (!block.AssetID.equals(UUID.Zero))
					{
						WearableType type = WearableType.get(block.WearableType);

						WearableData data = new WearableData();
						data.Asset = null;
						data.AssetID = block.AssetID;
						data.AssetType = WearableTypeToAssetType(type);
						data.ItemID = block.ItemID;
						data.WearableType = type;

						// Add this wearable to our collection
						Wearables.put(type, data);
						JLogger.debug("Received WearableData: " +  data.toString());
					}
				}
			}
			else
			{
				JLogger.debug("Duplicate AgentWearablesUpdate received, discarding");
			}
		}

		if (changed)
		{
			// Fire the callback
			onAgentWearablesReply.raiseEvent(new AgentWearablesReplyEventArgs());
		}
	}

	protected void RebakeAvatarTexturesHandler(Object sender, PacketReceivedEventArgs e)
	{
		RebakeAvatarTexturesPacket rebake = (RebakeAvatarTexturesPacket)e.getPacket();

		// allow the library to do the rebake
		if (Client.settings.SEND_AGENT_APPEARANCE)
		{
			RequestSetAppearance(true);
		}

		onRebakeAvatarRequested.raiseEvent(new RebakeAvatarTexturesEventArgs(rebake.TextureData.TextureID));
	}

	protected void AgentCachedTextureResponseHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		AgentCachedTextureResponsePacket response = (AgentCachedTextureResponsePacket)e.getPacket();

		for (int i = 0; i < response.WearableData.length; i++)
		{
			AgentCachedTextureResponsePacket.WearableDataBlock block = response.WearableData[i];
			BakeType bakeType = BakeType.get(Utils.ubyteToInt(block.TextureIndex));
			AvatarTextureIndex index = BakeTypeToAgentTextureIndex(bakeType);

			JLogger.debug("Cache response for " + bakeType + ", TextureID=" + block.TextureID);

			if (!block.TextureID.equals(UUID.Zero))
			{
				// A simulator has a cache of this bake layer

				// FIXME: Use this. Right now we don't bother to check if this is a foreign host
				String host = Utils.bytesWithTrailingNullByteToString(block.HostName);

				Textures[(int)index.getIndex()].TextureID = block.TextureID;
			}
			else
			{
				// The server does not have a cache of this bake layer
				// FIXME:
			}
		}

		onCachedBakesReply.raiseEvent(new AgentCachedBakesReplyEventArgs());
	}

	private void Network_OnEventQueueRunning(Object sender, EventQueueRunningEventArgs e)
	{
		if (e.getSimulator().equals(Client.network.getCurrentSim()) 
				&& Client.settings.SEND_AGENT_APPEARANCE)
		{
			// Update appearance each time we enter a new sim and capabilities have been retrieved
			Client.appearance.RequestSetAppearance();
		}
	}

	private void Network_OnDisconnected(Object sender, DisconnectedEventArgs e)
	{
		if (RebakeScheduleTimer != null)
		{
			RebakeScheduleTimer.cancel();
			RebakeScheduleTimer = null;
		}

		//FIXME How to dispose appearnce thread
		//				            if (AppearanceThread != null)
		//				            {
		//				                if (AppearanceThread.IsAlive)
		//				                {
		//				                    AppearanceThread.Abort();
		//				                }
		//				                AppearanceThread = null;
		//				                AppearanceThreadRunning = 0;
		//				            }
	}

	//endregion Callbacks

	//region Static Helpers

	/// <summary>
	/// Converts a WearableType to a bodypart or clothing WearableType
	/// </summary>
	/// <param name="type">A WearableType</param>
	/// <returns>AssetType.Bodypart or AssetType.Clothing or AssetType.Unknown</returns>
	public static AssetType WearableTypeToAssetType(WearableType type)
	{
		switch (type)
		{
		case Shape:
		case Skin:
		case Hair:
		case Eyes:
			return AssetType.Bodypart;
		case Shirt:
		case Pants:
		case Shoes:
		case Socks:
		case Jacket:
		case Gloves:
		case Undershirt:
		case Underpants:
		case Skirt:
		case Tattoo:
		case Alpha:
		case Physics:
			return AssetType.Clothing;
		default:
			return AssetType.Unknown;
		}
	}

	/// <summary>
	/// Converts a BakeType to the corresponding baked texture slot in AvatarTextureIndex
	/// </summary>
	/// <param name="index">A BakeType</param>
	/// <returns>The AvatarTextureIndex slot that holds the given BakeType</returns>
	public static AvatarTextureIndex BakeTypeToAgentTextureIndex(BakeType index)
	{
		switch (index)
		{
		case Head:
			return AvatarTextureIndex.HeadBaked;
		case UpperBody:
			return AvatarTextureIndex.UpperBaked;
		case LowerBody:
			return AvatarTextureIndex.LowerBaked;
		case Eyes:
			return AvatarTextureIndex.EyesBaked;
		case Skirt:
			return AvatarTextureIndex.SkirtBaked;
		case Hair:
			return AvatarTextureIndex.HairBaked;
		default:
			return AvatarTextureIndex.Unknown;
		}
	}

	/// <summary>
	/// Gives the layer number that is used for morph mask
	/// </summary>
	/// <param name="bakeType">>A BakeType</param>
	/// <returns>Which layer number as defined in BakeTypeToTextures is used for morph mask</returns>
	public static AvatarTextureIndex MorphLayerForBakeType(BakeType bakeType)
	{
		// Indexes return here correspond to those returned
		// in BakeTypeToTextures(), those two need to be in sync.
		// Which wearable layer is used for morph is defined in avatar_lad.xml
		// by looking for <layer> that has <morph_mask> defined in it, and
		// looking up which wearable is defined in that layer. Morph mask
		// is never combined, it's always a straight copy of one single clothing
		// item's alpha channel per bake.
		switch (bakeType)
		{
		case Head:
			return AvatarTextureIndex.Hair; // hair
		case UpperBody:
			return AvatarTextureIndex.UpperShirt; // shirt
		case LowerBody:
			return AvatarTextureIndex.LowerPants; // lower pants
		case Skirt:
			return AvatarTextureIndex.Skirt; // skirt
		case Hair:
			return AvatarTextureIndex.Hair; // hair
		default:
			return AvatarTextureIndex.Unknown;
		}
	}

	/// <summary>
	/// Converts a BakeType to a list of the texture slots that make up that bake
	/// </summary>
	/// <param name="bakeType">A BakeType</param>
	/// <returns>A list of texture slots that are inputs for the given bake</returns>
	public static List<AvatarTextureIndex> BakeTypeToTextures(BakeType bakeType)
	{
		List<AvatarTextureIndex> textures = new ArrayList<AvatarTextureIndex>();

		switch (bakeType)
		{
		case Head:
			textures.add(AvatarTextureIndex.HeadBodypaint);
			textures.add(AvatarTextureIndex.HeadTattoo);
			textures.add(AvatarTextureIndex.Hair);
			textures.add(AvatarTextureIndex.HeadAlpha);
			break;
		case UpperBody:
			textures.add(AvatarTextureIndex.UpperBodypaint);
			textures.add(AvatarTextureIndex.UpperTattoo);
			textures.add(AvatarTextureIndex.UpperGloves);
			textures.add(AvatarTextureIndex.UpperUndershirt);
			textures.add(AvatarTextureIndex.UpperShirt);
			textures.add(AvatarTextureIndex.UpperJacket);
			textures.add(AvatarTextureIndex.UpperAlpha);
			break;
		case LowerBody:
			textures.add(AvatarTextureIndex.LowerBodypaint);
			textures.add(AvatarTextureIndex.LowerTattoo);
			textures.add(AvatarTextureIndex.LowerUnderpants);
			textures.add(AvatarTextureIndex.LowerSocks);
			textures.add(AvatarTextureIndex.LowerShoes);
			textures.add(AvatarTextureIndex.LowerPants);
			textures.add(AvatarTextureIndex.LowerJacket);
			textures.add(AvatarTextureIndex.LowerAlpha);
			break;
		case Eyes:
			textures.add(AvatarTextureIndex.EyesIris);
			textures.add(AvatarTextureIndex.EyesAlpha);
			break;
		case Skirt:
			textures.add(AvatarTextureIndex.Skirt);
			break;
		case Hair:
			textures.add(AvatarTextureIndex.Hair);
			textures.add(AvatarTextureIndex.HairAlpha);
			break;
		}

		return textures;
	}

	//endregion Static Helpers
}
