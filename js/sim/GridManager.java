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

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.protocol.CoarseLocationUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.MapBlockReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.MapBlockRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.MapItemReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.MapItemRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.MapNameRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.RegionHandleRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.RegionIDAndHandleReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.SimulatorViewerTimeMessagePacket;
import com.ngt.jopenmetaverse.shared.sim.Simulator.RegionFlags;
import com.ngt.jopenmetaverse.shared.sim.Simulator.SimAccess;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.gm.CoarseLocationUpdateEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.gm.GridItemsEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.gm.GridLayerEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.gm.GridRegionEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.gm.RegionHandleReplyEventArgs;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// Manages grid-wide tasks such as the world map
/// </summary>
public class GridManager {
	//region Enums

	/// <summary>
	/// Map layer request type
	/// </summary>
	public enum GridLayerType
	{
		//uint
		/// <summary>Objects and terrain are shown</summary>
		Objects(0),
		/// <summary>Only the terrain is shown, no objects</summary>
		Terrain(1),
		/// <summary>Overlay showing land for sale and for auction</summary>
		LandForSale(2);
		private long index;
		GridLayerType(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		//		private static final Map<Long,GridLayerType> lookup  = new HashMap<Long,GridLayerType>();
		//
		//		static {
		//			for(GridLayerType s : EnumSet.allOf(GridLayerType.class))
		//				lookup.put(s.getIndex(), s);
		//		}
		//
		//		public static GridLayerType get(Long index)
		//		{
		//			return lookup.get(index);
		//		}
	}

	/// <summary>
	/// Type of grid item, such as telehub, event, populator location, etc.
	/// </summary>
	public enum GridItemType
	{
		//uint
		/// <summary>Telehub</summary>
		Telehub(1),
		/// <summary>PG rated event</summary>
		PgEvent(2),
		/// <summary>Mature rated event</summary>
		MatureEvent(3),
		/// <summary>Popular location</summary>
		Popular(4),
		/// <summary>Locations of avatar groups in a region</summary>
		AgentLocations(6),
		/// <summary>Land for sale</summary>
		LandForSale(7),
		/// <summary>Classified ad</summary>
		Classified(8),
		/// <summary>Adult rated event</summary>
		AdultEvent(9),
		/// <summary>Adult land for sale</summary>
		AdultLandForSale(10);
		private long index;
		GridItemType(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,GridItemType> lookup  = new HashMap<Long,GridItemType>();

		static {
			for(GridItemType s : EnumSet.allOf(GridItemType.class))
				lookup.put(s.getIndex(), s);
		}

		public static GridItemType get(Long index)
		{
			return lookup.get(index);
		}
	}

	//endregion Enums

	//region Structs

	/// <summary>
	/// Information about a region on the grid map
	/// </summary>
	public class GridRegion
	{
		/// <summary>Sim X position on World Map</summary>
		public int X;
		/// <summary>Sim Y position on World Map</summary>
		public int Y;
		/// <summary>Sim Name (NOTE: In lowercase!)</summary>
		public String Name;
		/// <summary></summary>
		public SimAccess Access;
		/// <summary>Appears to always be zero (None)</summary>
		public EnumSet<RegionFlags> RegionFlags;
		/// <summary>Sim's defined Water Height</summary>
		public byte WaterHeight;
		/// <summary></summary>
		public byte Agents;
		/// <summary>UUID of the World Map image</summary>
		public UUID MapImageID;
		/// <summary>Unique identifier for this region, a combination of the X 
		/// and Y position</summary>
		//ulong
		public BigInteger RegionHandle;


		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		@Override
		public String toString()
		{
			return String.format("%s (%d/%d), Handle: %s, MapImage: %s, Access: %s, Flags: %s",
					Name, X, Y, RegionHandle.toString(), MapImageID.toString(), Access.toString(), RegionFlags.toString());
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		@Override
		public int hashCode()
		{
			return (new Integer(X)).hashCode() ^ (new Integer(Y)).hashCode();
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="obj"></param>
		/// <returns></returns>
		@Override
		public boolean equals(Object obj)
		{
			if (obj instanceof GridRegion)
				return equals((GridRegion)obj);
			else
				return false;
		}

		private boolean equals(GridRegion region)
		{
			return (this.X == region.X && this.Y == region.Y);
		}
	}

	/// <summary>
	/// Visual chunk of the grid map
	/// </summary>
	public class GridLayer
	{
		public int Bottom;
		public int Left;
		public int Top;
		public int Right;
		public UUID ImageID;

		public boolean containsRegion(int x, int y)
		{
			return (x >= Left && x <= Right && y >= Bottom && y <= Top);
		}
	}

	//endregion Structs

	//region Map Item Classes

	/// <summary>
	/// Base class for Map Items
	/// </summary>
	public abstract class MapItem
	{
		/// <summary>The Global X position of the item</summary>
		//uint
		public long GlobalX;
		/// <summary>The Global Y position of the item</summary>
		//uint
		public long GlobalY;

		/// <summary>Get the Local X position of the item</summary>
		//uint
		public long getLocalX() { return GlobalX % 256; } 
		/// <summary>Get the Local Y position of the item</summary>
		//uint
		public long getLocalY() { return GlobalY % 256; } 

		/// <summary>Get the Handle of the region</summary>
		//ulong
		public BigInteger getRegionHandle()
		{
			return Utils.uintsToULong((long)(GlobalX - (GlobalX % 256)), (long)(GlobalY - (GlobalY % 256))); 
		}
	}

	/// <summary>
	/// Represents an agent or group of agents location
	/// </summary>
	public class MapAgentLocation extends MapItem
	{       
		public int AvatarCount;
		public String Identifier;
	}

	/// <summary>
	/// Represents a Telehub location
	/// </summary>
	public class MapTelehub extends MapItem
	{        
	}

	/// <summary>
	/// Represents a non-adult parcel of land for sale
	/// </summary>
	public class MapLandForSale extends MapItem
	{        
		public int Size;
		public int Price;
		public String Name;
		public UUID ID;        
	}

	/// <summary>
	/// Represents an Adult parcel of land for sale
	/// </summary>
	public class MapAdultLandForSale extends MapItem
	{     
		public int Size;
		public int Price;
		public String Name;
		public UUID ID;
	}

	/// <summary>
	/// Represents a PG Event
	/// </summary>
	public class MapPGEvent extends MapItem
	{
		public DirectoryManager.EventFlags Flags; // Extra
		public DirectoryManager.EventCategories Category; // Extra2
		public String Description;
	}

	/// <summary>
	/// Represents a Mature event
	/// </summary>
	public class MapMatureEvent extends  MapItem
	{
		public DirectoryManager.EventFlags Flags; // Extra
		public DirectoryManager.EventCategories Category; // Extra2
		public String Description;
	}

	/// <summary>
	/// Represents an Adult event
	/// </summary>
	public class MapAdultEvent extends  MapItem
	{
		public DirectoryManager.EventFlags Flags; // Extra
		public DirectoryManager.EventCategories Category; // Extra2
		public String Description;
	}
	//endregion Grid Item Classes

	private EventObservable<CoarseLocationUpdateEventArgs> OnCoarseLocationUpdate = new EventObservable<CoarseLocationUpdateEventArgs>();
	private EventObservable<GridRegionEventArgs> OnGridRegion = new EventObservable<GridRegionEventArgs>();
	private EventObservable<GridLayerEventArgs> OnGridLayer = new EventObservable<GridLayerEventArgs>();
	private EventObservable<GridItemsEventArgs> OnGridItems = new EventObservable<GridItemsEventArgs>();
	private EventObservable<RegionHandleReplyEventArgs> OnRegionHandleReply = new EventObservable<RegionHandleReplyEventArgs>();

	public void registerOnCoarseLocationUpdate(EventObserver<CoarseLocationUpdateEventArgs> o)
	{
		OnCoarseLocationUpdate.addObserver(o);
	}

	public void unregisterOnCoarseLocationUpdate(EventObserver<CoarseLocationUpdateEventArgs> o)
	{
		OnCoarseLocationUpdate.deleteObserver(o);
	}

	public void registerOnGridRegion(EventObserver<GridRegionEventArgs> o)
	{
		OnGridRegion.addObserver(o);
	}

	public void unregisterOnGridRegion(EventObserver<GridRegionEventArgs> o)
	{
		OnGridRegion.deleteObserver(o);
	}

	public void registerOnGridLayer(EventObserver<GridLayerEventArgs> o)
	{
		OnGridLayer.addObserver(o);
	}

	public void unregisterOnGridLayer(EventObserver<GridLayerEventArgs> o)
	{
		OnGridLayer.deleteObserver(o);
	}

	public void registerOnGridItems(EventObserver<GridItemsEventArgs> o)
	{
		OnGridItems.addObserver(o);
	}

	public void unregisterOnGridItems(EventObserver<GridItemsEventArgs> o)
	{
		OnGridItems.deleteObserver(o);
	}

	public void registerOnRegionHandleReply(EventObserver<RegionHandleReplyEventArgs> o)
	{
		OnRegionHandleReply.addObserver(o);
	}

	public void unregisterOnRegionHandleReply(EventObserver<RegionHandleReplyEventArgs> o)
	{
		OnRegionHandleReply.deleteObserver(o);
	}


	//        //region Delegates
	//
	//        /// <summary>The event subscribers. null if no subcribers</summary>
	//        private EventHandler<CoarseLocationUpdateEventArgs> m_CoarseLocationUpdate;
	//
	//        /// <summary>Raises the CoarseLocationUpdate event</summary>
	//        /// <param name="e">A CoarseLocationUpdateEventArgs object containing the
	//        /// data sent by simulator</param>
	//        protected virtual void OnCoarseLocationUpdate(CoarseLocationUpdateEventArgs e)
	//        {
	//            EventHandler<CoarseLocationUpdateEventArgs> handler = m_CoarseLocationUpdate;
	//            if (handler != null)
	//                handler(this, e);
	//        }
	//
	//        /// <summary>Thread sync lock object</summary>
	//        private readonly object m_CoarseLocationUpdateLock = new object();
	//
	//        /// <summary>Raised when the simulator sends a <see cref="CoarseLocationUpdatePacket"/> 
	//        /// containing the location of agents in the simulator</summary>
	//        public event EventHandler<CoarseLocationUpdateEventArgs> CoarseLocationUpdate
	//        {
	//            add { lock (m_CoarseLocationUpdateLock) { m_CoarseLocationUpdate += value; } }
	//            remove { lock (m_CoarseLocationUpdateLock) { m_CoarseLocationUpdate -= value; } }
	//        }
	//
	//        /// <summary>The event subscribers. null if no subcribers</summary>
	//        private EventHandler<GridRegionEventArgs> m_GridRegion;
	//
	//        /// <summary>Raises the GridRegion event</summary>
	//        /// <param name="e">A GridRegionEventArgs object containing the
	//        /// data sent by simulator</param>
	//        protected virtual void OnGridRegion(GridRegionEventArgs e)
	//        {
	//            EventHandler<GridRegionEventArgs> handler = m_GridRegion;
	//            if (handler != null)
	//                handler(this, e);
	//        }
	//
	//        /// <summary>Thread sync lock object</summary>
	//        private readonly object m_GridRegionLock = new object();
	//
	//        /// <summary>Raised when the simulator sends a Region Data in response to 
	//        /// a Map request</summary>
	//        public event EventHandler<GridRegionEventArgs> GridRegion
	//        {
	//            add { lock (m_GridRegionLock) { m_GridRegion += value; } }
	//            remove { lock (m_GridRegionLock) { m_GridRegion -= value; } }
	//        }
	//
	//        /// <summary>The event subscribers. null if no subcribers</summary>
	//        private EventHandler<GridLayerEventArgs> m_GridLayer;
	//
	//        /// <summary>Raises the GridLayer event</summary>
	//        /// <param name="e">A GridLayerEventArgs object containing the
	//        /// data sent by simulator</param>
	//        protected virtual void OnGridLayer(GridLayerEventArgs e)
	//        {
	//            EventHandler<GridLayerEventArgs> handler = m_GridLayer;
	//            if (handler != null)
	//                handler(this, e);
	//        }
	//
	//        /// <summary>Thread sync lock object</summary>
	//        private readonly object m_GridLayerLock = new object();
	//
	//        /// <summary>Raised when the simulator sends GridLayer object containing
	//        /// a map tile coordinates and texture information</summary>
	//        public event EventHandler<GridLayerEventArgs> GridLayer
	//        {
	//            add { lock (m_GridLayerLock) { m_GridLayer += value; } }
	//            remove { lock (m_GridLayerLock) { m_GridLayer -= value; } }
	//        }
	//
	//        /// <summary>The event subscribers. null if no subcribers</summary>
	//        private EventHandler<GridItemsEventArgs> m_GridItems;
	//
	//        /// <summary>Raises the GridItems event</summary>
	//        /// <param name="e">A GridItemEventArgs object containing the
	//        /// data sent by simulator</param>
	//        protected virtual void OnGridItems(GridItemsEventArgs e)
	//        {
	//            EventHandler<GridItemsEventArgs> handler = m_GridItems;
	//            if (handler != null)
	//                handler(this, e);
	//        }
	//
	//        /// <summary>Thread sync lock object</summary>
	//        private readonly object m_GridItemsLock = new object();
	//
	//        /// <summary>Raised when the simulator sends GridItems object containing
	//        /// details on events, land sales at a specific location</summary>
	//        public event EventHandler<GridItemsEventArgs> GridItems
	//        {
	//            add { lock (m_GridItemsLock) { m_GridItems += value; } }
	//            remove { lock (m_GridItemsLock) { m_GridItems -= value; } }
	//        }
	//
	//        /// <summary>The event subscribers. null if no subcribers</summary>
	//        private EventHandler<RegionHandleReplyEventArgs> m_RegionHandleReply;
	//
	//        /// <summary>Raises the RegionHandleReply event</summary>
	//        /// <param name="e">A RegionHandleReplyEventArgs object containing the
	//        /// data sent by simulator</param>
	//        protected virtual void OnRegionHandleReply(RegionHandleReplyEventArgs e)
	//        {
	//            EventHandler<RegionHandleReplyEventArgs> handler = m_RegionHandleReply;
	//            if (handler != null)
	//                handler(this, e);
	//        }
	//
	//        /// <summary>Thread sync lock object</summary>
	//        private readonly object m_RegionHandleReplyLock = new object();
	//
	//        /// <summary>Raised in response to a Region lookup</summary>
	//        public event EventHandler<RegionHandleReplyEventArgs> RegionHandleReply
	//        {
	//            add { lock (m_RegionHandleReplyLock) { m_RegionHandleReply += value; } }
	//            remove { lock (m_RegionHandleReplyLock) { m_RegionHandleReply -= value; } }
	//        }
	//
	//        //endregion Delegates

	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();

	/// <summary>Unknown</summary>
	public float getSunPhase() {  return sunPhase; } 
	/// <summary>Current direction of the sun</summary>
	public Vector3 getSunDirection() { return sunDirection; } 
	/// <summary>Current angular velocity of the sun</summary>
	public Vector3 getSunAngVelocity() {  return sunAngVelocity; } 
	/// <summary>Microseconds since the start of SL 4-hour day</summary>
	//ulong
	public long getTimeOfDay() { return timeOfDay; } 

	/// <summary>A dictionary of all the regions, indexed by region name</summary>
	public Map<String, GridRegion> Regions = new HashMap<String, GridRegion>();
	/// <summary>A dictionary of all the regions, indexed by region handle</summary>
	//<BigInteger, GridRegion>
	public Map<BigInteger, GridRegion> RegionsByHandle = new HashMap<BigInteger, GridRegion>();

	private GridClient Client;
	private float sunPhase;
	private Vector3 sunDirection;
	private Vector3 sunAngVelocity;
	private long timeOfDay;

	/// <summary>
	/// Constructor
	/// </summary>
	/// <param name="client">Instance of GridClient object to associate with this GridManager instance</param>
	public GridManager(GridClient client)
	{
		Client = client;

		//            //Client.Network.RegisterCallback(PacketType.MapLayerReply, MapLayerReplyHandler);
		//            Client.network.RegisterCallback(PacketType.MapBlockReply, MapBlockReplyHandler);
		Client.network.RegisterCallback(PacketType.MapBlockReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{ MapBlockReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}});

		//            Client.network.RegisterCallback(PacketType.MapItemReply, MapItemReplyHandler);
		Client.network.RegisterCallback(PacketType.MapItemReply,
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{MapItemReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}}
				);

		//            Client.network.RegisterCallback(PacketType.SimulatorViewerTimeMessage, SimulatorViewerTimeMessageHandler);
		Client.network.RegisterCallback(PacketType.SimulatorViewerTimeMessage,  new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{SimulatorViewerTimeMessageHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

		//            Client.network.RegisterCallback(PacketType.CoarseLocationUpdate, CoarseLocationHandler, false);
		Client.network.RegisterCallback(PacketType.CoarseLocationUpdate, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{CoarseLocationHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}}  
		, false);

		//            Client.network.RegisterCallback(PacketType.RegionIDAndHandleReply, RegionHandleReplyHandler);
		Client.network.RegisterCallback(PacketType.RegionIDAndHandleReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{RegionHandleReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}}  
				);

	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="layer"></param>
	public void RequestMapLayer(GridLayerType layer) throws Exception
	{
		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("MapLayer");

		if (url != null)
		{
			OSDMap body = new OSDMap();
			body.put("Flags", OSD.FromInteger((int)layer.getIndex()));

			CapsHttpClient request = new CapsHttpClient(url);
			//                request.OnComplete += new CapsHttpClient.CompleteCallback(MapLayerResponseHandler);
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>(){
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					MapLayerResponseHandler(arg.getClient(), arg.getResult(), arg.getError());
				}
			});
			request.BeginGetResponse(body, OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
	}

	/// <summary>
	/// Request a map layer
	/// </summary>
	/// <param name="regionName">The name of the region</param>
	/// <param name="layer">The type of layer</param>
	public void RequestMapRegion(String regionName, GridLayerType layer)
	{
		MapNameRequestPacket request = new MapNameRequestPacket();

		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.AgentData.Flags = layer.getIndex();
		request.AgentData.EstateID = 0; // Filled in on the sim
		request.AgentData.Godlike = false; // Filled in on the sim
		request.NameData.Name = Utils.stringToBytesWithTrailingNullByte(regionName);

		Client.network.SendPacket(request);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="layer"></param>
	/// <param name="minX"></param>
	/// <param name="minY"></param>
	/// <param name="maxX"></param>
	/// <param name="maxY"></param>
	/// <param name="returnNonExistent"></param>
	public void RequestMapBlocks(GridLayerType layer, int minX, int minY, int maxX, int maxY, boolean returnNonExistent)
	{
		MapBlockRequestPacket request = new MapBlockRequestPacket();

		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.AgentData.Flags = layer.getIndex();
		request.AgentData.Flags |= (long)(returnNonExistent ? 0x10000 : 0);
		request.AgentData.EstateID = 0; // Filled in at the simulator
		request.AgentData.Godlike = false; // Filled in at the simulator

		request.PositionData.MinX = minX;
		request.PositionData.MinY = minY;
		request.PositionData.MaxX = maxX;
		request.PositionData.MaxY = maxY;

		Client.network.SendPacket(request);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="regionHandle"></param>
	/// <param name="item"></param>
	/// <param name="layer"></param>
	/// <param name="timeoutMS"></param>
	/// <returns></returns>
	public List<MapItem> MapItems(BigInteger regionHandle, GridItemType item, GridLayerType layer, int timeoutMS) throws InterruptedException
	{
		List<MapItem> itemList = null;
		final AutoResetEvent itemsEvent = new AutoResetEvent(false);

		//            EventHandler<GridItemsEventArgs> callback =
		//                delegate(Object sender, GridItemsEventArgs e)
		//                {
		//                    if (e.Type == GridItemType.AgentLocations)
		//                    {
		//                        itemList = e.Items;
		//                        itemsEvent.Set();
		//                    }
		//                };
		//
		//            GridItems += callback;
		//
		//            RequestMapItems(regionHandle, item, layer);

		final Object[] itemListArray = new Object[]{null};  
		EventObserver<GridItemsEventArgs> callback = new EventObserver<GridItemsEventArgs>()
				{
			@Override
			public void handleEvent(Observable o, GridItemsEventArgs e) {
				if (e.getType().equals(GridItemType.AgentLocations))
				{
					itemListArray[0] = e.getItems();
					itemsEvent.set();
				}}	
				};
				itemList = (List<MapItem>)itemListArray[0];

				OnGridItems.addObserver(callback);
				RequestMapItems(regionHandle, item, layer);

				itemsEvent.waitOne(timeoutMS);

				OnGridItems.deleteObserver(callback);

				return itemList;
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="regionHandle"></param>
	/// <param name="item"></param>
	/// <param name="layer"></param>
	public void RequestMapItems(BigInteger regionHandle, GridItemType item, GridLayerType layer)
	{
		MapItemRequestPacket request = new MapItemRequestPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.AgentData.Flags = layer.getIndex();
		request.AgentData.Godlike = false; // Filled in on the sim
		request.AgentData.EstateID = 0; // Filled in on the sim

		request.RequestData.ItemType = item.getIndex();
		request.RequestData.RegionHandle = regionHandle;

		Client.network.SendPacket(request);
	}

	/// <summary>
	/// Request data for all mainland (Linden managed) simulators
	/// </summary>
	public void RequestMainlandSims(GridLayerType layer)
	{
		RequestMapBlocks(layer, 0, 0, 65535, 65535, false);
	}

	/// <summary>
	/// Request the region handle for the specified region UUID
	/// </summary>
	/// <param name="regionID">UUID of the region to look up</param>
	public void RequestRegionHandle(UUID regionID)
	{
		RegionHandleRequestPacket request = new RegionHandleRequestPacket();
		request.RequestBlock = new RegionHandleRequestPacket.RequestBlockBlock();
		request.RequestBlock.RegionID = regionID;
		Client.network.SendPacket(request);
	}

	/// <summary>
	/// Get grid region information using the region name, this function
	/// will block until it can find the region or gives up
	/// </summary>
	/// <param name="name">Name of sim you're looking for</param>
	/// <param name="layer">Layer that you are requesting</param>
	/// <param name="region">Will contain a GridRegion for the sim you're
	/// looking for if successful, otherwise an empty structure</param>
	/// <returns>True if the GridRegion was successfully fetched, otherwise
	/// false</returns>
	public boolean GetGridRegion(final String name, GridLayerType layer, GridRegion[] region) throws InterruptedException
	{
		if (Utils.isNullOrEmpty(name))
		{
			JLogger.error("GetGridRegion called with a null or empty region name");
			region[0] = new GridRegion();
			return false;
		}

		if (Regions.containsKey(name))
		{
			// We already have this GridRegion structure
			region[0] = Regions.get(name);
			return true;
		}
		else
		{
			final AutoResetEvent regionEvent = new AutoResetEvent(false);

			EventObserver<GridRegionEventArgs> callback = new EventObserver<GridRegionEventArgs>()
					{ @Override
				public void handleEvent(Observable o, GridRegionEventArgs e) 
					{
						if (e.getRegion().Name.equals(name))
							regionEvent.set();
					}
					};

					OnGridRegion.addObserver(callback);

					RequestMapRegion(name, layer);
					regionEvent.waitOne(Client.settings.MAP_REQUEST_TIMEOUT);

					OnGridRegion.deleteObserver(callback);

					if (Regions.containsKey(name))
					{
						// The region was found after our request
						region[0] = Regions.get(name);
						return true;
					}
					else
					{
						JLogger.warn("Couldn't find region " + name);
						region[0] = new GridRegion();
						return false;
					}


					//			EventHandler<GridRegionEventArgs> callback =
					//					delegate(Object sender, GridRegionEventArgs e)
					//					{
					//				if (e.Region.Name == name)
					//					regionEvent.Set();
					//					};
					//					GridRegion += callback;
					//
					//					RequestMapRegion(name, layer);
					//					regionEvent.WaitOne(Client.Settings.MAP_REQUEST_TIMEOUT, false);
					//
					//					GridRegion -= callback;
					//
					//					if (Regions.ContainsKey(name))
					//					{
					//						// The region was found after our request
					//						region = Regions[name];
					//						return true;
					//					}
					//					else
					//					{
					//						Logger.Log("Couldn't find region " + name, Helpers.LogLevel.Warning, Client);
					//						region = new GridRegion();
					//						return false;
					//					}
		}
	}

	protected void MapLayerResponseHandler(CapsHttpClient client, OSD result, Exception error)
	{
		OSDMap body = (OSDMap)result;
		OSDArray layerData = (OSDArray)body.get("LayerData");

		if (OnGridLayer != null)
		{
			for (int i = 0; i < layerData.count(); i++)
			{
				OSDMap thisLayerData = (OSDMap)layerData.get(i);

				GridLayer layer = new GridLayer();
				layer.Bottom = thisLayerData.get("Bottom").asInteger();
				layer.Left = thisLayerData.get("Left").asInteger();
				layer.Top = thisLayerData.get("Top").asInteger();
				layer.Right = thisLayerData.get("Right").asInteger();
				layer.ImageID = thisLayerData.get("ImageID").asUUID();

				OnGridLayer.raiseEvent((new GridLayerEventArgs(layer)));                    
			}
		}

		if (body.containsKey("MapBlocks"))
		{
			// TODO: At one point this will become activated
			JLogger.error("Got MapBlocks through CAPS, please finish this function!");
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void MapBlockReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		MapBlockReplyPacket map = (MapBlockReplyPacket)e.getPacket();

		for (MapBlockReplyPacket.DataBlock block :map.Data)
		{
			if (block.X != 0 && block.Y != 0)
			{
				GridRegion region = new GridRegion();

				region.X = block.X;
				region.Y = block.Y;
				region.Name = Utils.bytesWithTrailingNullByteToString(block.Name);
				// RegionFlags seems to always be zero here?
				region.RegionFlags = RegionFlags.get((int)block.RegionFlags);
				region.WaterHeight = block.WaterHeight;
				region.Agents = block.Agents;
				region.Access = SimAccess.get((short)block.Access);
				region.MapImageID = block.MapImageID;
				region.RegionHandle = Utils.uintsToULong(((long)region.X * 256), ((long)region.Y * 256));

				synchronized (Regions)
				{
					Regions.put(region.Name,  region);
					RegionsByHandle.put(region.RegionHandle, region);
				}

				if (OnGridRegion != null)
				{
					OnGridRegion.raiseEvent((new GridRegionEventArgs(region)));
				}
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void MapItemReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnGridItems != null)
		{
			MapItemReplyPacket reply = (MapItemReplyPacket)e.getPacket();
			GridItemType type = GridItemType.get((reply.RequestData.ItemType));
			List<MapItem> items = new ArrayList<MapItem>();

			for (int i = 0; i < reply.Data.length; i++)
			{
				String name = Utils.bytesWithTrailingNullByteToString(reply.Data[i].Name);

				switch (type)
				{
				case AgentLocations:
					MapAgentLocation location = new MapAgentLocation();
					location.GlobalX = reply.Data[i].X;
					location.GlobalY = reply.Data[i].Y;
					location.Identifier = name;
					location.AvatarCount = reply.Data[i].Extra;
					items.add(location);
					break;
				case Classified:
					//FIXME:
					JLogger.error("FIXME");
					break;
				case LandForSale:
					MapLandForSale landsale = new MapLandForSale();
					landsale.GlobalX = reply.Data[i].X;
					landsale.GlobalY = reply.Data[i].Y;
					landsale.ID = reply.Data[i].ID;
					landsale.Name = name;
					landsale.Size = reply.Data[i].Extra;
					landsale.Price = reply.Data[i].Extra2;
					items.add(landsale);
					break;
				case MatureEvent:
					MapMatureEvent matureEvent = new MapMatureEvent();
					matureEvent.GlobalX = reply.Data[i].X;
					matureEvent.GlobalY = reply.Data[i].Y;
					matureEvent.Description = name;
					matureEvent.Flags = DirectoryManager.EventFlags.get(reply.Data[i].Extra2);
					items.add(matureEvent);
					break;
				case PgEvent:
					MapPGEvent PGEvent = new MapPGEvent();
					PGEvent.GlobalX = reply.Data[i].X;
					PGEvent.GlobalY = reply.Data[i].Y;
					PGEvent.Description = name;
					PGEvent.Flags = DirectoryManager.EventFlags.get(reply.Data[i].Extra2);
					items.add(PGEvent);
					break;
				case Popular:
					//FIXME:
					JLogger.error("FIXME");
					break;
				case Telehub:
					MapTelehub teleHubItem = new MapTelehub();
					teleHubItem.GlobalX = reply.Data[i].X;
					teleHubItem.GlobalY = reply.Data[i].Y;
					items.add(teleHubItem);
					break;
				case AdultLandForSale:
					MapAdultLandForSale adultLandsale = new MapAdultLandForSale();
					adultLandsale.GlobalX = reply.Data[i].X;
					adultLandsale.GlobalY = reply.Data[i].Y;
					adultLandsale.ID = reply.Data[i].ID;
					adultLandsale.Name = name;
					adultLandsale.Size = reply.Data[i].Extra;
					adultLandsale.Price = reply.Data[i].Extra2;
					items.add(adultLandsale);
					break;
				case AdultEvent:
					MapAdultEvent adultEvent = new MapAdultEvent();
					adultEvent.GlobalX = reply.Data[i].X;
					adultEvent.GlobalY = reply.Data[i].Y;
					adultEvent.Description = Utils.bytesWithTrailingNullByteToString(reply.Data[i].Name);
					adultEvent.Flags = DirectoryManager.EventFlags.get(reply.Data[i].Extra2);
					items.add(adultEvent);
					break;
				default:
					JLogger.warn("Unknown map item type " + type);
					break;
				}
			}

			OnGridItems.raiseEvent((new GridItemsEventArgs(type, items)));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void SimulatorViewerTimeMessageHandler(Object sender, final PacketReceivedEventArgs e)
	{
		SimulatorViewerTimeMessagePacket time = (SimulatorViewerTimeMessagePacket)e.getPacket();

		sunPhase = time.TimeInfo.SunPhase;
		sunDirection = time.TimeInfo.SunDirection;
		sunAngVelocity = time.TimeInfo.SunAngVelocity;
		timeOfDay = time.TimeInfo.UsecSinceStart.longValue();
		// TODO: Does anyone have a use for the time stuff?
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void CoarseLocationHandler(Object sender, final PacketReceivedEventArgs e)
	{
		CoarseLocationUpdatePacket coarse = (CoarseLocationUpdatePacket)e.getPacket();

		// populate a dictionary from the packet, for local use
		Map<UUID, Vector3> coarseEntries = new HashMap<UUID, Vector3>();
		for (int i = 0; i < coarse.AgentData.length; i++)
		{
			if(coarse.Location.length > 0)
				coarseEntries.put(coarse.AgentData[i].AgentID, new Vector3((int)coarse.Location[i].X, (int)coarse.Location[i].Y, (int)coarse.Location[i].Z * 4));

			// the friend we are tracking on radar
			if (i == coarse.Index.Prey)
				e.getSimulator().preyID = coarse.AgentData[i].AgentID;
		}

		// find stale entries (people who left the sim)
		final List<UUID> removedEntries = new ArrayList<UUID>();
		for(Entry<UUID, Vector3> entry: e.getSimulator().avatarPositions.getDictionary().entrySet())
		{
			if(!coarseEntries.containsKey(entry.getKey()))
			{
				removedEntries.add(entry.getKey());
			}
		}

		//		FindAll(delegate(UUID findID) { return !coarseEntries.ContainsKey(findID); });

		// anyone who was not listed in the previous update
		final List<UUID> newEntries = new ArrayList<UUID>();

		synchronized (e.getSimulator().avatarPositions.getDictionary())
		{
			// remove stale entries
			for(UUID trackedID :removedEntries)
				e.getSimulator().avatarPositions.remove(trackedID);

			// add or update tracked info, and record who is new
			for (Entry<UUID, Vector3> entry : coarseEntries.entrySet())
			{
				if (!e.getSimulator().avatarPositions.containsKey(entry.getKey()))
					newEntries.add(entry.getKey());

				e.getSimulator().avatarPositions.add(entry.getKey(), entry.getValue());
			}
		}

		if (OnCoarseLocationUpdate != null)
		{
			threadPool.execute(new Runnable(){
				public void run()
				{
					OnCoarseLocationUpdate.raiseEvent(
							(new CoarseLocationUpdateEventArgs(e.getSimulator(), 
									newEntries, removedEntries))); 
				}
			});
			//			ThreadPool.QueueUserWorkItem(delegate(object o)
			//					{ OnCoarseLocationUpdate(new CoarseLocationUpdateEventArgs(e.getSimulator(), newEntries, removedEntries)); });
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void RegionHandleReplyHandler(Object sender, PacketReceivedEventArgs e)
	{            
		if (OnRegionHandleReply != null)
		{
			RegionIDAndHandleReplyPacket reply = (RegionIDAndHandleReplyPacket)e.getPacket();
			OnRegionHandleReply.raiseEvent(new RegionHandleReplyEventArgs(reply.ReplyBlock.RegionID, reply.ReplyBlock.RegionHandle));
		}
	}
	//endregion
}
